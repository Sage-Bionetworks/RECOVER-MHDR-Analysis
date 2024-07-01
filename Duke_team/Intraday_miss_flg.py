
# %%
%reload_ext autoreload
%autoreload 2

import synapseclient
import pyarrow as pa
from pyarrow import fs
import pyarrow.dataset as ds
import pyarrow.parquet as pq
import pandas as pd
import re
from collections import defaultdict
import time
import tqdm
import numpy as np
from data_utils import *
import matplotlib.pyplot as plt
import time

########
# Set up Access and download dataset
########
syn = synapseclient.Synapse()
syn.login()

ARCHIVE_VERSION = '2024-05-21'
COHORT = 'adults'
##just make sure that all of them are in one cohort as initially we are considering the dataset at cohort level
## since we have all data partitioned as ARCHIVE_VERSION/COHORT/DATASET_TYPE/PARTICIPANT_ID/part-x.parquet

########
#### Set up access and Get list of valid datasets
#### archived versions of the external parquet dataset (syn52506069)
########
## Set up Token access
sts_token = syn.get_sts_storage_token(entity='syn54128737', permission='read_only', output_format='json')

s3_external = fs.S3FileSystem(
    access_key=sts_token['accessKeyId'],
    secret_key=sts_token['secretAccessKey'],
    session_token=sts_token['sessionToken'],
    region='us-east-1'
)

## Get list of datasets in the S3 bucket
base_s3_uri_external = f"{sts_token['bucket']}/{sts_token['baseKey']}/{ARCHIVE_VERSION}/{COHORT}"
selector = fs.FileSelector(base_s3_uri_external, recursive=False)
parquet_datasets_external = s3_external.get_file_info(selector)

## Get all valid datasets
valid_paths = []
i = 0
for dataset in parquet_datasets_external:
    if re.search(r'recover-velsera-integration/main/archive/', dataset.path, re.IGNORECASE):
        i += 1
        print(f"{i}: {dataset.path}")
        valid_paths.append(dataset.path)

## Get dataset type (for eg., dataset_enrolledparticipants)
valid_paths_ext_df = pd.DataFrame(valid_paths, columns=['parquet_path_external'])
valid_paths_ext_df['datasetType'] = valid_paths_ext_df['parquet_path_external'].apply(lambda x: x.split('/')[5])

##############
## Read Fitbit Intraday in chunks.
## Each chunk contains all data for a select set of participants
##############
subset_paths_df = valid_paths_ext_df[valid_paths_ext_df['datasetType'] == 'dataset_fitbitintradaycombined'].reset_index()

participant_counts = defaultdict(int)

# get dataset path
path = subset_paths_df['parquet_path_external'][0]

# dataset at cohort level (eg. fitbitintradaycombined/adults/)
dataset_cohort = ds.dataset(path, filesystem=s3_external)

# Required list of participants 
## list of required participants; just make sure that all of them are in one cohort as initially we are considering
## the dataset at cohort level
# a few adult participants, some participant can have multiple parquet files associated with them
participant_list = ['RA11003-00024']


# %%
unique_subject_id = list(set(file_name.split('/')[6] for file_name in dataset_cohort.files))
ID_S2C5 = np.load('ID.npy', allow_pickle=True)
unique_id_S2C5_full = np.intersect1d(ID_S2C5, unique_subject_id)
step = 4
weekdays_order = ['Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday']

# %%
unique_id_S2C5 = unique_id_S2C5_full[0:100]
unique_id_S2C5 = ['RA11001-00173']
for i, subject_id in enumerate(unique_id_S2C5):
    if i%10 == 0:
        print("test" + str(subject_id) + " " + str(i))
    print("test" + str(subject_id))
    # Find all file names containing the subject ID
    matching_files = [file for file in dataset_cohort.files if subject_id in file]
    dataset = ds.dataset(matching_files, filesystem=s3_external, format='parquet')

    intra_comb = dataset.to_table().to_pandas()
    intra_hr = intra_comb[intra_comb['Type']=='activities-heart']
    del intra_comb
    intra_hr['DateTime'] = pd.to_datetime(intra_hr.loc[:, 'DateTime'])
    # intra_hr = intra_hr[(intra_hr['DateTime'] >= pd.Timestamp('2023-08-01')) & (intra_hr['DateTime'] < pd.Timestamp('2023-09-01'))]
    intra_hr = intra_hr.loc[:,['ParticipantIdentifier', 'DateTime', 'Value']]
    if len(intra_hr)<2:
        print(intra_hr['DateTime'].min(), intra_hr['DateTime'].max())
        print(str(subject_id))
        continue

    intervals = pd.date_range(intra_hr['DateTime'].min().normalize(), intra_hr['DateTime'].max().normalize() + pd.Timedelta(days=1) - pd.Timedelta(seconds=1), freq='15min')
    count_df = pd.DataFrame({
        'interval_start': intervals,
        'missflg': True
    })

    count_df = count_df.sort_values(by='interval_start').reset_index(drop=True)
    intra_hr = intra_hr.sort_values(by='DateTime').reset_index(drop=True)
    # st = time.time()
    # for i in tqdm(range(len(count_df))):        
    #     start_time = count_df.iloc[i, 0]
    #     if i < len(count_df) - 1:
    #         end_time = count_df.iloc[i + 1, 0]
    #         if intra_hr[(intra_hr['DateTime'] >= start_time) & (intra_hr['DateTime'] < end_time)].shape[0] > 1:
    #             count_df.iloc[i, 1] = False
    #     else:
    #         if intra_hr[intra_hr['DateTime'] >= start_time].shape[0] > 1:
    #             count_df.iloc[i, 1] = False
    # print(time.time()-st)

    st = time.time()
    start_times = count_df.iloc[:, 0]

    # Calculate end times (shifted start times)
    end_times = start_times.shift(-1, fill_value=intra_hr['DateTime'].max())
    # For intervals that are not the last, check if the number of rows in intra_hr between start and end times is greater than 15
    condition_non_last = (intra_hr['DateTime'][:5000].values[:, None] >= start_times.values) & (intra_hr['DateTime'][:5000].values[:, None] < end_times.values)
    conditions = np.sum(condition_non_last, axis=0) > 15

    # Update the cpunt_df_2 based on the conditions
    count_df.iloc[:, 1] = ~conditions
    print(time.time()-st)

    # Group by 15-minute intervals
    count_df['15_min_chunk'] = count_df['interval_start'].dt.time
    count_df['weekday'] = count_df['interval_start'].dt.day_name()
    # Calculate the ratio of True values for each 15-minute chunk
    miss_ratio_h = count_df.groupby('15_min_chunk')['missflg'].mean().reset_index()
    miss_ratio_week = count_df.groupby(['weekday', '15_min_chunk'])['missflg'].mean().reset_index()

    # To have a meaningful visualization with weekdays and 15-minute chunks, create a pivot table
    pivot_table = miss_ratio_week.pivot(index='15_min_chunk', columns='weekday', values='missflg')
    pivot_table = pivot_table[weekdays_order]
    array = miss_ratio_h.iloc[:,1].values

    # Save the pivot table to a CSV file
    pivot_table.to_csv('./missratio_week/' + subject_id + '.csv')

    # Save the array to a file
    np.save('./missratio_h/' + subject_id + '.npy', array)






# %%
step = 4

time_series = pivot_table.index.values
reduced_time_series = time_series[::step]

# Plot the pivot table using pcolormesh
plt.figure(figsize=(10, 6))
plt.pcolormesh(pivot_table.T, shading='auto', cmap='Blues_r')
plt.colorbar(label='Miss Ratio')

# Set the ticks for x and y axis
plt.xticks(ticks=np.arange(0, len(pivot_table.index), step), labels=reduced_time_series, rotation=90)
plt.yticks(ticks=np.arange(0, len(pivot_table.columns)), labels=pivot_table.columns)

plt.title('Missingness by 15-Minute Chunks and Weekday ' + subject_id)
plt.xlabel('15-Minute Chunks')
plt.ylabel('Weekday')
plt.show()

# %%
