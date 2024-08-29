
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

# %%
unique_id_S2C5 = unique_id_S2C5_full[30:50]
for subject_id in unique_id_S2C5:
    print("test" + str(subject_id))
    # Find all file names containing the subject ID
    matching_files = [file for file in dataset_cohort.files if subject_id in file]
    dataset = ds.dataset(matching_files, filesystem=s3_external, format='parquet')

    intra_comb = dataset.to_table().to_pandas()
    intra_hr = intra_comb[intra_comb['Type']=='activities-heart']
    intra_hr.loc[:,'DateTime'] = pd.to_datetime(intra_hr.loc[:, 'DateTime'])
    intra_hr = intra_hr[(intra_hr['DateTime'] >= pd.Timestamp('2023-08-01')) & (intra_hr['DateTime'] < pd.Timestamp('2023-09-01'))]
    if len(intra_hr)<2:
        print(intra_hr['DateTime'].min(), intra_hr['DateTime'].max())
        print(str(subject_id))
        continue

    del intra_comb

    count_df = cal_subject_counts(intra_hr)
    pivot_table = cal_date_hour_mx(count_df)
    pivot_table.to_csv('./tables/' + subject_id + '.csv')


# plot_date_hour_mx(pivot_table)

# %%
import os
import pandas as pd

folder_path = '/home/ec2-user/RECOVER-MHDR-Analysis/Duke_team/tables'
df_sum = pd.read_csv("/home/ec2-user/RECOVER-MHDR-Analysis/Duke_team/tables/RA11003-00024.csv")
for i, filename in enumerate(os.listdir(folder_path)):
    file_path = os.path.join(folder_path, filename)
    df = pd.read_csv(file_path)
    all_columns = [str(i) for i in range(24)]
    df2 = pd.DataFrame(df)
    df2 = df2.set_index('date').reindex(columns=all_columns, fill_value=0.0).reset_index()

    # Generate all dates from 2023-08-01 to 2023-08-31
    all_dates = pd.date_range(start="2023-08-01", end="2023-08-31").strftime('%Y-%m-%d').tolist()

    # Set index to 'date' and reindex with all dates, filling missing values with 0.0
    df2 = df2.set_index('date').reindex(all_dates, fill_value=0.0).reset_index()

    # Rename the index column back to 'date'
    df2.rename(columns={'index': 'date'}, inplace=True)
    df_sum = df_sum + df2
    print(i)



# %%
import pandas as pd
import numpy as np

# Sample data for the second pivot table
data2 = {
    'date': ['2023-08-01', '2023-08-02'],
    '0': [0.0, 247.5],
    '1': [0.0, 110.0],
    '2': [0.0, 0.0],
    '3': [0.0, 0.0],
    '4': [0.0, 1.5],
    '5': [0.0, 76.5],
    '6': [0.0, 20.0],
    '7': [0.0, 118.0],
    '8': [0.0, 150.0],
    '16': [231.5, 0.0],
    '17': [701.5, 0.0],
    '18': [686.0, 0.0],
    '19': [10.0, 0.0],
    '20': [237.5, 0.0],
    '21': [218.0, 0.0],
    '22': [225.0, 0.0],
    '23': [255.5, 0.0],
}

# Create DataFrame from sample data
df2 = pd.DataFrame(data2)

# Generate all dates from 2023-08-01 to 2023-08-31
all_dates = pd.date_range(start="2023-08-01", end="2023-08-31").strftime('%Y-%m-%d').tolist()

# Set index to 'date' and reindex with all dates, filling missing values with 0.0
df2 = df2.set_index('date').reindex(all_dates, fill_value=0.0).reset_index()

# Rename the index column back to 'date'
df2.rename(columns={'index': 'date'}, inplace=True)


# Display the aligned DataFrame
print(df2)
# %%
df2 = pd.read_csv("./tables/RA11101-00301.csv",index_col=[0])
plot_date_hour_mx(df2)
# %%
