# %%
import synapseclient
import pyarrow as pa
from pyarrow import fs
import pyarrow.dataset as ds
import pyarrow.parquet as pq
import pandas as pd
import re
from collections import defaultdict
import time
# import duckdb
# import rpy2
# import rpy2.robjects as robjects
# %load_ext rpy2.ipython

########
# Set up Access and download dataset
########
syn = synapseclient.Synapse()
syn.login()

ARCHIVE_VERSION = '2024-06-13'
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

# %%
sage_data_lat = pd.read_csv('sage_data_lat.csv')
sage_data_rfi = pd.read_csv('sage_data_rfi.csv')
lat_id = sage_data_lat['participantidentifier'].unique()
rfi_id = sage_data_rfi['participantidentifier'].unique()

# %%
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
subset_paths_df = valid_paths_ext_df[valid_paths_ext_df['datasetType'] == 'dataset_fitbitsleeplogs_sleeplogdetails'].reset_index()

participant_counts = defaultdict(int)

# get dataset path
path = subset_paths_df['parquet_path_external'][0]

# dataset at cohort level (eg. fitbitintradaycombined/adults/)
dataset_cohort = ds.dataset(path, filesystem=s3_external)

# Required list of participants 
## list of required participants; just make sure that all of them are in one cohort as initially we are considering
## the dataset at cohort level
# a few adult participants, some participant can have multiple parquet files associated with them
participant_list = ['RA11001-00056']

df_lat = pd.DataFrame(columns=['participantidentifier', 'remonsetlatency'])
df_rfi = pd.DataFrame(columns=['participantidentifier', 'remfragmentationindex'])

for i, subject_id in enumerate(lat_id):
    if i%10 == 0:
        print("test" + str(subject_id) + " " + str(i))
    else:
        print("test" + str(subject_id))
    # Find all file names containing the subject ID
    matching_files = [file for file in dataset_cohort.files if subject_id in file]
    dataset = ds.dataset(matching_files, filesystem=s3_external, format='parquet')

    intra_slogdetails = dataset.to_table().to_pandas()
    intra_slogdetails = intra_slogdetails[intra_slogdetails['Type'] != "ShortSleepLevel"]

    # Validate REM Onset latency
    intra_slogdetails['Date'] = pd.to_datetime(intra_slogdetails['StartDate']).dt.date

    # Function to calculate the time difference between the first non-wake and first REM StartDate
    def calculate_time_difference(group):
        # Find the first non-wake StartDate
        non_wake_start = group.loc[group['Value'] != 'wake', 'StartDate'].min()
        
        # Find the first REM StartDate
        rem_start = group.loc[group['Value'] == 'rem', 'StartDate'].min()
        
        # Calculate the time difference
        if pd.notnull(non_wake_start) and pd.notnull(rem_start):
            return pd.to_datetime(rem_start) - pd.to_datetime(non_wake_start)
        else:
            return pd.NaT  # Return NaT if either is not found

    # Group by Date and calculate the time difference for each date
    time_differences = intra_slogdetails.groupby('Date').apply(calculate_time_difference).dropna()
    result = time_differences.mean().total_seconds()
    df_lat.loc[len(df_lat)] = [subject_id, result]

# %%
for i, subject_id in enumerate(rfi_id):
    if i%10 == 0:
        print("test" + str(subject_id) + " " + str(i))
    else:
        print("test" + str(subject_id))
    # Find all file names containing the subject ID
    matching_files = [file for file in dataset_cohort.files if subject_id in file]
    dataset = ds.dataset(matching_files, filesystem=s3_external, format='parquet')

    intra_slogdetails = dataset.to_table().to_pandas()
    intra_slogdetails = intra_slogdetails[intra_slogdetails['Type'] != "ShortSleepLevel"]
    rem_slogdetails = intra_slogdetails[intra_slogdetails['Value'] == 'rem']
    rem_slogdetails.sort_values(by='StartDate').reset_index(drop=True)

    # Convert StartDate and EndDate to datetime

    # Convert StartDate and EndDate to datetime if not already done
    intra_slogdetails['StartDate'] = pd.to_datetime(intra_slogdetails['StartDate'])
    intra_slogdetails['EndDate'] = pd.to_datetime(intra_slogdetails['EndDate'])

    # Extract the date from StartDate
    intra_slogdetails['Date'] = intra_slogdetails['StartDate'].dt.date

    # Calculate the duration for each row in hours
    intra_slogdetails['Duration'] = (intra_slogdetails['EndDate'] - intra_slogdetails['StartDate']).dt.total_seconds() / 3600

    # Filter the DataFrame for REM stages only
    rem_data = intra_slogdetails[intra_slogdetails['Value'] == 'rem']

    # Group by Date and calculate the total REM sleep time
    total_rem_sleep_time = rem_data.groupby('Date')['Duration'].sum()

    # Count the number of REM stages for each date
    rem_counts = rem_data.groupby('Date').size()

    # Calculate the ratio of REM stages to total REM sleep time
    rem_sleep_ratio = rem_counts / total_rem_sleep_time

    # Display the results
    rem_sleep_ratio = rem_sleep_ratio.dropna()
    df_rfi = df_rfi.append({'participantidentifier': subject_id, 'remonsetlatency': rem_sleep_ratio}, ignore_index=True)

# %%
