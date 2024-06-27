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
file_list = [] # initiate list of files that represent the subset of data corresponding to participant_list

# %%
# Sample data
data1 = {
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

# Create DataFrame
df1 = pd.DataFrame(data1)

# Create columns from 0 to 23 with default values of 0.0
all_columns = [str(i) for i in range(24)]
df2 = df1.set_index('date').reindex(columns=all_columns, fill_value=0.0).reset_index()
df2 = df1.set_index()
# Display the aligned DataFrame
print(df2)

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
