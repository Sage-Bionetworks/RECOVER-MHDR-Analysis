
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
from constant import *

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
sts_token = syn.get_sts_storage_token(entity='syn52935439', permission='read_only', output_format='json')

s3_external = fs.S3FileSystem(
    access_key=sts_token['accessKeyId'],
    secret_key=sts_token['secretAccessKey'],
    session_token=sts_token['sessionToken'],
    region='us-east-1'
)

## Get list of datasets in the S3 bucket
base_s3_uri_external = f"{sts_token['bucket']}/{sts_token['baseKey']}/{ARCHIVE_VERSION}"
selector = fs.FileSelector(base_s3_uri_external, recursive=False)
parquet_datasets_external = s3_external.get_file_info(selector)

## Get all valid datasets
valid_paths = []
i = 0
for dataset in parquet_datasets_external:
    if re.search(r'recover-main-project/main/archive/', dataset.path, re.IGNORECASE):
        i += 1
        print(f"{i}: {dataset.path}")
        valid_paths.append(dataset.path)

## Get dataset type (for eg., dataset_enrolledparticipants)
valid_paths_ext_df = pd.DataFrame(valid_paths, columns=['parquet_path_external'])
valid_paths_ext_df['datasetType'] = valid_paths_ext_df['parquet_path_external'].apply(lambda x: x.split('/')[4])

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
participant_list = ['RA11003-00024']
unique_subject_id = list(set(file_name.split('/')[6] for file_name in dataset_cohort.files))
# %%
vars = [
    "ParticipantIdentifier", 
    "DateTime", 
    "DeepSleepSummaryBreathRate",
    "RemSleepSummaryBreathRate",
    "FullSleepSummaryBreathRate",
    "LightSleepSummaryBreathRate"
]

# Load only a subset of the columns from the dataset
dataset = ds.dataset(dataset_cohort.files[0], filesystem=s3_external)

# Convert the dataset to a pandas DataFrame and apply transformations
df = (
    dataset.to_table(columns=vars)
    .to_pandas()
    .astype({
        'DeepSleepSummaryBreathRate': 'float64',
        'RemSleepSummaryBreathRate': 'float64',
        'FullSleepSummaryBreathRate': 'float64',
        'LightSleepSummaryBreathRate': 'float64'
    })
)
# %%
