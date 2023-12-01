import pandas as pd
import re
import pyarrow.parquet as pq
from pyarrow import fs 

def get_dataset_df_str(full_path_list, dataset_name, fs):
    for p in full_path_list:
        found_name = re.findall(dataset_name, p)
        if len(found_name) > 0:
            break
    print("dataset: " + p)
    dataset = pq.ParquetDataset(p, filesystem=fs)
    df = dataset.read().to_pandas()
    print(df.columns)
    return df

def import_dataset(parquet_dir_id, syn):
    token = syn.get_sts_storage_token(parquet_dir_id, permission="read_only")

    # Connect to S3 using the token
    s3 = fs.S3FileSystem(access_key=token['accessKeyId'], 
                         secret_key = token['secretAccessKey'],
                         session_token = token['sessionToken'])

    bucket_path = token['bucket']+'/'+token['baseKey']+'/'

    # List archive folders
    archive_folders = s3.get_file_info(fs.FileSelector(bucket_path, recursive=False))
    archive_paths = [x.path for x in archive_folders if "owner" not in x.path]
    
    # List datasets in a specific archive
    dataset_list = s3.get_file_info(fs.FileSelector(archive_paths[-1], recursive=False))
    dataset_paths = [x.path for x in dataset_list if "owner" not in x.path]
    data_path_names = [item.replace('recover-main-project/main/archive/2023-09-21/', '') for item in dataset_paths]

    return dataset_paths, data_path_names, s3
    