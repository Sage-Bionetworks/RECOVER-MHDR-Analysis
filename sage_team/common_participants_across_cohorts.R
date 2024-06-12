############
## RECOVER Project
## Identify repeated participants/get participants common to both cohorts
## The output is a dataframe with three columns: ParticipantID, ParticipantIdentifier and GlobalKey.
## Any two participants with same GlobalKey => same participant
## Author: meghasyam@sagebase.org
############
library(tidyverse)
library(synapser)
synapser::synLogin()

############
## Get enrollment data from an External archive version
############
ARCHIVE_VERSION <- c('2024-04-23')

sts_token <- synapser::synGetStsStorageToken(entity = 'syn52506069', # sts enabled destination folder
                                             permission = 'read_only',   # request a read only token
                                             output_format = 'json')

s3_external <- arrow::S3FileSystem$create(access_key = sts_token$accessKeyId,
                                          secret_key = sts_token$secretAccessKey,
                                          session_token = sts_token$sessionToken,
                                          region="us-east-1")

base_s3_uri <- paste0(sts_token$bucket, "/", sts_token$baseKey,'/',ARCHIVE_VERSION)
parquet_datasets <- s3_external$GetFileInfo(arrow::FileSelector$create(base_s3_uri, recursive=F))

i <- 0
valid_paths <- character()
for (dataset in parquet_datasets) {
  if (grepl('recover-main-project/main/archive/', dataset$path, perl = T, ignore.case = T)) {
    i <- i+1
    cat(i)
    cat(":", dataset$path, "\n")
    valid_paths <- c(valid_paths, dataset$path)
  }
}

valid_paths_df <- valid_paths %>%
  as.data.frame() %>%
  `colnames<-`('parquet_path') %>%
  dplyr::rowwise() %>%
  dplyr::mutate(datasetType = str_split(parquet_path,'/')[[1]][5]) %>%
  dplyr::ungroup()

## Read enrollment dataset
parquet_path <- valid_paths_df %>% 
  dplyr::filter(datasetType == 'dataset_enrolledparticipants')

parquet_path <- parquet_path$parquet_path
parquet.df <- arrow::open_dataset(s3_external$path(as.character(parquet_path))) %>% dplyr::collect()
print(as.character(parquet_path))

## Participant info from enrollment dataset
participants_df <- parquet.df %>% 
  dplyr::select(ParticipantID, ParticipantIdentifier, GlobalKey) %>% 
  unique()

## ParticipantIdentifier is program code. i.e., RA XXXXX-XXXXX or RP XXXXX-XXXXX
## ParticipantID is similar to above; but from device/app
## GlobalKey is the key from CE. same GlobalKey => same person

############
## Get repeated participants
############
common_participants_key <- participants_df %>% 
  dplyr::group_by(GlobalKey) %>% 
  dplyr::count() %>% 
  dplyr::filter(n > 1)

common_participants <- participants_df %>% 
  dplyr::filter(GlobalKey %in% common_participants_key$GlobalKey) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(cohort = ifelse(substr(ParticipantIdentifier,2,2)=='A','adult','pediatric')) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(archive_version = ARCHIVE_VERSION)

write.csv(common_participants, 'common_particpants_across_cohorts.csv')
