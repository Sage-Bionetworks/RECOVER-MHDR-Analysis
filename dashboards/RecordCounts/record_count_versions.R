## Record count day tally per release/archive version
library(tidyverse)
library(synapser)
synapser::synLogin()

setwd('~/RECOVER-MHDR-Analysis/dashboards/RecordCounts/')

############
## List of External archive versions
############
ARCHIVE_VERSIONS <- c('2023-08-02',
                      '2023-09-08',
                      '2023-09-21',
                      '2023-11-10',
                      '2023-12-06',
                      '2024-02-01')
ARCHIVE_VERSIONS <- lubridate::as_date(ARCHIVE_VERSIONS)
possible_archive_dates <- c(ARCHIVE_VERSIONS,
                            ARCHIVE_VERSIONS-1,
                            ARCHIVE_VERSIONS-2,
                            ARCHIVE_VERSIONS-3)

########
# Get record count for latest archive version of external parquets
# Change archive version
########
for(i in seq(1,length(ARCHIVE_VERSIONS))){
  ARCHIVE_VERSION <- ARCHIVE_VERSIONS[i]
  print(ARCHIVE_VERSION)
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
    dplyr::ungroup() %>%
    dplyr::filter(datasetType %in% c('dataset_enrolledparticipants',
                                     'dataset_fitbitactivitylogs',
                                     "dataset_fitbitdailydata",
                                     "dataset_fitbitdevices",
                                     # "dataset_fitbitintradaycombined",
                                     "dataset_fitbitrestingheartrates",
                                     "dataset_fitbitecg",
                                     "dataset_fitbitsleeplogs",
                                     "dataset_googlefitsamples",
                                     "dataset_healthkitv2activitysummaries",
                                     "dataset_healthkitv2electrocardiogram",
                                     "dataset_healthkitv2heartbeat",
                                     # "dataset_healthkitv2samples",
                                     "dataset_healthkitv2statistics",
                                     "dataset_healthkitv2workouts",
                                     "dataset_symptomlog"))

  ## Read each dataset, get row count
  dataset_row_count <- lapply(valid_paths_df$parquet_path, function(parquet_path){
    parquet.df <- arrow::open_dataset(s3_external$path(as.character(parquet_path))) %>% dplyr::collect()
    print(as.character(parquet_path))
    parquet.df <- data.frame(nrows = nrow(parquet.df),
                             dataset_path= parquet_path)

    return(parquet.df)

  }) %>% data.table::rbindlist(fill = T)

  dataset_row_count$dataset_path <- as.character(dataset_row_count$dataset_path)

  dataset_row_count_current_external <- dataset_row_count %>%
    dplyr::left_join(valid_paths_df %>%
                       dplyr::rename(dataset_path = parquet_path))

  file_name <- paste0('all_nrows_parquet_external_',ARCHIVE_VERSION,'.csv')

  write.csv(dataset_row_count_current_external,file_name)
}

########
# Get record count for current internal parquets
########
sts_token <- synapser::synGetStsStorageToken(entity = 'syn51406699', # sts enabled destination folder
                                             permission = 'read_only',   # request a read only token
                                             output_format = 'json')

s3_external <- arrow::S3FileSystem$create(access_key = sts_token$accessKeyId,
                                          secret_key = sts_token$secretAccessKey,
                                          session_token = sts_token$sessionToken,
                                          region="us-east-1")

base_s3_uri <- paste0(sts_token$bucket, "/", sts_token$baseKey,'/archive')
parquet_datasets <- s3_external$GetFileInfo(arrow::FileSelector$create(base_s3_uri, recursive=F))

i <- 0
valid_paths_arc <- character()
for (dataset in parquet_datasets) {
  if (grepl('recover-processed-data/main/parquet/archive', dataset$path, perl = T, ignore.case = T)) {
    i <- i+1
    cat(i)
    cat(":", dataset$path, "\n")
    valid_paths_arc <- c(valid_paths_arc, dataset$path)
  }
}

valid_paths_df_arc <- valid_paths_arc %>%
  as.data.frame() %>%
  `colnames<-`('parquet_path') %>%
  dplyr::rowwise() %>%
  dplyr::mutate(date = str_split(parquet_path,'/')[[1]][5]) %>%
  dplyr::mutate(date = paste0(str_split(date,'_')[[1]][1:3], collapse = '-')) %>%
  dplyr::mutate(date =  lubridate::as_date(date)) %>%
  ungroup() %>%
  dplyr::filter(date %in% possible_archive_dates) %>% 
  droplevels()


for(i_row in seq(nrow(valid_paths_df_arc))){
  print(as.character(i_row))
  base_s3_uri <- valid_paths_df_arc$parquet_path[i_row]
  ARCHIVE_VERSION <- as.character(valid_paths_df_arc$date[i_row])
  parquet_datasets <- s3_external$GetFileInfo(arrow::FileSelector$create(base_s3_uri, recursive=F))
  print(base_s3_uri)
  i <- 0
  valid_paths <- character()
  for (dataset in parquet_datasets) {
    if (grepl('recover-processed-data/main/parquet/archive', dataset$path, perl = T, ignore.case = T)) {
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
    dplyr::mutate(datasetType = str_split(parquet_path,'/')[[1]][6]) %>%
    dplyr::ungroup() %>%
    dplyr::filter(datasetType %in% c('dataset_enrolledparticipants',
                                     'dataset_fitbitactivitylogs',
                                     "dataset_fitbitdailydata",
                                     "dataset_fitbitdevices",
                                     # "dataset_fitbitintradaycombined",
                                     "dataset_fitbitecg",
                                     "dataset_fitbitrestingheartrates",
                                     "dataset_fitbitsleeplogs",
                                     "dataset_googlefitsamples",
                                     "dataset_healthkitv2activitysummaries",
                                     "dataset_healthkitv2electrocardiogram",
                                     "dataset_healthkitv2heartbeat",
                                     # "dataset_healthkitv2samples",
                                     "dataset_healthkitv2statistics",
                                     "dataset_healthkitv2workouts",
                                     "dataset_symptomlog"))

  # print(valid_paths_df)

  ## Read each dataset, get row count
  dataset_row_count <- lapply(valid_paths_df$parquet_path, function(parquet_path){
    parquet.df <- arrow::open_dataset(s3_external$path(as.character(parquet_path))) %>% dplyr::collect()

    parquet.df <- data.frame(nrows = nrow(parquet.df),
                             dataset_path= parquet_path)

    return(parquet.df)

  }) %>% data.table::rbindlist(fill = T)

  dataset_row_count$dataset_path <- as.character(dataset_row_count$dataset_path)

  dataset_row_count_current_internal <- dataset_row_count %>%
    dplyr::left_join(valid_paths_df %>%
                       dplyr::rename(dataset_path = parquet_path))

  file_name <- paste0('all_nrows_parquet_internal_',ARCHIVE_VERSION,'.csv')

  write.csv(dataset_row_count_current_internal,file_name)

}

