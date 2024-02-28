## Record count day tally per release/archive version
library(tidyverse)
library(synapser)
synapser::synLogin()

setwd('./dashboards/RecordCounts/')

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

##########
# NROWS for internal and external parquets for various ARCHIVE VERSIONS
##########

all_nrows_internal_files <- list.files('.')
all_nrows_internal_files <- all_nrows_internal_files[grepl('all_nrows_parquet_internal_', all_nrows_internal_files)]

all_nrows_current_internal <- lapply(all_nrows_internal_files,function(file_path){
  read.csv(file_path, stringsAsFactors = F) %>% 
    dplyr::select(-X, nrows, dataset_type = datasetType, dataset_path) %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(dataset_type = stringr::str_split(dataset_type,'_')[[1]][2]) %>% 
    dplyr::ungroup() %>% 
    dplyr::rename(nrows_internal = nrows) %>% 
    dplyr::mutate(file_path = file_path)
}) %>% data.table::rbindlist(fill = T) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(date = stringr::str_split(file_path, '_')[[1]][5]) %>% 
  dplyr::mutate(date = stringr::str_split(date, '\\.')[[1]][1]) %>% 
  dplyr::mutate(date = lubridate::as_date(date)) %>% 
  dplyr::ungroup()
  
all_nrows_external_files <- list.files('.')
all_nrows_external_files <- all_nrows_external_files[grepl('all_nrows_parquet_external_', all_nrows_external_files)]

all_nrows_current_external <- lapply(all_nrows_external_files,function(file_path){
  read.csv(file_path, stringsAsFactors = F) %>% 
    dplyr::select(-X, nrows, dataset_type = datasetType, dataset_path) %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(dataset_type = stringr::str_split(dataset_type,'_')[[1]][2]) %>% 
    dplyr::ungroup() %>% 
    dplyr::rename(nrows_external = nrows) %>% 
    dplyr::mutate(file_path = file_path)
}) %>% data.table::rbindlist(fill = T) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(date = stringr::str_split(file_path, '_')[[1]][5]) %>% 
  dplyr::mutate(date = stringr::str_split(date, '\\.')[[1]][1]) %>% 
  dplyr::mutate(date = lubridate::as_date(date)) %>% 
  dplyr::ungroup()


# #### ROWS direct plots
# all_nrows_tally <- all_nrows_json_curated %>% 
#   dplyr::select(dataset_type, date, nrows=cs_nrows_JSON) %>% 
#   dplyr::mutate(source = 'JSONS') %>% 
#   dplyr::full_join(all_nrows_current_internal %>% 
#                      dplyr::select(-dataset_path, -file_path) %>% 
#                      dplyr::select(dataset_type, date, nrows=nrows_internal) %>% 
#                      dplyr::mutate(source = 'internal')) %>% 
#   dplyr::full_join(all_nrows_current_external %>%
#                      dplyr::select(-dataset_path, -file_path) %>% 
#                      dplyr::select(dataset_type, date, nrows=nrows_external) %>% 
#                      dplyr::mutate(source = 'external')) 
# 
# all_nrows_tally$date <- as.character(all_nrows_tally$date)
# 
# dataset_types_plot <- all_nrows_tally$dataset_type %>% unique()
# 
# fitbit_data_types <- dataset_types_plot[grepl('fitbit',dataset_types_plot)]
# 
# all_nrows_tally_temp <- all_nrows_tally %>% 
#   dplyr::filter(dataset_type %in% fitbit_data_types)
# 
# ggplot(data = all_nrows_tally_temp, aes(x = date, y = nrows)) +
#   geom_bar(aes(fill = source),stat = "identity",position = "dodge") +
#   labs(title = 'fitbit types',
#        x = "Date", y = "rows") +  facet_wrap(~dataset_type, scales = 'free') + theme_minimal() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# 
# 
# 
# healthkit_data_types <- dataset_types_plot[grepl('healthkit',dataset_types_plot)]
# 
# all_nrows_tally_temp <- all_nrows_tally %>% 
#   dplyr::filter(dataset_type %in% healthkit_data_types)
# 
# ggplot(data = all_nrows_tally_temp, aes(x = date, y = nrows)) +
#   geom_bar(aes(fill = source),stat = "identity",position = "dodge") +
#   labs(title = 'healthkit types',
#        x = "Date", y = "rows") +  facet_wrap(~dataset_type, scales = 'free') + theme_minimal() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# 
# 
# rest_data_types <- dataset_types_plot[!grepl('healthkit',dataset_types_plot)]
# rest_data_types <- rest_data_types[!grepl('fitbit',rest_data_types)]
# 
# all_nrows_tally_temp <- all_nrows_tally %>% 
#   dplyr::filter(dataset_type %in% rest_data_types)
# 
# ggplot(data = all_nrows_tally_temp, aes(x = date, y = nrows)) +
#   geom_bar(aes(fill = source),stat = "identity",position = "dodge") +
#   labs(title = 'rest types',
#        x = "Date", y = "rows") +  facet_wrap(~dataset_type, scales = 'free') + theme_minimal() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# 
# 
# #### difference plots
# 
# all_nrows_tally <- all_nrows_json_curated %>% 
#   dplyr::select(dataset_type, date, nrows_json=cs_nrows_JSON) %>% 
#   dplyr::full_join(all_nrows_current_internal %>% 
#                      dplyr::select(-dataset_path, -file_path) %>% 
#                      dplyr::select(dataset_type, date, nrows_internal)) %>% 
#   dplyr::full_join(all_nrows_current_external %>%
#                      dplyr::select(-dataset_path, -file_path) %>% 
#                      dplyr::select(dataset_type, date, nrows_external)) %>% 
#   dplyr::rowwise() %>% 
#   dplyr::mutate(valid_row = !(is.na(nrows_internal)&&is.na(nrows_external))) %>% 
#   dplyr::ungroup() %>% 
#   dplyr::filter(valid_row) %>% 
#   dplyr::filter(!is.na(nrows_json)) %>% 
#   dplyr::rowwise() %>% 
#   dplyr::mutate(nrows_external_diff = nrows_external- nrows_json) %>% 
#   dplyr::mutate(nrows_internal_diff = nrows_internal- nrows_json) %>% 
#   dplyr::mutate(nrows_external_internal_diff = nrows_external- nrows_internal) %>% 
#   dplyr::ungroup()
# 
# all_nrows_tally$date <- as.character(all_nrows_tally$date)
# 
# 
# dataset_types_plot <- all_nrows_tally$dataset_type %>% unique()
# 
# fitbit_data_types <- dataset_types_plot[grepl('fitbit',dataset_types_plot)]
# 
# all_nrows_tally_temp <- all_nrows_tally %>% 
#   dplyr::filter(dataset_type %in% fitbit_data_types)
# 
# ggplot(data = all_nrows_tally_temp, aes(x = date, y = nrows_external_internal_diff)) +
#   geom_bar(aes(fill = dataset_type),stat = "identity",position = "dodge") +
#   labs(title = 'fitbit types',
#        x = "Date", y = "rows") +  facet_wrap(~dataset_type, scales = 'free') + theme_minimal() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
# 
# 
# 
# healthkit_data_types <- dataset_types_plot[grepl('healthkit',dataset_types_plot)]
# 
# all_nrows_tally_temp <- all_nrows_tally %>% 
#   dplyr::filter(dataset_type %in% healthkit_data_types)
# 
# 
# ggplot(data = all_nrows_tally_temp, aes(x = date, y = nrows_external_internal_diff)) +
#   geom_bar(aes(fill = dataset_type),stat = "identity",position = "dodge") +
#   labs(title = 'healthkit types',
#        x = "Date", y = "rows") +  facet_wrap(~dataset_type, scales = 'free') + theme_minimal() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
# 
# 
# rest_data_types <- dataset_types_plot[!grepl('healthkit',dataset_types_plot)]
# rest_data_types <- rest_data_types[!grepl('fitbit',rest_data_types)]
# 
# all_nrows_tally_temp <- all_nrows_tally %>% 
#   dplyr::filter(dataset_type %in% rest_data_types)
# 
# ggplot(data = all_nrows_tally_temp, aes(x = date, y = nrows_external_internal_diff)) +
#   geom_bar(aes(fill = dataset_type),stat = "identity",position = "dodge") +
#   labs(title = 'healthkit types',
#        x = "Date", y = "rows") +  facet_wrap(~dataset_type, scales = 'free') + theme_minimal() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
