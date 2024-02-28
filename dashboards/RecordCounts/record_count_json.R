##############
## todo
# SymptomLog_value_symptoms
# SymptomLog_value_treatments
# dataset_healthkitv2workouts_events
# dataset_healthkitv2heartbeat_subsamples
# dataset_healthkitv2electrocardiogram_subsamples
# dataset_fitbitsleeplogs_sleeplogdetails
# dataset_enrolledparticipants_customfields_symptoms
# dataset_enrolledparticipants_customfields_treatments
## full datasets todo
# dataset_fitbitintradaycombined
# dataset_healthkitv2samples
##############
## Required functions and parameters
source('~/recover-s3-synindex/awscli_utils.R')
source('~/recover-s3-synindex/params.R')
library(synapser)
library(tidyverse)
unzipFile <- function(file_path_in, target_path_in, file_list_return= TRUE){
  
  file_path <- c(getwd(),'/temp_aws/main/',file_path_in, sep = '') %>% paste0(collapse = '')
  
  target_path <- c(getwd(), target_path_in, file_path_in, '.csv',sep = '') %>% paste0(collapse = '')
  
  
  print(file_path)
  
  command_in <- paste0('unzip -l ',file_path,' > ',target_path)
  print(command_in)
  system(command_in)
  if(file_list_return){
    file_list <- read.csv(target_path)
    return(file_list)
  }
}

unzipFileSingle <- function(file_path_in,  file_to_extract){
  
  file_path_in <- substr(file_path_in, 1, str_length(file_path_in)-4)
  
  file_path <- c(getwd(),'/',file_path_in, sep = '') %>% paste0(collapse = '')
  
  file_path <- str_replace(file_path,'temp_folder/','temp_aws/main/')
  
  if(grepl('adults',file_path_in)){
    target_path <- 'temp_unzip_location/adults_v1/'
  }else{
    target_path <- 'temp_unzip_location/pediatric_v1/'
  }
  
  
  # print(file_path)
  # print(target_path)
  
  
  command_in <- paste0('unzip -o -j ',file_path,' ',file_to_extract, ' -d ',target_path)
  # print(command_in)
  system(command_in)
  
}
#############
# Sync S3 ingress bucket to Local EC2 (First Sync )
# using prod-creds (with 'read' only permissions) for accessing the ingress bucket 
#############
# synapser::synLogin(daemon_acc, daemon_acc_password) # login into Synapse
synapser::synLogin()

sts_token <- synapser::synGetStsStorageToken(entity = 'syn52293299', # sts enabled destination folder
                                             permission = 'read_write',  
                                             output_format = 'json')

# configure the environment with AWS token (this is the aws_profile named 'env-var')
Sys.setenv('AWS_ACCESS_KEY_ID'=sts_token$accessKeyId,
           'AWS_SECRET_ACCESS_KEY'=sts_token$secretAccessKey,
           'AWS_SESSION_TOKEN'=sts_token$sessionToken)

# # access AWS s3 from env set sts token, sts token was requested from Synapse folder above
# s3SyncToLocal(source_bucket = paste0('s3://', INGRESS_BUCKET,'/'),
#               local_destination = AWS_DOWNLOAD_LOCATION,
#               aws_profile = 'env-var')

### Now rename adults\v1 to adults_v1 and vice versa for pediatric.
### delete test_upload.rtf

################
## Get names of all files inside zips
################
all_files <- list.files(path = AWS_DOWNLOAD_LOCATION, recursive = T)
# dir.create('temp_folder')
# dir.create('temp_folder/adults_v1')
# dir.create('temp_folder/pediatric_v1')

for(i in seq(length(all_files))){
  unzipFile(all_files[i],target_path = '/temp_folder/',file_list_return = T)
  print(i)
}

all_metadata_files <- list.files(path = 'temp_folder', recursive = T)

json_files_in_zip <- lapply(all_metadata_files, function(file_path){
  print(file_path)
  # Read the entire text file
  file_path_temp <- paste0('temp_folder/', file_path)
  
  file_content <- readLines(file_path_temp)
  
  # Identify the start and end line numbers of the table
  start_line <- min(which(grepl("Length", file_content)) + 1)
  end_line <- max(which(grepl("-------", file_content)) - 1)
  
  # Extract the lines corresponding to the table
  table_lines <- file_content[start_line:end_line]
  
  # Function to parse each line
  parse_line <- function(line) {
    parts <- unlist(regmatches(line, gregexpr("\\S+", line)))
    return(data.frame(Length = as.character(parts[1]),
                      Date = as.character(parts[2]),
                      Time = parts[3],
                      Name = paste(parts[4:length(parts)], collapse = " ")))
  }
  
  # Apply the function to each line and combine into a data frame
  parsed_data <- do.call(rbind, lapply(table_lines, parse_line))
  
  # View the parsed data
  parsed_data <- parsed_data %>% 
    dplyr::mutate(file_path = file_path_temp)
  
  return(parsed_data)
}) %>% data.table::rbindlist(fill = T) %>% 
  dplyr::filter(!grepl('-----', Length)) %>% 
  dplyr::mutate(Length = as.numeric(as.character(Length)))  %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(data_type = stringr::str_split(Name, '_')[[1]][1]) %>% 
  dplyr::ungroup()

#############################
### Get row counts for each datasetType across all jsons from all zip exports
#############################
# dir.create('temp_unzip_location')

####
# EnrolledParticipants dataset record counts
####
#####
curr_file_nrows <- tryCatch(read.csv('all_nrows_enrolled.csv', stringsAsFactors = FALSE) %>% 
                        dplyr::select(-X),
                      error=function(e){
                        return(NULL)
                        })

subset_p <- json_files_in_zip %>% 
  dplyr::filter(data_type == 'EnrolledParticipants') %>% 
  dplyr::filter(!file_path %in% curr_file_nrows$file_in) %>% 
  droplevels()

unlink('temp_unzip_location/adults_v1/', recursive = T)
unlink('temp_unzip_location/pediatric_v1/', recursive = T)

if(nrow(subset_p)){
  aa <- apply(subset_p, 1, function(x){
    ## unzip the file now
    unzipFileSingle(x['file_path'],  x['Name'])
    
    ## target path
    if(grepl('adults',x['file_path'])){
      target_path <- 'temp_unzip_location/adults_v1/'
    }else{
      target_path <- 'temp_unzip_location/pediatric_v1/'
    }
    
    ## read the just unzipped file
    curr_file <- ndjson::stream_in(paste0(target_path, x['Name'])) 
    
    curr_file <- data.frame(nrows = nrow(curr_file),
                            file_in = x['file_path'],
                            name_in = x['Name'])
    
    print(x['file_path'])
    
    return(curr_file)
    
  })
  
  nrows_EnrolledParticipants <- aa %>% 
    data.table::rbindlist(fill = T) 
  
  all_nrows_enrolled <- nrows_EnrolledParticipants %>% 
    dplyr::full_join(curr_file_nrows)
  
  write.csv(all_nrows_enrolled, 'all_nrows_enrolled.csv')
  
  rm(nrows_EnrolledParticipants)
}

####
# FitbitActivityLogs dataset record counts
####
curr_file_nrows <- tryCatch(read.csv('all_nrows_fitbitactivitylogs.csv', stringsAsFactors = FALSE) %>% 
                        dplyr::select(-X) %>% 
                          dplyr::mutate(LogId = as.character(LogId)),
                      error=function(e){
                        return(NULL)
                      })

subset_p <- json_files_in_zip %>% 
  dplyr::filter(data_type == 'FitbitActivityLogs') %>% 
  dplyr::filter(!file_path %in% curr_file_nrows$file_in) %>% 
  droplevels() %>% 
  dplyr::filter(Length > 0)

unlink('temp_unzip_location/adults_v1/', recursive = T)
unlink('temp_unzip_location/pediatric_v1/', recursive = T)

if(nrow(subset_p)){
  aa <- apply(subset_p, 1, function(x){
    ## unzip the file now
    unzipFileSingle(x['file_path'],  x['Name'])
    
    ## target path
    if(grepl('adults',x['file_path'])){
      target_path <- 'temp_unzip_location/adults_v1/'
    }else{
      target_path <- 'temp_unzip_location/pediatric_v1/'
    }
    
    ## read the just unzipped file
    curr_file <- ndjson::stream_in(paste0(target_path, x['Name'])) 
    
    if(nrow(curr_file)){
      curr_file <- curr_file %>% 
        dplyr::select(LogId) %>% 
        unique() %>% 
        dplyr::mutate(file_in = x['file_path'],
                      name_in = x['Name'])
    }
    
    print(x['file_path'])
    
    return(curr_file)
    
  })
  
  nrows_FitbitActivityLogs <- aa %>% 
    data.table::rbindlist(fill = T) %>%
    unique() %>% 
    dplyr::full_join(curr_file_nrows)
  
  write.csv(nrows_FitbitActivityLogs, 'all_nrows_fitbitactivitylogs.csv')
  
  rm(nrows_FitbitActivityLogs)
}


####
# FitbitDailyData dataset record counts
####
curr_file_nrows <- tryCatch(read.csv('all_nrows_fitbitdailydata.csv', stringsAsFactors = FALSE) %>% 
                              dplyr::select(-X) ,
                            error=function(e){
                              return(NULL)
                            })

subset_p <- json_files_in_zip %>% 
  dplyr::filter(data_type == 'FitbitDailyData') %>% 
  dplyr::filter(!file_path %in% curr_file_nrows$file_in) %>% 
  droplevels() %>% 
  dplyr::filter(Length > 0)

unlink('temp_unzip_location/adults_v1/', recursive = T)
unlink('temp_unzip_location/pediatric_v1/', recursive = T)

if(nrow(subset_p)){{
  aa <- apply(subset_p, 1, function(x){
    ## unzip the file now
    unzipFileSingle(x['file_path'],  x['Name'])
    
    ## target path
    if(grepl('adults',x['file_path'])){
      target_path <- 'temp_unzip_location/adults_v1/'
    }else{
      target_path <- 'temp_unzip_location/pediatric_v1/'
    }
    
    ## read the just unzipped file
    curr_file <- ndjson::stream_in(paste0(target_path, x['Name'])) 
    
    if(nrow(curr_file)){
      curr_file <- curr_file %>% 
        dplyr::select(Date, ParticipantIdentifier) %>% 
        dplyr::mutate(file_in = x['file_path'],
                      name_in = x['Name'])
    }
    
    print(x['file_path'])
    
    return(curr_file)
    
  })
  
  nrows_FitbitDailyData <- aa %>% 
    data.table::rbindlist(fill = T) %>% 
    unique() %>% 
    dplyr::full_join(curr_file_nrows)
  
  write.csv(nrows_FitbitDailyData, 'all_nrows_fitbitdailydata.csv')
  
  rm(nrows_FitbitDailyData)
}}


####
# FitbitDevices dataset record counts
####
curr_file_nrows <- tryCatch(read.csv('all_nrows_fitbitdevices.csv', stringsAsFactors = FALSE) %>% 
                              dplyr::select(-X) ,
                            error=function(e){
                              return(NULL)
                            })

subset_p <- json_files_in_zip %>% 
  dplyr::filter(data_type == 'FitbitDevices') %>% 
  dplyr::filter(!file_path %in% curr_file_nrows$file_in) %>% 
  droplevels() %>% 
  dplyr::filter(Length > 0)

unlink('temp_unzip_location/adults_v1/', recursive = T)
unlink('temp_unzip_location/pediatric_v1/', recursive = T)

if(nrow(subset_p)){
  
  aa <- apply(subset_p, 1, function(x){
    ## unzip the file now
    unzipFileSingle(x['file_path'],  x['Name'])
    
    ## target path
    if(grepl('adults',x['file_path'])){
      target_path <- 'temp_unzip_location/adults_v1/'
    }else{
      target_path <- 'temp_unzip_location/pediatric_v1/'
    }
    
    ## read the just unzipped file
    curr_file <- ndjson::stream_in(paste0(target_path, x['Name'])) 
  
    if(nrow(curr_file)){
      curr_file <- curr_file %>% 
        dplyr::mutate(file_in = x['file_path'],
                      name_in = x['Name'])
    }
    
    
    print(x['file_path'])
    
    return(curr_file)
    
  })
  
  nrows_FitbitDevices <- aa %>% 
    data.table::rbindlist(fill = T) %>% 
    unique() %>% 
    dplyr::full_join(curr_file_nrows)
  
  write.csv(nrows_FitbitDevices, 'all_nrows_fitbitdevices.csv')
  rm(nrows_FitbitDevices)
}

####
# FitbitRestingHeartRates dataset record counts
####
curr_file_nrows <- tryCatch(read.csv('all_nrows_fitbitrestingheartrates.csv', stringsAsFactors = FALSE) %>% 
                              dplyr::select(-X) ,
                            error=function(e){
                              return(NULL)
                            })

subset_p <- json_files_in_zip %>% 
  dplyr::filter(data_type == 'FitbitRestingHeartRates') %>% 
  dplyr::filter(!file_path %in% curr_file_nrows$file_in) %>% 
  droplevels() %>% 
  dplyr::filter(Length > 0)

unlink('temp_unzip_location/adults_v1/', recursive = T)
unlink('temp_unzip_location/pediatric_v1/', recursive = T)

if(nrow(subset_p)){
  
  aa <- apply(subset_p, 1, function(x){
    ## unzip the file now
    unzipFileSingle(x['file_path'],  x['Name'])
    
    ## target path
    if(grepl('adults',x['file_path'])){
      target_path <- 'temp_unzip_location/adults_v1/'
    }else{
      target_path <- 'temp_unzip_location/pediatric_v1/'
    }
    
    ## read the just unzipped file
    curr_file <- ndjson::stream_in(paste0(target_path, x['Name'])) 

    if(nrow(curr_file)){
      curr_file <- curr_file %>% 
        dplyr::select(Date, ParticipantIdentifier) %>% 
        dplyr::mutate(file_in = x['file_path'],
                      name_in = x['Name'])
    }
    
    print(x['file_path'])
    
    return(curr_file)
    
  })
  
  nrows_FitbitRestingHeartRates <- aa %>% 
    data.table::rbindlist(fill = T) %>% 
    unique() %>% 
    dplyr::full_join(curr_file_nrows)

  write.csv(nrows_FitbitRestingHeartRates, 'all_nrows_fitbitrestingheartrates.csv')
  rm(nrows_FitbitRestingHeartRates)
}

####
# FitbitEcg dataset record counts
####
curr_file_nrows <- tryCatch(read.csv('all_nrows_fitbitecg.csv', stringsAsFactors = FALSE) %>% 
                              dplyr::select(-X) ,
                            error=function(e){
                              return(NULL)
                            })

subset_p <- json_files_in_zip %>% 
  dplyr::filter(data_type == 'FitbitEcg') %>% 
  dplyr::filter(!file_path %in% curr_file_nrows$file_in) %>% 
  droplevels() %>% 
  dplyr::filter(Length > 0)

unlink('temp_unzip_location/adults_v1/', recursive = T)
unlink('temp_unzip_location/pediatric_v1/', recursive = T)

if(nrow(subset_p)){
  
  aa <- apply(subset_p, 1, function(x){
    ## unzip the file now
    unzipFileSingle(x['file_path'],  x['Name'])
    
    ## target path
    if(grepl('adults',x['file_path'])){
      target_path <- 'temp_unzip_location/adults_v1/'
    }else{
      target_path <- 'temp_unzip_location/pediatric_v1/'
    }
    
    ## read the just unzipped file
    curr_file <- ndjson::stream_in(paste0(target_path, x['Name'])) 
    
    if(nrow(curr_file)){
      curr_file <- curr_file %>% 
        dplyr::select(FitbitEcgKey, ParticipantIdentifier) %>% 
        dplyr::mutate(file_in = x['file_path'],
                      name_in = x['Name'])
    }
    
    
    print(x['file_path'])
    
    return(curr_file)
    
  })
  
  nrows_FitbitEcg <- aa %>% 
    data.table::rbindlist(fill = T) %>% 
    unique() %>% 
    dplyr::full_join(curr_file_nrows)
  
  write.csv(nrows_FitbitEcg, 'all_nrows_fitbitecg.csv')
  rm(nrows_FitbitEcg)
}

####
# GoogleFitSamples dataset record counts
####
curr_file_nrows <- tryCatch(read.csv('all_nrows_googlefitsamples.csv', stringsAsFactors = FALSE) %>% 
                              dplyr::select(-X) ,
                            error=function(e){
                              return(NULL)
                            })

subset_p <- json_files_in_zip %>% 
  dplyr::filter(data_type == 'GoogleFitSamples') %>% 
  dplyr::filter(!file_path %in% curr_file_nrows$file_in) %>% 
  droplevels() %>% 
  dplyr::filter(Length > 0)

unlink('temp_unzip_location/adults_v1/', recursive = T)
unlink('temp_unzip_location/pediatric_v1/', recursive = T)

if(nrow(subset_p)){
  
  aa <- apply(subset_p, 1, function(x){
    ## unzip the file now
    unzipFileSingle(x['file_path'],  x['Name'])
    
    ## target path
    if(grepl('adults',x['file_path'])){
      target_path <- 'temp_unzip_location/adults_v1/'
    }else{
      target_path <- 'temp_unzip_location/pediatric_v1/'
    }
    
    ## read the just unzipped file
    curr_file <- ndjson::stream_in(paste0(target_path, x['Name'])) 
    
    curr_file <- data.frame(nrows = nrow(curr_file),
                            file_in = x['file_path'],
                            name_in = x['Name'])
    
    print(x['file_path'])
    
    return(curr_file)
    
  })
  
  nrows_GoogleFitSamples <- aa %>% 
    data.table::rbindlist(fill = T) %>% 
    dplyr::full_join(curr_file_nrows)
  
  write.csv(nrows_GoogleFitSamples, 'all_nrows_googlefitsamples.csv')
  rm(nrows_GoogleFitSamples)
}

####
# HealthKitV2ActivitySummaries dataset record counts
####
curr_file_nrows <- tryCatch(read.csv('all_nrows_healthkitv2activitysummaries.csv', stringsAsFactors = FALSE) %>% 
                              dplyr::select(-X) ,
                            error=function(e){
                              return(NULL)
                            })

subset_p <- json_files_in_zip %>% 
  dplyr::filter(data_type == 'HealthKitV2ActivitySummaries') %>% 
  dplyr::filter(!file_path %in% curr_file_nrows$file_in) %>% 
  droplevels() %>% 
  dplyr::filter(Length > 0)

unlink('temp_unzip_location/adults_v1/', recursive = T)
unlink('temp_unzip_location/pediatric_v1/', recursive = T)

if(nrow(subset_p)){
  
  aa <- apply(subset_p, 1, function(x){
    ## unzip the file now
    unzipFileSingle(x['file_path'],  x['Name'])
    
    ## target path
    if(grepl('adults',x['file_path'])){
      target_path <- 'temp_unzip_location/adults_v1/'
    }else{
      target_path <- 'temp_unzip_location/pediatric_v1/'
    }
    
    ## read the just unzipped file
    curr_file <- ndjson::stream_in(paste0(target_path, x['Name'])) 
    
    if(nrow(curr_file)){
      curr_file <- curr_file %>% 
        dplyr::select(HealthKitActivitySummaryKey, ParticipantIdentifier) %>% 
        dplyr::mutate(file_in = x['file_path'],
                      name_in = x['Name'])
    }
    
    print(x['file_path'])
    
    return(curr_file)
    
  })
  
  nrows_HealthKitV2ActivitySummaries <- aa %>% 
    data.table::rbindlist(fill = T) %>% 
    dplyr::full_join(curr_file_nrows)
  
  write.csv(nrows_HealthKitV2ActivitySummaries, 'all_nrows_healthkitv2activitysummaries.csv')
  rm(nrows_HealthKitV2ActivitySummaries)
}

####
# SymptomLog dataset record counts
####
curr_file_nrows <- tryCatch(read.csv('all_nrows_symptomlog.csv', stringsAsFactors = FALSE) %>% 
                              dplyr::select(-X) ,
                            error=function(e){
                              return(NULL)
                            })

subset_p <- json_files_in_zip %>% 
  dplyr::filter(data_type == 'SymptomLog') %>% 
  dplyr::filter(!file_path %in% curr_file_nrows$file_in) %>% 
  droplevels() %>% 
  dplyr::filter(Length > 0)

unlink('temp_unzip_location/adults_v1/', recursive = T)
unlink('temp_unzip_location/pediatric_v1/', recursive = T)

if(nrow(subset_p)){
  
  aa <- apply(subset_p, 1, function(x){
    ## unzip the file now
    unzipFileSingle(x['file_path'],  x['Name'])
    
    ## target path
    if(grepl('adults',x['file_path'])){
      target_path <- 'temp_unzip_location/adults_v1/'
    }else{
      target_path <- 'temp_unzip_location/pediatric_v1/'
    }
    
    ## read the just unzipped file
    curr_file <- ndjson::stream_in(paste0(target_path, x['Name'])) 
    
    if(nrow(curr_file)){
      curr_file <- curr_file %>% 
        dplyr::select(DataPointKey, ParticipantIdentifier) %>% 
        unique() %>% 
        na.omit() %>% 
        dplyr::mutate(file_in = x['file_path'],
                      name_in = x['Name'])
    }
    
    print(x['file_path'])
    
    return(curr_file)
    
  })
  
  nrows_SymptomLog <- aa %>% 
    data.table::rbindlist(fill = T) %>% 
    unique() %>% 
    dplyr::full_join(curr_file_nrows)

  write.csv(nrows_SymptomLog, 'all_nrows_symptomlog.csv')
  rm(nrows_SymptomLog)
}

####
# HealthKitV2Workouts dataset record counts
####
curr_file_nrows <- tryCatch(read.csv('all_nrows_healthkitv2workouts.csv', stringsAsFactors = FALSE) %>% 
                              dplyr::select(-X) ,
                            error=function(e){
                              return(NULL)
                            })

subset_p <- json_files_in_zip %>% 
  dplyr::filter(data_type == 'HealthKitV2Workouts') %>% 
  dplyr::filter(!file_path %in% curr_file_nrows$file_in) %>% 
  droplevels() %>% 
  dplyr::filter(Length > 0)

unlink('temp_unzip_location/adults_v1/', recursive = T)
unlink('temp_unzip_location/pediatric_v1/', recursive = T)

if(nrow(subset_p)){
  
  aa <- apply(subset_p, 1, function(x){
    ## unzip the file now
    unzipFileSingle(x['file_path'],  x['Name'])
    
    ## target path
    if(grepl('adults',x['file_path'])){
      target_path <- 'temp_unzip_location/adults_v1/'
    }else{
      target_path <- 'temp_unzip_location/pediatric_v1/'
    }
    
    ## read the just unzipped file
    curr_file <- ndjson::stream_in(paste0(target_path, x['Name'])) 
    
    if(nrow(curr_file)){
      curr_file <- curr_file %>% 
        dplyr::select(HealthKitWorkoutKey, ParticipantIdentifier) %>% 
        unique() %>% 
        na.omit() %>% 
        dplyr::mutate(file_in = x['file_path'],
                      name_in = x['Name'])
    }
    
    print(x['file_path'])
    
    return(curr_file)
    
  })
  
  nrows_HealthKitV2Workouts <- aa %>% 
    data.table::rbindlist(fill = T) %>% 
    unique() %>% 
    dplyr::full_join(curr_file_nrows)
  
  write.csv(nrows_HealthKitV2Workouts, 'all_nrows_healthkitv2workouts.csv')
  rm(nrows_HealthKitV2Workouts)
}

####
# HealthKitV2Electrocardiogram dataset record counts
####
curr_file_nrows <- tryCatch(read.csv('all_nrows_healthkitv2electrocardiogram.csv', stringsAsFactors = FALSE) %>% 
                              dplyr::select(-X) ,
                            error=function(e){
                              return(NULL)
                            })

subset_p <- json_files_in_zip %>% 
  dplyr::filter(data_type == 'HealthKitV2Electrocardiogram') %>% 
  dplyr::filter(!file_path %in% curr_file_nrows$file_in) %>% 
  droplevels() %>% 
  dplyr::filter(Length > 0)

unlink('temp_unzip_location/adults_v1/', recursive = T)
unlink('temp_unzip_location/pediatric_v1/', recursive = T)

if(nrow(subset_p)){
  
  aa <- apply(subset_p, 1, function(x){
    ## unzip the file now
    unzipFileSingle(x['file_path'],  x['Name'])
    
    ## target path
    if(grepl('adults',x['file_path'])){
      target_path <- 'temp_unzip_location/adults_v1/'
    }else{
      target_path <- 'temp_unzip_location/pediatric_v1/'
    }
    
    ## read the just unzipped file
    curr_file <- ndjson::stream_in(paste0(target_path, x['Name'])) 
    
    if(nrow(curr_file)){
      curr_file <- curr_file %>% 
        dplyr::select(HealthKitECGSampleKey, ParticipantIdentifier) %>% 
        unique() %>% 
        na.omit() %>% 
        dplyr::mutate(file_in = x['file_path'],
                      name_in = x['Name'])
    }
    
    print(x['file_path'])
    
    return(curr_file)
    
  })
  
  nrows_HealthKitV2Electrocardiogram <- aa %>% 
    data.table::rbindlist(fill = T) %>% 
    unique() %>% 
    dplyr::full_join(curr_file_nrows)
  
  write.csv(nrows_HealthKitV2Electrocardiogram, 'all_nrows_healthkitv2electrocardiogram.csv')
  rm(nrows_HealthKitV2Electrocardiogram)
}

####
# HealthKitV2Heartbeat dataset record counts
####
curr_file_nrows <- tryCatch(read.csv('all_nrows_healthkitv2heartbeat.csv', stringsAsFactors = FALSE) %>% 
                              dplyr::select(-X) ,
                            error=function(e){
                              return(NULL)
                            })

subset_p <- json_files_in_zip %>% 
  dplyr::filter(data_type == 'HealthKitV2Heartbeat') %>% 
  dplyr::filter(!file_path %in% curr_file_nrows$file_in) %>% 
  droplevels() %>% 
  dplyr::filter(Length > 0)

unlink('temp_unzip_location/adults_v1/', recursive = T)
unlink('temp_unzip_location/pediatric_v1/', recursive = T)

if(nrow(subset_p)){
  
  aa <- apply(subset_p, 1, function(x){
    ## unzip the file now
    unzipFileSingle(x['file_path'],  x['Name'])
    
    ## target path
    if(grepl('adults',x['file_path'])){
      target_path <- 'temp_unzip_location/adults_v1/'
    }else{
      target_path <- 'temp_unzip_location/pediatric_v1/'
    }
    
    ## read the just unzipped file
    curr_file <- ndjson::stream_in(paste0(target_path, x['Name'])) 
    
    if(nrow(curr_file)){
      curr_file <- curr_file %>% 
        dplyr::select(HealthKitHeartbeatSampleKey, ParticipantIdentifier) %>% 
        unique() %>% 
        na.omit() %>% 
        dplyr::mutate(file_in = x['file_path'],
                      name_in = x['Name'])
    }
    
    print(x['file_path'])
    
    return(curr_file)
    
  })
  
  nrows_HealthKitV2Heartbeat <- aa %>% 
    data.table::rbindlist(fill = T) %>% 
    unique() %>% 
    dplyr::full_join(curr_file_nrows)
  
  write.csv(nrows_HealthKitV2Heartbeat, 'all_nrows_healthkitv2heartbeat.csv')
  rm(nrows_HealthKitV2Heartbeat)
}

####
# HealthKitV2Statistics dataset record counts
####
curr_file_nrows <- tryCatch(data.table::fread('all_nrows_healthkitv2statistics.csv',
                                              stringsAsFactors = FALSE,
                                              header = TRUE) %>% 
                              dplyr::select(-V1) ,
                            error=function(e){
                              return(NULL)
                            })

subset_p <- json_files_in_zip %>% 
  dplyr::filter(data_type == 'HealthKitV2Statistics') %>% 
  dplyr::filter(!file_path %in% curr_file_nrows$file_in) %>% 
  droplevels() %>% 
  dplyr::filter(Length > 0)

unlink('temp_unzip_location/adults_v1/', recursive = T)
unlink('temp_unzip_location/pediatric_v1/', recursive = T)

if(nrow(subset_p)){
  aa <- apply(subset_p, 1, function(x){
    ## unzip the file now
    unzipFileSingle(x['file_path'],  x['Name'])
    
    ## target path
    if(grepl('adults',x['file_path'])){
      target_path <- 'temp_unzip_location/adults_v1/'
    }else{
      target_path <- 'temp_unzip_location/pediatric_v1/'
    }
    
    ## read the just unzipped file
    curr_file <- ndjson::stream_in(paste0(target_path, x['Name'])) 
    
    # curr_file <- data.frame(nrows = nrow(curr_file),
    #                         file_in = x['file_path'],
    #                         name_in = x['Name'])
    
    if(nrow(curr_file)){
      curr_file <- curr_file %>% 
        dplyr::select(HealthKitStatisticKey, ParticipantIdentifier) %>% 
        dplyr::mutate(file_in = x['file_path'],
                      name_in = x['Name'])
    }
    
    print(x['file_path'])
    
    return(curr_file)
    
  })
  
  nrows_HealthKitV2Statistics <- aa %>% 
    data.table::rbindlist(fill = T) %>% 
    unique() %>% 
    dplyr::full_join(curr_file_nrows)
  
  write.csv(nrows_HealthKitV2Statistics, 'all_nrows_healthkitv2statistics.csv')
  rm(nrows_HealthKitV2Statistics)
}

####
# FitbitSleepLogs dataset record counts
####
curr_file_nrows <- tryCatch(data.table::fread('all_nrows_fitbitsleeplogs.csv',
                                              stringsAsFactors = FALSE,
                                              header = TRUE) %>% 
                              dplyr::select(-V1) %>% 
                              dplyr::mutate(LogId = as.character(LogId)),
                            error=function(e){
                              return(NULL)
                            })

subset_p <- json_files_in_zip %>% 
  dplyr::filter(data_type == 'FitbitSleepLogs') %>% 
  dplyr::filter(!file_path %in% curr_file_nrows$file_in) %>% 
  droplevels() %>% 
  dplyr::filter(Length > 0)

unlink('temp_unzip_location/adults_v1/', recursive = T)
unlink('temp_unzip_location/pediatric_v1/', recursive = T)

if(nrow(subset_p)){
  
  aa <- apply(subset_p, 1, function(x){
    ## unzip the file now
    unzipFileSingle(x['file_path'],  x['Name'])
    
    ## target path
    if(grepl('adults',x['file_path'])){
      target_path <- 'temp_unzip_location/adults_v1/'
    }else{
      target_path <- 'temp_unzip_location/pediatric_v1/'
    }
    
    ## read the just unzipped file
    curr_file <- ndjson::stream_in(paste0(target_path, x['Name'])) 
    
    if(nrow(curr_file)){
      curr_file <- curr_file %>% 
        dplyr::select(LogId) %>% 
        unique() %>% 
        na.omit() %>% 
        dplyr::mutate(file_in = x['file_path'],
                      name_in = x['Name'])
    }
    
    print(x['file_path'])
    
    return(curr_file)
    
  })
  
  nrows_FitbitSleepLogs <- aa %>% 
    data.table::rbindlist(fill = T) %>% 
    unique() %>% 
    dplyr::full_join(curr_file_nrows)
  write.csv(nrows_FitbitSleepLogs, 'all_nrows_fitbitsleeplogs.csv')
  rm(nrows_FitbitSleepLogs)  
}

#####
# Get record count per dataset
#####
## plot of cumultive data in 
# plot of data coming in
# funnel plot of JSONdata -> INternal parquet(archives) -> external parquet(archives)


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

base_s3_uri <- paste0(sts_token$bucket, "/", sts_token$baseKey)
parquet_datasets <- s3_external$GetFileInfo(arrow::FileSelector$create(base_s3_uri, recursive=F))

i <- 0
valid_paths <- character()
for (dataset in parquet_datasets) {
  if (grepl('recover-processed-data/main/parquet/', dataset$path, perl = T, ignore.case = T)) {
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
  dplyr::mutate(datasetType = str_split(parquet_path,'/')[[1]][4]) %>%
  dplyr::ungroup() %>% 
  dplyr::filter(datasetType %in% c('dataset_enrolledparticipants',
                                   'dataset_fitbitactivitylogs',
                                   "dataset_fitbitdailydata",
                                   "dataset_fitbitdevices",
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

write.csv(dataset_row_count_current_internal,'all_nrows_parquet_internal.csv')

########
# Get record count for latest archive version of external parquets
# Change archive version
########
ARCHIVE_VERSION <- '2024-02-01'
sts_token <- synapser::synGetStsStorageToken(entity = 'syn52912560', # sts enabled destination folder
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
  if (grepl('recover-main-project/staging/', dataset$path, perl = T, ignore.case = T)) {
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
  dplyr::mutate(datasetType = str_split(parquet_path,'/')[[1]][4]) %>%
  dplyr::ungroup() %>% 
  dplyr::filter(datasetType %in% c('dataset_enrolledparticipants',
                                   'dataset_fitbitactivitylogs',
                                   "dataset_fitbitdailydata",
                                   "dataset_fitbitdevices",
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

write.csv(dataset_row_count_current_external,'all_nrows_parquet_external.csv')


############
## Compare all nrows and build a funnel plot
############
### get rows for each dataset in jsons

############# EnrolledParticipants

all_nrows_json <- read.csv('all_nrows.csv', stringsAsFactors = F) %>% 
  dplyr::select(-X) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(dataset_type = stringr::str_split(name_in, '_')[[1]][1]) %>% 
  dplyr::mutate(cohort = ifelse(grepl('adults',file_in),'adults','pediatric')) %>% 
  dplyr::ungroup()

all_nrows_json_enrolled <- all_nrows_json %>% 
  dplyr::filter(dataset_type == 'EnrolledParticipants') %>% 
  dplyr::group_by(cohort, dataset_type) %>% 
  dplyr::summarise(nrows = max(nrows))

############# FitbitECG

all_nrows_json <- read.csv('all_nrows_fitbitecg.csv', stringsAsFactors = F) %>% 
  dplyr::select(-X) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(dataset_type = stringr::str_split(name_in, '_')[[1]][1]) %>% 
  dplyr::mutate(cohort = ifelse(grepl('adults',file_in),'adults','pediatric')) %>% 
  dplyr::ungroup()

all_nrows_json_fitbitecg <- all_nrows_json %>% 
  dplyr::select(-file_in, -name_in) %>% 
  unique() %>% 
  dplyr::group_by(cohort, dataset_type) %>% 
  dplyr::count() %>% 
  dplyr::rename(nrows = n)

############# FitbitDailyData

all_nrows_json <- data.table::fread('all_nrows_fitbitdailydata.csv', stringsAsFactors = F) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(dataset_type = stringr::str_split(name_in, '_')[[1]][1]) %>% 
  dplyr::mutate(cohort = ifelse(grepl('adults',file_in),'adults','pediatric')) %>% 
  dplyr::ungroup()

all_nrows_json_fitbitdailydata <- all_nrows_json %>% 
  dplyr::select(-file_in, -name_in, -V1) %>% 
  unique() %>% 
  dplyr::group_by(cohort, dataset_type) %>% 
  dplyr::count() %>% 
  dplyr::rename(nrows = n)

############# FitbitRestingHeartrates

all_nrows_json <- data.table::fread('all_nrows_fitbitrestingheartrates.csv', stringsAsFactors = F) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(dataset_type = stringr::str_split(name_in, '_')[[1]][1]) %>% 
  dplyr::mutate(cohort = ifelse(grepl('adults',file_in),'adults','pediatric')) %>% 
  dplyr::ungroup()

all_nrows_json_fitbitrestingheartrates <- all_nrows_json %>% 
  dplyr::select(-file_in, -name_in, -V1) %>% 
  unique() %>% 
  dplyr::group_by(cohort, dataset_type) %>% 
  dplyr::count() %>% 
  dplyr::rename(nrows = n)

############# FitbitSleeplogs

all_nrows_json <- data.table::fread('all_nrows_fitbitsleeplogs.csv', stringsAsFactors = F) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(dataset_type = stringr::str_split(name_in, '_')[[1]][1]) %>% 
  dplyr::mutate(cohort = ifelse(grepl('adults',file_in),'adults','pediatric')) %>% 
  dplyr::ungroup()

all_nrows_json_fitbitsleeplogs <- all_nrows_json %>% 
  dplyr::select(-file_in, -name_in, -V1) %>% 
  unique() %>% 
  dplyr::group_by(cohort, dataset_type) %>% 
  dplyr::count() %>% 
  dplyr::rename(nrows = n)

############# FitbitDevices

all_nrows_json <- data.table::fread('all_nrows_fitbitdevices.csv', stringsAsFactors = F) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(dataset_type = stringr::str_split(name_in, '_')[[1]][1]) %>% 
  dplyr::mutate(cohort = ifelse(grepl('adults',file_in),'adults','pediatric')) %>% 
  dplyr::ungroup() 

all_nrows_json_fitbitdevices <- all_nrows_json %>% 
  dplyr::select(-V1, -file_in, -name_in) %>% 
  unique() %>% 
  dplyr::group_by(cohort, dataset_type) %>% 
  dplyr::count() %>% 
  dplyr::rename(nrows = n)

############# FitbitActivityLogs

all_nrows_json <- data.table::fread('all_nrows_fitbitactivitylogs.csv', stringsAsFactors = F) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(dataset_type = stringr::str_split(name_in, '_')[[1]][1]) %>% 
  dplyr::mutate(cohort = ifelse(grepl('adults',file_in),'adults','pediatric')) %>% 
  dplyr::ungroup()

all_nrows_json_fitbitactivitylogs <- all_nrows_json %>% 
  dplyr::select(-file_in, -name_in, -V1) %>% 
  unique() %>% 
  dplyr::group_by(cohort, dataset_type) %>% 
  dplyr::count() %>% 
  dplyr::rename(nrows = n)

############# HealthKitV2Heartbeat

all_nrows_json <- data.table::fread('all_nrows_healthkitv2heartbeat.csv', stringsAsFactors = F, header = T) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(dataset_type = stringr::str_split(name_in, '_')[[1]][1]) %>% 
  dplyr::mutate(deleted = ifelse(grepl('Deleted',name_in),TRUE,FALSE)) %>% 
  dplyr::mutate(cohort = ifelse(grepl('adults',file_in),'adults','pediatric')) %>% 
  dplyr::ungroup()

all_nrows_json_healthkitv2heartbeat <- all_nrows_json %>% 
  dplyr::select(-file_in, -name_in, -V1) %>% 
  unique() %>% 
  dplyr::group_by(cohort, dataset_type, deleted) %>% 
  dplyr::count() %>%
  dplyr::rename(nrows = n) %>% 
  dplyr::ungroup()

all_nrows_json_healthkitv2heartbeat_deleted <- all_nrows_json_healthkitv2heartbeat %>% 
  dplyr::filter(deleted) %>% 
  dplyr::rename(nrows_deleted = nrows)

all_nrows_json_healthkitv2heartbeat_put <- all_nrows_json_healthkitv2heartbeat %>% 
  dplyr::filter(!deleted) 

all_nrows_json_healthkitv2heartbeat <- all_nrows_json_healthkitv2heartbeat_put %>% 
  dplyr::left_join(all_nrows_json_healthkitv2heartbeat_deleted %>% 
                     dplyr::select(-deleted)) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(nrows_nett = ifelse(is.na(nrows_deleted),nrows,nrows-nrows_deleted)) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(cohort, dataset_type, nrows = nrows_nett)


############# HealthKitV2Workouts

all_nrows_json <- data.table::fread('all_nrows_healthkitv2workouts.csv', stringsAsFactors = F, header = T) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(dataset_type = stringr::str_split(name_in, '_')[[1]][1]) %>% 
  dplyr::mutate(cohort = ifelse(grepl('adults',file_in),'adults','pediatric')) %>% 
  dplyr::ungroup()

all_nrows_json_healthkitv2workouts <- all_nrows_json %>% 
  dplyr::select(-file_in, -name_in, -V1) %>% 
  unique() %>% 
  dplyr::group_by(cohort, dataset_type) %>% 
  dplyr::count() %>%
  dplyr::rename(nrows = n) %>% 
  dplyr::ungroup()


############# HealthKitV2Electrocardiogram

all_nrows_json <- data.table::fread('all_nrows_healthkitv2electrocardiogram.csv', stringsAsFactors = F, header = T) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(dataset_type = stringr::str_split(name_in, '_')[[1]][1]) %>% 
  dplyr::mutate(cohort = ifelse(grepl('adults',file_in),'adults','pediatric')) %>% 
  dplyr::ungroup()

all_nrows_json_healthkitv2electrocardiogram <- all_nrows_json %>% 
  dplyr::select(-file_in, -name_in, -V1) %>% 
  unique() %>% 
  dplyr::group_by(cohort, dataset_type) %>% 
  dplyr::count() %>%
  dplyr::rename(nrows = n) %>% 
  dplyr::ungroup()


############# HealthKitV2ActivitySummaries

all_nrows_json <- data.table::fread('all_nrows_healthkitv2activitysummaries.csv', stringsAsFactors = F, header = T) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(dataset_type = stringr::str_split(name_in, '_')[[1]][1]) %>% 
  dplyr::mutate(cohort = ifelse(grepl('adults',file_in),'adults','pediatric')) %>% 
  dplyr::ungroup()

all_nrows_json_healthkitv2activitysummaries <- all_nrows_json %>% 
  dplyr::select(-file_in, -name_in, -V1) %>% 
  unique() %>% 
  dplyr::group_by(cohort, dataset_type) %>% 
  dplyr::count() %>%
  dplyr::rename(nrows = n) %>% 
  dplyr::ungroup()


############# HealthKitV2Statistics

all_nrows_json <- data.table::fread('all_nrows_healthkitv2statistics.csv', stringsAsFactors = F, header = T) 

temp_aa <- grepl('adults',all_nrows_json$file_in)

temp_aa[temp_aa==TRUE] <- 'adults' # aa is a logical vector inititally
temp_aa[temp_aa=='FALSE'] <- 'pediatric' # after the previous change it is character now

all_nrows_json$cohort <- temp_aa

all_nrows_json <- all_nrows_json %>% 
  dplyr::mutate(dataset_type = 'HealthKitV2Statistics') %>% 
  dplyr::ungroup()

all_nrows_json_healthkitv2statistics <- all_nrows_json %>% 
  dplyr::select(-file_in, -name_in, -V1) %>% 
  unique() %>% 
  dplyr::group_by(cohort, dataset_type) %>% 
  dplyr::count() %>% 
  dplyr::rename(nrows = n)

############# GoogleFitSamples

all_nrows_json <- data.table::fread('all_nrows_googlefitsamples.csv', stringsAsFactors = F, header = T) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(dataset_type = stringr::str_split(name_in, '_')[[1]][1]) %>% 
  dplyr::mutate(cohort = ifelse(grepl('adults',file_in),'adults','pediatric')) %>% 
  dplyr::ungroup()

all_nrows_json_googlefitsamples <- all_nrows_json %>% 
  dplyr::select(-file_in, -name_in, -V1) %>% 
  unique() %>% 
  dplyr::group_by(cohort, dataset_type) %>% 
  dplyr::summarise(nrows = sum(nrows)) %>% 
  dplyr::ungroup()


############# Symptomlog

all_nrows_json <- data.table::fread('all_nrows_symptomlog.csv', stringsAsFactors = F, header = T) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(dataset_type = stringr::str_split(name_in, '_')[[1]][1]) %>% 
  dplyr::mutate(cohort = ifelse(grepl('adults',file_in),'adults','pediatric')) %>% 
  dplyr::ungroup()

all_nrows_json_symptomlog <- all_nrows_json %>% 
  dplyr::select(-file_in, -name_in, -V1) %>% 
  unique() %>% 
  dplyr::group_by(cohort, dataset_type) %>% 
  dplyr::count() %>% 
  dplyr::rename(nrows = n)


all_nrows_json_curated <- all_nrows_json_enrolled %>% 
  dplyr::full_join(all_nrows_json_fitbitactivitylogs) %>%
  dplyr::full_join(all_nrows_json_fitbitdailydata) %>%
  dplyr::full_join(all_nrows_json_fitbitdevices) %>%
  dplyr::full_join(all_nrows_json_fitbitecg) %>%
  dplyr::full_join(all_nrows_json_fitbitrestingheartrates) %>%
  dplyr::full_join(all_nrows_json_fitbitsleeplogs) %>%
  dplyr::full_join(all_nrows_json_healthkitv2activitysummaries) %>%
  dplyr::full_join(all_nrows_json_healthkitv2electrocardiogram) %>%
  dplyr::full_join(all_nrows_json_healthkitv2heartbeat) %>%
  dplyr::full_join(all_nrows_json_healthkitv2statistics) %>%
  dplyr::full_join(all_nrows_json_healthkitv2workouts) %>%
  dplyr::full_join(all_nrows_json_googlefitsamples) %>% 
  dplyr::full_join(all_nrows_json_symptomlog) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(dataset_type = tolower(dataset_type)) %>% 
  dplyr::ungroup() %>% 
  dplyr::rename(nrows_json = nrows) %>% 
  dplyr::group_by(dataset_type) %>% 
  dplyr::summarise(nrows_JSON = sum(nrows_json)) %>% 
  dplyr::ungroup()


all_nrows_current_internal <- read.csv('all_nrows_parquet_internal.csv', stringsAsFactors = F) %>% 
  dplyr::select(-X, nrows, dataset_type = datasetType, dataset_path) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(dataset_type = stringr::str_split(dataset_type,'_')[[1]][2]) %>% 
  dplyr::ungroup() %>% 
  dplyr::rename(nrows_internal = nrows)

all_nrows_current_external <- read.csv('all_nrows_parquet_external.csv', stringsAsFactors = F) %>% 
  dplyr::select(-X, nrows, dataset_type = datasetType, dataset_path) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(dataset_type = stringr::str_split(dataset_type,'_')[[1]][2]) %>% 
  dplyr::ungroup() %>% 
  dplyr::rename(nrows_external = nrows)

all_nrows_tally <- all_nrows_json_curated %>% 
  dplyr::full_join(all_nrows_current_internal %>% dplyr::select(-dataset_path)) %>% 
  dplyr::full_join(all_nrows_current_external %>% dplyr::select(-dataset_path)) %>% 
  dplyr::rename(nrows_json=nrows_JSON)


write.csv(all_nrows_tally,'all_nrows_tally.csv')

##########
# Funnel plots from all_nrows_tally
##########
library(reshape2)

## healthkit stuff
dfm <- melt(all_nrows_tally[,c('dataset_type','nrows_json','nrows_internal','nrows_external')],id.vars = 1) %>% 
  dplyr::filter(grepl('healthkit',dataset_type)) %>% 
  dplyr::group_by(dataset_type) %>% 
  dplyr::mutate(value_percent = round(value/max(value)*100,1)) %>% 
  dplyr::ungroup()

## Percent plot
ggplot(dfm,aes(x = variable,y = value_percent)) + 
  geom_bar(aes(fill = variable),stat = "identity",position = "dodge") +
  geom_text(aes(label=value_percent), vjust=0) +
  facet_wrap(~dataset_type, scales = 'free') +
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

## numbers plot
ggplot(dfm,aes(x = variable,y = value)) + 
  geom_bar(aes(fill = variable),stat = "identity",position = "dodge") +
  geom_text(aes(label=value), vjust=0) +
  facet_wrap(~dataset_type, scales = 'free') +
  # scale_y_log10()+
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

#### Fitbit stuff
dfm <- melt(all_nrows_tally[,c('dataset_type','nrows_json','nrows_internal','nrows_external')],id.vars = 1) %>% 
  dplyr::filter(grepl('fitbit',dataset_type)) %>% 
  dplyr::filter(dataset_type %in% c('fitbitactivitylogs','fitbitdevices',
                                    'fitbitsleeplogs','fitbitrestingheartrates',
                                    'fitbitdailydata')) %>%
  dplyr::group_by(dataset_type) %>% 
  dplyr::mutate(value_percent = round(value/max(value)*100,1)) %>% 
  dplyr::ungroup()

## Percent plot
ggplot(dfm,aes(x = variable,y = value_percent)) + 
  geom_bar(aes(fill = variable),stat = "identity",position = "dodge") +
  geom_text(aes(label=value_percent), vjust=0) +
  facet_wrap(~dataset_type, scales = 'free') +
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

## numbers plot
ggplot(dfm,aes(x = variable,y = value)) + 
  geom_bar(aes(fill = variable),stat = "identity",position = "dodge") +
  geom_text(aes(label=value), vjust=0) +
  facet_wrap(~dataset_type, scales = 'free') +
  # scale_y_log10()+
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

## NON healthkit, NON fitbit
dfm <- melt(all_nrows_tally[,c('dataset_type','nrows_json','nrows_internal','nrows_external')],id.vars = 1) %>% 
  dplyr::filter(!grepl('fitbit',dataset_type)) %>% 
  dplyr::filter(!grepl('healthkit',dataset_type)) %>% 
  dplyr::group_by(dataset_type) %>% 
  dplyr::mutate(value_percent = round(value/max(value)*100,1)) %>% 
  dplyr::ungroup()

## Percent plot
ggplot(dfm,aes(x = variable,y = value_percent)) + 
  geom_bar(aes(fill = variable),stat = "identity",position = "dodge") +
  geom_text(aes(label=value_percent), vjust=0) +
  facet_wrap(~dataset_type, scales = 'free') +
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

## numbers plot
ggplot(dfm,aes(x = variable,y = value)) + 
  geom_bar(aes(fill = variable),stat = "identity",position = "dodge") +
  geom_text(aes(label=value), vjust=0) +
  facet_wrap(~dataset_type, scales = 'free') +
  # scale_y_log10()+
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

#######
subset_all_enrolled <- all_enrolled %>% 
  # dplyr::filter(!is.na(`CustomFields.EOPReason`)) 
  dplyr::filter(ParticipantIdentifier == 'RA12303-00128') %>% 
  dplyr::select(ParticipantIdentifier,TimeZone, UtcOffset,EventDates.LastFitbitTrackerStepsDate) %>% 
  unique()

