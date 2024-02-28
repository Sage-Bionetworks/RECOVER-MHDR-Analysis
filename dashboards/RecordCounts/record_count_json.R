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
library(synapserutils)
library(tidyverse)
unzipFile <- function(file_path_in, target_path_in, file_list_return= TRUE){
  
  file_path <- file_path_in
  
  target_path <- stringr::str_replace(file_path,'temp_aws','temp_folder')
  target_path <- paste0(target_path,'.csv')
  
  # print(file_path)
  # print(target_path)
  
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
  
  file_path <- str_replace(file_path,'temp_folder/','temp_aws/')
  
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
# Sync S3 ingress bucket to Local EC2 (using syncFromSynapse)
#############
# synapser::synLogin(daemon_acc, daemon_acc_password) # login into Synapse
synapser::synLogin()
synapserutils::syncFromSynapse('syn51714264',path = '~/RECOVER-MHDR-Analysis/dashboards/RecordCounts/temp_aws')
# Remove synapse metadata manifest files
# delete test_upload.rtf

################
## Get names of all files inside zips
################
all_files <- list.files(path = '~/RECOVER-MHDR-Analysis/dashboards/RecordCounts/temp_aws',
                        recursive = T, full.names = T)
# dir.create('~/RECOVER-MHDR-Analysis/dashboards/RecordCounts/temp_folder')
# dir.create('~/RECOVER-MHDR-Analysis/dashboards/RecordCounts/temp_folder/adults_v1')
# dir.create('~/RECOVER-MHDR-Analysis/dashboards/RecordCounts/temp_folder/pediatric_v1')


for(i in seq(length(all_files))){
  unzipFile(all_files[i],target_path = '~/RECOVER-MHDR-Analysis/dashboards/RecordCounts/temp_folder/',file_list_return = T)
  print(i)
}

all_metadata_files <- list.files(path = '~/RECOVER-MHDR-Analysis/dashboards/RecordCounts/temp_folder/', recursive = T)

json_files_in_zip <- lapply(all_metadata_files, function(file_path){
  print(file_path)
  # Read the entire text file
  file_path_temp <- paste0('~/RECOVER-MHDR-Analysis/dashboards/RecordCounts/temp_folder/', file_path)
  
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
# dir.create('~/RECOVER-MHDR-Analysis/dashboards/RecordCounts/temp_unzip_location')
setwd('~/RECOVER-MHDR-Analysis/dashboards/RecordCounts/')

# account for correcting wd
json_files_in_zip <- json_files_in_zip %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(file_path = stringr::str_replace(file_path,'~/RECOVER-MHDR-Analysis/dashboards/RecordCounts/','')) %>% 
  dplyr::ungroup()

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


## final touches
unlink('temp_unzip_location/adults_v1/', recursive = T)
unlink('temp_unzip_location/pediatric_v1/', recursive = T)

setwd('~/RECOVER-MHDR-Analysis/')
