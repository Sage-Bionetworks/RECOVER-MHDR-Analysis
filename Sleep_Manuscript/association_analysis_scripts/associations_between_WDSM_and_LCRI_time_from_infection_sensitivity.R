
setwd("~/sleep_paper/analyses")

source("utility_functions_for_sleep_paper.R")

## modified ShapeData function which includes the days_from_infection covariate
ShapeData <- function(dat_clinical, 
                      dat_digital, 
                      measure_name, 
                      window_size,
                      days_between_visits_thr = 30,
                      pool_na = TRUE) {
  dat_clinical$visit_dt <- lubridate::as_date(dat_clinical$visit_dt)
  dat_clinical$index_dt_curr <- lubridate::as_date(dat_clinical$index_dt_curr)
  
  mean_name <- paste0("mhp:summary:weekly:mean:", measure_name)
  numrecord_name <- paste0("mhp:summary:weekly:numrecords:", measure_name) 
  digital_measures <- c(mean_name, numrecord_name)
  
  dat_digital <- dat_digital[dat_digital$FITBIT_CONCEPT_CD %in% digital_measures,]
  dat_clinical <- dat_clinical[!is.na(dat_clinical$visit_dt),]
  ids_digital <- unique(dat_digital$record_id)
  ids_clinical <- unique(dat_clinical$record_id)
  ids <- intersect(ids_digital, ids_clinical)
  nids <- length(ids)
  
  dat <- data.frame(matrix(NA, nids, 17))
  colnames(dat) <- c("record_id", "pasc_group", "pasc_1", "pasc_2", "digital_measure", 
                     "numweeks_in_window", "numrecs_in_window",
                     "visit_date_1", "visit_date_2", "days_between_visits",
                     "window_start_date", "window_end_date",
                     "dhd_first_date", "dhd_last_date", "visit_month_difference",
                     "month", "days_from_infection")
  dat[, "record_id"] <- ids
  
  for (i in seq(nids)) {
    #cat(i, "\n")
    
    sdat_digital <- dat_digital[dat_digital$record_id == ids[i],]
    
    first_date_digital_data <- min(sdat_digital$weekly_date, na.rm = TRUE)
    
    sdat_clinical <- dat_clinical[dat_clinical$record_id == ids[i],]
    
    ## remove any clinical visits prior to the digital data collection
    sdat_clinical <- sdat_clinical[sdat_clinical$visit_dt >= first_date_digital_data,]
    
    ## sort the clinical data according to the visit dates
    sdat_clinical <- sdat_clinical[order(sdat_clinical$visit_dt),]
    
    ## get the difference in days between the consecutive visits
    days_between_visits <- diff(sdat_clinical$visit_dt)
    
    ## get visit month differences
    visit_month_diff <- diff(sdat_clinical$visit_month_curr)  
    
    
    ## find the index of the first clinical visit for which the participant
    ## does not have a NA value for PASC status
    idx1 <- which(!is.na(sdat_clinical$pasc_jama))[1]
    
    ## ignore participants that only have NA values for the PASC status
    if (!is.na(idx1)) {
      
      ## get the visit month difference for the selected pair of visits
      vmd <- visit_month_diff[idx1]
      
      ## Edge cases:
      ##
      ## vmd is NA for participants that have a single visit (since 
      ## visit_month_diff is empty in this case)
      ##
      ## vmd is NA for participants whose first visit with non NA values
      ## for PASC status corresponds to the last visit (since 
      ## length(visit_month_diff) < idx1, and idx1 will be outside)
      ##
      ## In both cases, we set visit_month_diff to 6 and we set the 
      ## days_between_visits to be the difference between the expected
      ## date of the next visit and the actual visit date.
      if (is.na(vmd)) {
        vmd <- 6
        ## date of the only valid visit
        visit_dt_1 <- sdat_clinical$visit_dt[1]
        ## expected date of the next consecutive visit
        visit_dt_2 <- sdat_clinical$index_dt_curr[1] + 30 * sdat_clinical$visit_month_curr[1] + 90
        days_between_visits <- visit_dt_2 - visit_dt_1
      }
      
      ## If the next consecutive visit is less or equal to 3 month apart 
      ## (according to clinical core criterium) we grab the data from the 
      ## next clinical visit.
      if (vmd <= 3) {
        
        ## get the index of the next visit
        idx2 <- idx1 + 1
        
        ## pasc status at first selected visit
        pasc1 <- sdat_clinical[idx1, "pasc_jama"]
        
        ## pasc status at second selected visit
        pasc2 <- sdat_clinical[idx2, "pasc_jama"]
        
        ## define PASC groupings
        dat[i, "pasc_group"] <- GetPascGroup(pasc1, pasc2, pool_na = pool_na)
        
        ## get the dates of the first and second selected visits
        visit_dt_1 <- sdat_clinical$visit_dt[idx1]
        visit_dt_2 <- sdat_clinical$visit_dt[idx2]
      }
      
      ## If the next consecutive visit is more than 3 months apart 
      ## (according to clinical core criterium) it means that the
      ## participant missed at least one visit. In this case, we 
      ## set the PASC status and visit date of the next expected 
      ## visit date.
      if (vmd > 3) {
        
        ## pasc status at first selected visit
        pasc1 <- sdat_clinical[idx1, "pasc_jama"]
        
        ## pasc status at second selected visit
        pasc2 <- NA
        
        ## define PASC groupings
        dat[i, "pasc_group"] <- GetPascGroup(pasc1, pasc2, pool_na = pool_na)
        
        ## get the dates of the first and second selected visits
        visit_dt_1 <- sdat_clinical$visit_dt[idx1]
        visit_dt_2 <- sdat_clinical$index_dt_curr[idx1] + 30 * sdat_clinical$visit_month_curr[idx1] + 90
      }
      
      dat[i, "pasc_1"] <- pasc1
      dat[i, "pasc_2"] <- pasc2
      dat[i, "visit_date_1"] <- as.character(visit_dt_1)
      dat[i, "visit_date_2"] <- as.character(visit_dt_2)
      dat[i, "days_between_visits"] <- visit_dt_2 - visit_dt_1
      dat[i, "visit_month_difference"] <- vmd
      
      ## get the date of the mid time point between these two visits
      mid_visit_dt <- visit_dt_1 + round((visit_dt_2 - visit_dt_1)/2)
      
      ## set the lower and upper dates of the digital health data window
      lower_date <- mid_visit_dt - round(window_size/2)
      upper_date <- mid_visit_dt + round(window_size/2)
      
      dat[i, "window_start_date"] <- as.character(lower_date)
      dat[i, "window_end_date"] <- as.character(upper_date)
      
      ## compute days from infection date 
      ## (difference between the window midpoint and the index_dt)
      dat[i, "days_from_infection"] <- mid_visit_dt - sdat_clinical$index_dt_curr[idx1]
      
      ## shape the digital health data 
      sdat_digital_mean <- sdat_digital[sdat_digital$FITBIT_CONCEPT_CD == mean_name,]
      sdat_digital_numrecord <- sdat_digital[sdat_digital$FITBIT_CONCEPT_CD == numrecord_name,]
      sdat <- merge(sdat_digital_mean[, c("SUMMARY_VALUE", "weekly_date")],
                    sdat_digital_numrecord[, c("SUMMARY_VALUE", "weekly_date")],
                    by = "weekly_date")
      
      ## compute the average value of the digital health for the data within the window
      aux <- AverageDigitalMeasure(sdat,
                                   lower_date,
                                   upper_date)
      dat[i, "digital_measure"] <- aux$averaged_measure
      dat[i, "numweeks_in_window"] <- aux$window_numweeks
      dat[i, "numrecs_in_window"] <- aux$window_numrecs
      dat[i, "dhd_first_date"] <- aux$first_date
      dat[i, "dhd_last_date"] <- aux$last_date
      
      ## find the month of the mid time point 
      ## (to be used for seasonality adjustment)
      dat[i, "month"] <- lubridate::month(mid_visit_dt)
    }
  }
  
  ## remove participants that don't have any digital data within the selected window
  rows_2_keep <- which(!is.na(dat$digital_measure))
  dat <- dat[rows_2_keep,]
  
  ## only keep participants whose difference between visits is greater or
  ## equal to the days_between_visits_thr
  dat <- dat[dat$days_between_visits >= days_between_visits_thr,]
  
  dat$visit_date_1 <- lubridate::as_date(dat$visit_date_1)
  dat$visit_date_2 <- lubridate::as_date(dat$visit_date_2)
  
  dat$window_start_date <- lubridate::as_date(dat$window_start_date)
  dat$window_end_date <- lubridate::as_date(dat$window_end_date)
  
  dat$dhd_first_date <- lubridate::as_date(dat$dhd_first_date)
  dat$dhd_last_date <- lubridate::as_date(dat$dhd_last_date)
  
  dat$month <- as.factor(dat$month)
  
  ## rescale the remonsetlatency measure to minutes
  if (measure_name == "remonsetlatency") {
    dat$digital_measure <- dat$digital_measure/60
  }
  
  return(dat)
}



ShapeDataSD <- function(dat_clinical, 
                        dat_digital,
                        measure_name,
                        days_between_visits_thr = 30,
                        pool_na = TRUE) {
  dat_clinical$visit_dt <- lubridate::as_date(dat_clinical$visit_dt)
  dat_clinical$index_dt_curr <- lubridate::as_date(dat_clinical$index_dt_curr)
  
  dat_clinical <- dat_clinical[!is.na(dat_clinical$visit_dt),]
  ids_digital <- unique(dat_digital$record_id)
  ids_clinical <- unique(dat_clinical$record_id)
  ids <- intersect(ids_digital, ids_clinical)
  nids <- length(ids)
  
  dat <- data.frame(matrix(NA, nids, 17))
  colnames(dat) <- c("record_id", "pasc_group", "pasc_1", "pasc_2", "digital_measure", 
                     "numweeks_in_window", "numrecs_in_window",
                     "visit_date_1", "visit_date_2", "days_between_visits",
                     "window_start_date", "window_end_date",
                     "dhd_first_date", "dhd_last_date", "visit_month_difference",
                     "month", "days_from_infection")
  dat[, "record_id"] <- ids
  
  for (i in seq(nids)) {
    #cat(i, "\n")
    
    sdat_digital <- dat_digital[dat_digital$record_id == ids[i],]
    first_date_digital_data <- lubridate::as_date(sdat_digital$period_start)
    
    sdat_clinical <- dat_clinical[dat_clinical$record_id == ids[i],]
    
    ## remove any clinical visits prior to the digital data collection
    sdat_clinical <- sdat_clinical[sdat_clinical$visit_dt >= first_date_digital_data,]
    
    ## sort the clinical data according to the visit dates
    sdat_clinical <- sdat_clinical[order(sdat_clinical$visit_dt),]
    
    ## get the difference in days between the consecutive visits
    days_between_visits <- diff(sdat_clinical$visit_dt)
    
    ## get visit month differences
    visit_month_diff <- diff(sdat_clinical$visit_month_curr)  
    
    
    ## find the index of the first clinical visit for which the participant
    ## does not have a NA value for PASC status
    idx1 <- which(!is.na(sdat_clinical$pasc_jama))[1]
    
    ## ignore participants that only have NA values for the PASC status
    if (!is.na(idx1)) {
      
      ## get the visit month difference for the selected pair of visits
      vmd <- visit_month_diff[idx1]
      
      ## Edge cases:
      ##
      ## vmd is NA for participants that have a single visit (since 
      ## visit_month_diff is empty in this case)
      ##
      ## vmd is NA for participants whose first visit with non NA values
      ## for PASC status corresponds to the last visit (since 
      ## length(visit_month_diff) < idx1, and idx1 will be outside)
      ##
      ## In both cases, we set visit_month_diff to 6 and we set the 
      ## days_between_visits to be the difference between the expected
      ## date of the next visit and the actual visit date.
      if (is.na(vmd)) {
        vmd <- 6
        ## date of the only valid visit
        visit_dt_1 <- sdat_clinical$visit_dt[1]
        ## expected date of the next consecutive visit
        visit_dt_2 <- sdat_clinical$index_dt_curr[1] + 30 * sdat_clinical$visit_month_curr[1] + 90
        days_between_visits <- visit_dt_2 - visit_dt_1
      }
      
      ## If the next consecutive visit is less or equal to 3 month apart 
      ## (according to clinical core criterium) we grab the data from the 
      ## next clinical visit.
      if (vmd <= 3) {
        
        ## get the index of the next visit
        idx2 <- idx1 + 1
        
        ## pasc status at first selected visit
        pasc1 <- sdat_clinical[idx1, "pasc_jama"]
        
        ## pasc status at second selected visit
        pasc2 <- sdat_clinical[idx2, "pasc_jama"]
        
        ## define PASC groupings
        dat[i, "pasc_group"] <- GetPascGroup(pasc1, pasc2, pool_na = pool_na)
        
        ## get the dates of the first and second selected visits
        visit_dt_1 <- sdat_clinical$visit_dt[idx1]
        visit_dt_2 <- sdat_clinical$visit_dt[idx2]
      }
      
      ## If the next consecutive visit is more than 3 months apart 
      ## (according to clinical core criterium) it means that the
      ## participant missed at least one visit. In this case, we 
      ## set the PASC status and visit date of the next expected 
      ## visit date.
      if (vmd > 3) {
        
        ## pasc status at first selected visit
        pasc1 <- sdat_clinical[idx1, "pasc_jama"]
        
        ## pasc status at second selected visit
        pasc2 <- NA
        
        ## define PASC groupings
        dat[i, "pasc_group"] <- GetPascGroup(pasc1, pasc2, pool_na = pool_na)
        
        ## get the dates of the first and second selected visits
        visit_dt_1 <- sdat_clinical$visit_dt[idx1]
        visit_dt_2 <- sdat_clinical$index_dt_curr[idx1] + 30 * sdat_clinical$visit_month_curr[idx1] + 90
      }
      
      dat[i, "pasc_1"] <- pasc1
      dat[i, "pasc_2"] <- pasc2
      dat[i, "visit_date_1"] <- as.character(visit_dt_1)
      dat[i, "visit_date_2"] <- as.character(visit_dt_2)
      dat[i, "days_between_visits"] <- visit_dt_2 - visit_dt_1
      dat[i, "visit_month_difference"] <- vmd
      
      ## get the date of the mid time point between these two visits
      mid_visit_dt <- visit_dt_1 + round((visit_dt_2 - visit_dt_1)/2)
      
      ## set the lower and upper dates of the digital health data window
      #lower_date <- mid_visit_dt - round(window_size/2)
      #upper_date <- mid_visit_dt + round(window_size/2)
      
      dat[i, "window_start_date"] <- sdat_digital$period_start
      dat[i, "window_end_date"] <- sdat_digital$period_end
      
      ## compute days from infection date 
      ## (difference between the window midpoint and the index_dt)
      dat[i, "days_from_infection"] <- mid_visit_dt - sdat_clinical$index_dt_curr[idx1]
      
      dat[i, "digital_measure"] <- sdat_digital$sd
      dat[i, "numweeks_in_window"] <- sdat_digital$count
      dat[i, "numrecs_in_window"] <- sdat_digital$count
      dat[i, "dhd_first_date"] <- sdat_digital$period_start
      dat[i, "dhd_last_date"] <- sdat_digital$period_end
      
      ## find the month of the mid time point 
      ## (to be used for seasonality adjustment)
      dat[i, "month"] <- lubridate::month(mid_visit_dt)
    }
  }
  
  ## remove participants that don't have any digital data within the selected window
  rows_2_keep <- which(!is.na(dat$digital_measure))
  dat <- dat[rows_2_keep,]
  
  ## only keep participants whose difference between visits is greater or
  ## equal to the days_between_visits_thr
  dat <- dat[dat$days_between_visits >= days_between_visits_thr,]
  
  dat$visit_date_1 <- lubridate::as_date(dat$visit_date_1)
  dat$visit_date_2 <- lubridate::as_date(dat$visit_date_2)
  
  dat$window_start_date <- lubridate::as_date(dat$window_start_date)
  dat$window_end_date <- lubridate::as_date(dat$window_end_date)
  
  dat$dhd_first_date <- lubridate::as_date(dat$dhd_first_date)
  dat$dhd_last_date <- lubridate::as_date(dat$dhd_last_date)
  
  dat$month <- as.factor(dat$month)
  
  ## rescale the SD measures to minutes
  if (measure_name == "duration_sd") {
    dat$digital_measure <- dat$digital_measure/60000
  }
  if (measure_name == "midsleep_sd") {
    dat$digital_measure <- dat$digital_measure * 60
  }
  
  return(dat)
}


visit_data_version <- "/sbgenomics/project-files/RECOVERAdult_Data_2024.12/RECOVERAdult_BiostatsDerived_20241205/visits/visits_20241206.csv"
fitbit_data_version <- "/sbgenomics/project-files/RECOVERAdult_Data_2024.12/RECOVERAdult_i2b2_fitbit.tsv"
core_data_version <- "/sbgenomics/project-files/RECOVERAdult_Data_2024.12/RECOVERAdult_BiostatsDerived_20241205/core/core_proc_20241206.csv"

## filtering results
path_mahmud <- "/sbgenomics/project-files/SANDBOX_Analysis_files/Sage_Mahmud/"
file_name_sleep_5days <- "final_matched_data_consecutive_5days_3hr_updated_V1_20250210.csv"
file_name_hrv_5days <- "final_matched_data_5days_3hr_hrv_updated_V1_20250210.csv"
file_name_spo2_5days <- "final_matched_data_5days_3hr_sp02_updated_V1_20250210.csv"


## covariates and windows files
path_elias <- "/sbgenomics/project-files/SANDBOX_Analysis_files/Sage_Elias/"
## covariates
file_name_covariates_sleep_5days <- "covariates_sleep_5days_3hr_02_11_2025.csv"
file_name_covariates_hrv_5days <- "covariates_hrv_5days_3hr_02_11_2025.csv"
file_name_covariates_spo2_5days <- "covariates_spo2_5days_3hr_02_11_2025.csv"
## windows
file_name_windows_sleep_5days <- "windows_sleep_5days_3hr_02_11_2025.csv"


cov_nms_1 <- NULL
dhd_cov_nms_1 <- c("days_from_infection")

cov_nms_2 <- c("biosex", "age_enroll", "race_unique_an")
dhd_cov_nms_2 <- c("days_from_infection")

cov_nms_3 <- c("biosex", "age_enroll", "race_unique_an", "bmi")
dhd_cov_nms_3 <- c("days_from_infection")

cov_nms_4 <- c("biosex", "age_enroll", "race_unique_an", "bmi", "alcohol", "smoking")
dhd_cov_nms_4 <- c("days_from_infection")

cov_nms_5 <- c("biosex", "age_enroll", "race_unique_an", "bmi", "alcohol", "smoking")
dhd_cov_nms_5 <- c("month", "days_from_infection")

cov_nms_6 <- c("biosex", "age_enroll", "race_unique_an", "bmi", "alcohol", "smoking")
dhd_cov_nms_6 <- c("month", "numrecs_in_window", "days_between_visits", "days_from_infection")

## load training data ids
training_ids <- read.csv("/sbgenomics/project-files/SANDBOX_Analysis_files/Sage_Solly/Training_participant_20250221.csv")[, 1]

## load vist data and update pasc definition
dat_visit <- read.csv(visit_data_version)
dat_visit$pasc_jama <- dat_visit$pasc_jama2024
dat_visit$pasc_score <- dat_visit$pasc_score_2024

## keep only data from the training subjects
dat_visit <- dat_visit[dat_visit$record_id %in% training_ids,]

dat_visit <- KeepVisitsAfterXDaysFromFirstInfection(dat_visit, thr = 182)

## get all infection dates per participant
infection_dates <- GetInfectionDates(dat_visit)

## keep only the first infection date
first_infection_dates <- infection_dates[!duplicated(infection_dates$record_id),]

## load the fitbit data
dat_fitbit <- data.table::fread(fitbit_data_version, sep = '\t', header = T)
dat_fitbit$record_id <- dat_fitbit$PARTICIPANT_ID
dat_fitbit <- as.data.frame(dat_fitbit)

## get only weekly mean and numrecords concepts
concepts <- unique(dat_fitbit$FITBIT_CONCEPT_CD)
idx_weekly <- which(unlist(lapply(strsplit(concepts, ":"), function(x) x[3])) == "weekly")
idx_mean <- which(unlist(lapply(strsplit(concepts, ":"), function(x) x[4])) == "mean")
idx_numrecords <- which(unlist(lapply(strsplit(concepts, ":"), function(x) x[4])) == "numrecords")
idx_concepts <- intersect(idx_weekly, c(idx_mean, idx_numrecords))
my_concepts <- concepts[idx_concepts]

## set of selected variables in the fitbit data
sel_variables <- c("restinghr", 
                   "hrv",
                   "spo2avg",
                   "efficiency",
                   "minsawake",
                   "minsafterwakeup",
                   "remsleepbrthrate",
                   "sleepleveldeep",
                   "remfragmentationind",
                   "remonsetlatency",
                   "minsasleep",
                   "sleeplevellight",
                   "sleeplevelrem")
sel_concepts <- c(paste0("mhp:summary:weekly:mean:", sel_variables), 
                  paste0("mhp:summary:weekly:numrecords:", sel_variables))

## keep only the data from the selected variables
dat_fitbit <- dat_fitbit[dat_fitbit$FITBIT_CONCEPT_CD %in% sel_concepts,]

## keep only records collected 6 months after the first infection date
dat_fitbit <- KeepDigitalDataCollectedXDaysFromFirstInfection(dat_fitbit,
                                                              first_infection_dates,
                                                              thr = 182)

## SUMMARY_DATE is always a Sunday (it corresponds to the start_date of
## the week of summarized data). We add 3 to move it to the mid-week date
## (a Wednesday always)
dat_fitbit$weekly_date <- lubridate::as_date(dat_fitbit$SUMMARY_DATE)
dat_fitbit$weekly_date <- dat_fitbit$weekly_date + 3

dat_core <- read.csv(core_data_version)
dat_core <- dat_core[, c("record_id", "biosex", "age_enroll", "race_unique_an")]
## keep only data from the training subjects
dat_core <- dat_core[dat_core$record_id %in% training_ids,]

idx_female <- which(dat_core$biosex == 1)
idx_na <- which(is.na(dat_core$biosex))
dat_core$biosex <- "male"
dat_core$biosex[idx_female] <- "female"
dat_core$biosex[idx_na] <- NA


#####################
## sleep variables
#####################

scale_covariates <- TRUE

sleep_variables <- c("efficiency", 
                     "minsawake",
                     "remsleepbrthrate",
                     "sleepleveldeep",
                     "remfragmentationind",
                     "remonsetlatency",
                     "minsasleep",
                     "sleeplevellight",
                     "sleeplevelrem")

## get ids from filtering
dat <- read.csv(paste0(path_mahmud, file_name_sleep_5days))
ids <- unique(dat$ParticipantIdentifier)
ids_to_keep_list <- vector(mode = "list", length = length(sleep_variables))
for (i in seq(length(sleep_variables))) {
  ids_to_keep_list[[i]] <- ids
}

## load covariate data
cov_dat <- read.csv(paste0(path_elias, file_name_covariates_sleep_5days))
dat_cov <- merge(dat_core, cov_dat, by = "record_id")
dat_cov <- CategorizeBMI(dat_cov)

m1_sleep_5days <- RunLogisticRegr(dat_clinical = dat_visit, 
                                  dat_digital = dat_fitbit,
                                  dat_covariates = dat_cov,
                                  measure_names = sleep_variables, 
                                  covariate_names = cov_nms_1,
                                  dhd_covariate_names = dhd_cov_nms_1,
                                  window_size = 182,
                                  ids_to_keep_list = ids_to_keep_list,
                                  scale_covariates = scale_covariates)

m2_sleep_5days <- RunLogisticRegr(dat_clinical = dat_visit, 
                                  dat_digital = dat_fitbit,
                                  dat_covariates = dat_cov,
                                  measure_names = sleep_variables, 
                                  covariate_names = cov_nms_2,
                                  dhd_covariate_names = dhd_cov_nms_2,
                                  window_size = 182,
                                  ids_to_keep_list = ids_to_keep_list,
                                  scale_covariates = scale_covariates)

m3_sleep_5days <- RunLogisticRegr(dat_clinical = dat_visit, 
                                  dat_digital = dat_fitbit,
                                  dat_covariates = dat_cov,
                                  measure_names = sleep_variables, 
                                  covariate_names = cov_nms_3,
                                  dhd_covariate_names = dhd_cov_nms_3,
                                  window_size = 182,
                                  ids_to_keep_list = ids_to_keep_list,
                                  scale_covariates = scale_covariates)

m6_sleep_5days <- RunLogisticRegr(dat_clinical = dat_visit, 
                                  dat_digital = dat_fitbit,
                                  dat_covariates = dat_cov,
                                  measure_names = sleep_variables, 
                                  covariate_names = cov_nms_6,
                                  dhd_covariate_names = dhd_cov_nms_6,
                                  window_size = 182,
                                  ids_to_keep_list = ids_to_keep_list,
                                  scale_covariates = scale_covariates)


dat <- read.csv("/sbgenomics/project-files/SANDBOX_Analysis_files/SAGE_Megha/Derived_Measures/Sleep_SD/sliding26weeks_stats_midsleep.csv")
wdat <- read.csv(paste0(path_elias, file_name_windows_sleep_5days))

dat_midsleep_sd <- GrabStandardDeviationData(dat, wdat)

m1_midsleep_sd_5days <- RunLogisticRegrSD(dat_clinical = dat_visit, 
                                          dat_digital = dat_midsleep_sd,
                                          dat_covariates = dat_cov,
                                          measure_name = "midsleep_sd",
                                          covariate_names = cov_nms_1,
                                          dhd_covariate_names = dhd_cov_nms_1,
                                          window_size = 182,
                                          ids_to_keep_list = ids_to_keep_list,
                                          scale_covariates = scale_covariates)

m2_midsleep_sd_5days <- RunLogisticRegrSD(dat_clinical = dat_visit, 
                                          dat_digital = dat_midsleep_sd,
                                          dat_covariates = dat_cov,
                                          measure_name = "midsleep_sd",
                                          covariate_names = cov_nms_2,
                                          dhd_covariate_names = dhd_cov_nms_2,
                                          window_size = 182,
                                          ids_to_keep_list = ids_to_keep_list,
                                          scale_covariates = scale_covariates)

m3_midsleep_sd_5days <- RunLogisticRegrSD(dat_clinical = dat_visit, 
                                          dat_digital = dat_midsleep_sd,
                                          dat_covariates = dat_cov,
                                          measure_name = "midsleep_sd",
                                          covariate_names = cov_nms_3,
                                          dhd_covariate_names = dhd_cov_nms_3,
                                          window_size = 182,
                                          ids_to_keep_list = ids_to_keep_list,
                                          scale_covariates = scale_covariates)

m6_midsleep_sd_5days <- RunLogisticRegrSD(dat_clinical = dat_visit, 
                                          dat_digital = dat_midsleep_sd,
                                          dat_covariates = dat_cov,
                                          measure_name = "midsleep_sd",
                                          covariate_names = cov_nms_6,
                                          dhd_covariate_names = dhd_cov_nms_6,
                                          window_size = 182,
                                          ids_to_keep_list = ids_to_keep_list,
                                          scale_covariates = scale_covariates)


dat <- read.csv("/sbgenomics/project-files/SANDBOX_Analysis_files/SAGE_Megha/Derived_Measures/Sleep_SD/sliding26weeks_stats_duration.csv")
wdat <- read.csv(paste0(path_elias, file_name_windows_sleep_5days))

dat_duration_sd <- GrabStandardDeviationData(dat, wdat)

m1_duration_sd_5days <- RunLogisticRegrSD(dat_clinical = dat_visit, 
                                          dat_digital = dat_duration_sd,
                                          dat_covariates = dat_cov,
                                          measure_name = "duration_sd",
                                          covariate_names = cov_nms_1,
                                          dhd_covariate_names = dhd_cov_nms_1,
                                          window_size = 182,
                                          ids_to_keep_list = ids_to_keep_list,
                                          scale_covariates = scale_covariates)

m2_duration_sd_5days <- RunLogisticRegrSD(dat_clinical = dat_visit, 
                                          dat_digital = dat_duration_sd,
                                          dat_covariates = dat_cov,
                                          measure_name = "duration_sd",
                                          covariate_names = cov_nms_2,
                                          dhd_covariate_names = dhd_cov_nms_2,
                                          window_size = 182,
                                          ids_to_keep_list = ids_to_keep_list,
                                          scale_covariates = scale_covariates)

m3_duration_sd_5days <- RunLogisticRegrSD(dat_clinical = dat_visit, 
                                          dat_digital = dat_duration_sd,
                                          dat_covariates = dat_cov,
                                          measure_name = "duration_sd",
                                          covariate_names = cov_nms_3,
                                          dhd_covariate_names = dhd_cov_nms_3,
                                          window_size = 182,
                                          ids_to_keep_list = ids_to_keep_list,
                                          scale_covariates = scale_covariates)

m6_duration_sd_5days <- RunLogisticRegrSD(dat_clinical = dat_visit, 
                                          dat_digital = dat_duration_sd,
                                          dat_covariates = dat_cov,
                                          measure_name = "duration_sd",
                                          covariate_names = cov_nms_6,
                                          dhd_covariate_names = dhd_cov_nms_6,
                                          window_size = 182,
                                          ids_to_keep_list = ids_to_keep_list,
                                          scale_covariates = scale_covariates)


##################
## hr variables
##################

hr_variables <- c("restinghr", "hrv")

dat <- read.csv(paste0(path_mahmud, file_name_hrv_5days))
ids <- unique(dat$ParticipantIdentifier)
ids_to_keep_list <- vector(mode = "list", length = length(hr_variables))
for (i in seq(length(hr_variables))) {
  ids_to_keep_list[[i]] <- ids
}

cov_dat <- read.csv(paste0(path_elias, file_name_covariates_hrv_5days))
dat_cov <- merge(dat_core, cov_dat, by = "record_id")
dat_cov <- CategorizeBMI(dat_cov)

m1_hr_5days <- RunLogisticRegr(dat_clinical = dat_visit, 
                               dat_digital = dat_fitbit,
                               dat_covariates = dat_cov,
                               measure_names = hr_variables, 
                               covariate_names = cov_nms_1,
                               dhd_covariate_names = dhd_cov_nms_1,
                               window_size = 182,
                               ids_to_keep_list = ids_to_keep_list,
                               scale_covariates = scale_covariates)

m2_hr_5days <- RunLogisticRegr(dat_clinical = dat_visit, 
                               dat_digital = dat_fitbit,
                               dat_covariates = dat_cov,
                               measure_names = hr_variables, 
                               covariate_names = cov_nms_2,
                               dhd_covariate_names = dhd_cov_nms_2,
                               window_size = 182,
                               ids_to_keep_list = ids_to_keep_list,
                               scale_covariates = scale_covariates)

m3_hr_5days <- RunLogisticRegr(dat_clinical = dat_visit, 
                               dat_digital = dat_fitbit,
                               dat_covariates = dat_cov,
                               measure_names = hr_variables, 
                               covariate_names = cov_nms_3,
                               dhd_covariate_names = dhd_cov_nms_3,
                               window_size = 182,
                               ids_to_keep_list = ids_to_keep_list,
                               scale_covariates = scale_covariates)

m6_hr_5days <- RunLogisticRegr(dat_clinical = dat_visit, 
                               dat_digital = dat_fitbit,
                               dat_covariates = dat_cov,
                               measure_names = hr_variables, 
                               covariate_names = cov_nms_6,
                               dhd_covariate_names = dhd_cov_nms_6,
                               window_size = 182,
                               ids_to_keep_list = ids_to_keep_list,
                               scale_covariates = scale_covariates)


###################
## spo2 variables
###################

spo2_variables <- c("spo2avg")

dat <- read.csv(paste0(path_mahmud, file_name_spo2_5days))
ids <- unique(dat$ParticipantIdentifier)
ids_to_keep_list <- vector(mode = "list", length = length(spo2_variables))
for (i in seq(length(spo2_variables))) {
  ids_to_keep_list[[i]] <- ids
}

cov_dat <- read.csv(paste0(path_elias, file_name_covariates_spo2_5days))
dat_cov <- merge(dat_core, cov_dat, by = "record_id")
dat_cov <- CategorizeBMI(dat_cov)

m1_spo2_5days <- RunLogisticRegr(dat_clinical = dat_visit, 
                                 dat_digital = dat_fitbit,
                                 dat_covariates = dat_cov,
                                 measure_names = spo2_variables, 
                                 covariate_names = cov_nms_1,
                                 dhd_covariate_names = dhd_cov_nms_1,
                                 window_size = 182,
                                 ids_to_keep_list = ids_to_keep_list,
                                 scale_covariates = scale_covariates)

m2_spo2_5days <- RunLogisticRegr(dat_clinical = dat_visit, 
                                 dat_digital = dat_fitbit,
                                 dat_covariates = dat_cov,
                                 measure_names = spo2_variables, 
                                 covariate_names = cov_nms_2,
                                 dhd_covariate_names = dhd_cov_nms_2,
                                 window_size = 182,
                                 ids_to_keep_list = ids_to_keep_list,
                                 scale_covariates = scale_covariates)

m3_spo2_5days <- RunLogisticRegr(dat_clinical = dat_visit, 
                                 dat_digital = dat_fitbit,
                                 dat_covariates = dat_cov,
                                 measure_names = spo2_variables, 
                                 covariate_names = cov_nms_3,
                                 dhd_covariate_names = dhd_cov_nms_3,
                                 window_size = 182,
                                 ids_to_keep_list = ids_to_keep_list,
                                 scale_covariates = scale_covariates)

m6_spo2_5days <- RunLogisticRegr(dat_clinical = dat_visit, 
                                 dat_digital = dat_fitbit,
                                 dat_covariates = dat_cov,
                                 measure_names = spo2_variables, 
                                 covariate_names = cov_nms_6,
                                 dhd_covariate_names = dhd_cov_nms_6,
                                 window_size = 182,
                                 ids_to_keep_list = ids_to_keep_list,
                                 scale_covariates = scale_covariates)


##################################################################
## organize the results and generate the figure
##################################################################

m1_5days <- rbind(m1_sleep_5days$outputs, 
                  m1_midsleep_sd_5days$outputs,
                  m1_duration_sd_5days$outputs,
                  m1_hr_5days$outputs, 
                  m1_spo2_5days$outputs)

m2_5days <- rbind(m2_sleep_5days$outputs,
                  m2_midsleep_sd_5days$outputs,
                  m2_duration_sd_5days$outputs,
                  m2_hr_5days$outputs, 
                  m2_spo2_5days$outputs)

m3_5days <- rbind(m3_sleep_5days$outputs, 
                  m3_midsleep_sd_5days$outputs,
                  m3_duration_sd_5days$outputs,
                  m3_hr_5days$outputs, 
                  m3_spo2_5days$outputs)

m6_5days <- rbind(m6_sleep_5days$outputs,
                  m6_midsleep_sd_5days$outputs,
                  m6_duration_sd_5days$outputs,
                  m6_hr_5days$outputs, 
                  m6_spo2_5days$outputs)


mt_method <- "BH"

m1_5days$apval <- p.adjust(m1_5days$test_pval, method = mt_method)

m2_5days$apval <- p.adjust(m2_5days$test_pval, method = mt_method)

m3_5days$apval <- p.adjust(m3_5days$test_pval, method = mt_method)

m6_5days$apval <- p.adjust(m6_5days$test_pval, method = mt_method)

nms <- m1_5days$digital_measure

digital_measure_pvals_5days <- vector(mode = "list", length = length(nms))
names(digital_measure_pvals_5days) <- nms
digital_measure_pvals_14days <- digital_measure_pvals_5days

for (i in seq(length(nms))) {
  aux5 <- c(m1_5days[i, "apval"],
            m2_5days[i, "apval"],
            m3_5days[i, "apval"],
            m6_5days[i, "apval"])
  names(aux5) <- paste0("Model ", c(1, 2, 3, 6))
  digital_measure_pvals_5days[[i]] <- aux5
}

ordered_variables <- c("Sleep Efficiency",
                       "Sleep Duration",
                       "Minutes in Deep Sleep", 
                       "Minutes in REM Sleep",               
                       "REM Sleep Breathing Rate",     
                       "SpO2",
                       "Heart Rate Variability",
                       "Resting Heart Rate",
                       "SD of Sleep Duration",
                       "SD of Mid-Sleep",    
                       "REM Onset Latency",             
                       "REM Fragmentation Index",                                    
                       "Minutes in Light Sleep",                
                       "WASO")


apvals1 <- unlist(lapply(digital_measure_pvals_5days, function(x) x[1]))
eff1 <- CreateEffectSizeTableLR(m_sleep = m1_sleep_5days,
                                m_midsleep_sd = m1_midsleep_sd_5days,
                                m_duration_sd = m1_duration_sd_5days,
                                m_hr = m1_hr_5days,
                                m_spo2 = m1_spo2_5days,
                                adjusted_pvals = apvals1,
                                ordered_variables = ordered_variables)

apvals2 <- unlist(lapply(digital_measure_pvals_5days, function(x) x[2]))
eff2 <- CreateEffectSizeTableLR(m_sleep = m2_sleep_5days,
                                 m_midsleep_sd = m2_midsleep_sd_5days,
                                 m_duration_sd = m2_duration_sd_5days,
                                 m_hr = m2_hr_5days,
                                 m_spo2 = m2_spo2_5days,
                                 adjusted_pvals = apvals2,
                                ordered_variables = ordered_variables)

apvals3 <- unlist(lapply(digital_measure_pvals_5days, function(x) x[3]))
eff3 <- CreateEffectSizeTableLR(m_sleep = m3_sleep_5days,
                                 m_midsleep_sd = m3_midsleep_sd_5days,
                                 m_duration_sd = m3_duration_sd_5days,
                                 m_hr = m3_hr_5days,
                                 m_spo2 = m3_spo2_5days,
                                 adjusted_pvals = apvals3,
                                ordered_variables = ordered_variables)

apvals6 <- unlist(lapply(digital_measure_pvals_5days, function(x) x[4]))
eff6 <- CreateEffectSizeTableLR(m_sleep = m6_sleep_5days,
                                 m_midsleep_sd = m6_midsleep_sd_5days,
                                 m_duration_sd = m6_duration_sd_5days,
                                 m_hr = m6_hr_5days,
                                 m_spo2 = m6_spo2_5days,
                                 adjusted_pvals = apvals6,
                                ordered_variables = ordered_variables)

eff1$model <- rep("m1", 14)
eff2$model <- rep("m2", 14)
eff3$model <- rep("m3", 14)
eff6$model <- rep("full", 14)

resall <- rbind(eff1, eff2, eff3, eff6)

library(ggplot2)

resall$iv <- factor(resall$iv, levels = ordered_variables)
resall$model <- factor(resall$model, levels = c("m1", "m2", "m3", "full"))
resall$lower <- resall[, "lower bound (95% CI)"]
resall$upper <- resall[, "upper bound (95% CI)"]

## set the positions of the tick marks on both left and right y-axes
y_left_ticks <- c(-0.8, -0.4, 0, 0.4, 0.8)
y_right_ticks <- round(exp(y_left_ticks), 2)

gg = ggplot(resall,
            aes(x = factor(model), y = Estimate)
) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  geom_text(
    aes(label = sig, 
        y = ifelse(upper > 0, upper + 0.05, lower - 0.125)), 
    size = 5
  ) +
  geom_hline(yintercept = 0, 
             linetype = "dashed", color = "red") +
  facet_wrap(~ iv, ncol=5, scales = "fixed") +
  scale_x_discrete(labels = c("m1", "m2", "m3", "full")) + 
  labs(x = paste("Outcome:", "Long COVID Research Index status"), 
       y = "Estimate (in log odds scale)" 
  ) +
  scale_y_continuous(limits = c(-0.8, 0.8),
                     name = "Estimate (log odds scale)",
                     breaks = y_left_ticks, 
                     sec.axis = sec_axis(~ exp(.), 
                                         name = "Estimate (odds scale)",
                                         breaks = y_right_ticks)
  ) +
  theme_classic()


#pdf("logistic_regression_effects_sensitivity_time_from_infection.pdf", width = 9.5, height = 7)
print(gg)
#dev.off()

