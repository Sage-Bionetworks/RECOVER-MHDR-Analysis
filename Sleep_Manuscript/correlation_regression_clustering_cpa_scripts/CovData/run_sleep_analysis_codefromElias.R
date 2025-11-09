KeepVisitsAfterXDaysFromFirstInfection <- function(dat, thr = 180) {
  ## keep only records from infected participants
  dat <- dat[dat$infect_yn_curr == 1,]
  
  ## keep only records from visits taken place after thr days from
  ## the first infection date
  dat$visit_dt <- lubridate::as_date(dat$visit_dt)
  dat$index_dt_curr <- lubridate::as_date(dat$index_dt_curr)
  delta_time <- as.numeric(dat$visit_dt - dat$index_dt_curr)
  idx_2_keep <- which(delta_time >= thr)
  dat <- dat[idx_2_keep,]
  
  return(dat)
}


GetInfectionDates <- function(dat) {
  ## keep only visit records with infections
  dat <- dat[dat$infect_yn_curr == 1,]
  
  ids <- unique(dat$record_id)
  n_ids <- length(ids)
  
  infection_dates <- NULL
  for (i in seq(n_ids)) {
    #cat(i, "\n")
    
    ## get data from ith participant
    sdat <- dat[dat$record_id == ids[i],]
    
    ## find unique infection dates
    inf_dt <- unique(c(sdat$index_dt_curr, sdat$newinf_dt))
    
    ## sort the dates
    inf_dt <- as.character(sort(lubridate::as_date(inf_dt)))
    
    ## create data.frame with the record id and the infection dates
    n_dates <- length(inf_dt)
    aux <- data.frame(matrix(NA, n_dates, 2))
    aux[, 1] <- ids[i]
    aux[, 2] <- inf_dt
    
    ## concatenate the data.frames
    infection_dates <- rbind(infection_dates, aux)
  }
  colnames(infection_dates) <- c("record_id", "infection_date")
  
  return(infection_dates)
}


KeepDigitalDataCollectedXDaysFromFirstInfection <- function(dat,
                                                            first_infection_dates,
                                                            thr = 180) {
  first_infection_dates$infection_date <- lubridate::as_date(first_infection_dates$infection_date)
  ids <- unique(first_infection_dates$record_id)
  n_ids <- length(ids)
  dat <- dat[dat$record_id %in% ids,]
  
  for (i in seq(n_ids)) {
    cat(i, "\n")
    inf_dt <- first_infection_dates$infection_date[i]
    idx <- which(dat$record_id == ids[i] & dat$SUMMARY_DATE < inf_dt + thr)
    if (length(idx > 0)) {
      dat <- dat[-idx,]
    }
  }
  
  return(dat)
}


GetPascGroup <- function(pasc1, pasc2, pool_na = TRUE) {
  ## define PASC groupings
  grp <- paste(pasc1, pasc2, sep = "/")
  if (grp == "PASC/PASC") {
    grp <- "+/+"
  }
  if (grp == "PASC/PASC Indeterminate") {
    grp <- "+/-"
  }
  if (grp == "PASC Indeterminate/PASC") {
    grp <- "-/+"
  }
  if (grp == "PASC Indeterminate/PASC Indeterminate") {
    grp <- "-/-"
  }
  
  if (pool_na) {
    if (grp == "NA/PASC") {
      grp <- "?"
    }
    if (grp == "NA/PASC Indeterminate") {
      grp <- "?"
    }
    if (grp == "PASC/NA") {
      grp <- "?"
    }
    if (grp == "PASC Indeterminate/NA") {
      grp <- "?"
    }
    if (grp == "NA/NA") {
      grp <- "?"
    }
  }
  else {
    if (grp == "NA/PASC") {
      grp <- "?/+"
    }
    if (grp == "NA/PASC Indeterminate") {
      grp <- "?/-"
    }
    if (grp == "PASC/NA") {
      grp <- "+/?"
    }
    if (grp == "PASC Indeterminate/NA") {
      grp <- "-/?"
    }
    if (grp == "NA/NA") {
      grp <- "?/?"
    }
  }
  
  return(grp)
}


# processing measures stored in i2b2 data
## sdat must contains the weekly average values ("SUMMARY_VALUE.x")
## and the weekly number of records ("SUMMARY_VALUE.y")
AverageDigitalMeasure <- function(sdat, 
                                  lower_date, 
                                  upper_date) {
  WeightedAverage <- function(x) {
    numrecs <- as.numeric(x$SUMMARY_VALUE.y)
    values <- as.numeric(x$SUMMARY_VALUE.x)
    numrecord_weights <- numrecs/sum(numrecs, na.rm = TRUE)
    weighted_ave <- sum(values * numrecord_weights, na.rm = TRUE)
    return(weighted_ave)
  }
  averaged_measure <- NA
  window_numweeks <- NA
  window_numrecs <- NA
  first_date <- NA
  last_date <- NA
  idx <- which(sdat$weekly_date >= lower_date & sdat$weekly_date <= upper_date)
  if (length(idx) > 0) {
    averaged_measure <- WeightedAverage(x = sdat[idx, -1])
    window_numweeks <- length(idx)
    window_numrecs <- sum(as.numeric(sdat$SUMMARY_VALUE.y[idx]))
    first_date <- as.character(sdat$weekly_date[idx[1]])
    last_date <- as.character(sdat$weekly_date[idx[length(idx)]])
  }
  
  return(list(averaged_measure = averaged_measure,
              window_numweeks = window_numweeks,
              window_numrecs = window_numrecs,
              first_date = first_date,
              last_date = last_date))
}


ShapeData <- function(dat_clinical, 
                      dat_digital, 
                      measure_name, 
                      window_size,
                      days_between_visits_thr = 30) {
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
  
  dat <- data.frame(matrix(NA, nids, 16))
  colnames(dat) <- c("record_id", "pasc_group", "pasc_1", "pasc_2", "digital_measure", 
                     "numweeks_in_window", "numrecs_in_window",
                     "visit_date_1", "visit_date_2", "days_between_visits",
                     "window_start_date", "window_end_date",
                     "dhd_first_date", "dhd_last_date", "visit_month_difference",
                     "month")
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
        dat[i, "pasc_group"] <- GetPascGroup(pasc1, pasc2)
        
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
        dat[i, "pasc_group"] <- GetPascGroup(pasc1, pasc2)
        
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
  
  return(dat)
}

# processing midsleepSD, durationSD
GrabStandardDeviationData <- function(dat, wdat) {
  dat$period_start <- lubridate::as_date(dat$period_start)
  dat$period_end <- lubridate::as_date(dat$period_end)
  wdat$window_start_date <- lubridate::as_date(wdat$window_start_date)
  wdat$window_end_date <- lubridate::as_date(wdat$window_end_date)
  ids <- wdat$record_id
  nids <- length(ids)
  out <- data.frame(matrix(NA, nids, 5))
  colnames(out) <- c("record_id", "sd", "count", "period_start", "period_end")
  out[, 1] <- ids
  for (i in seq(nids)) {
    sdat <- dat[dat$ParticipantIdentifier == ids[i],]
    if (nrow(sdat) > 0) {
      idx <- which.min(abs(sdat$period_start - wdat$window_start_date[i]))
      out[i, "sd"] <- sdat[idx, "sd"]
      out[i, "count"] <- sdat[idx, "count"]
      out[i, "period_start"] <- as.character(sdat[idx, "period_start"])
      out[i, "period_end"] <- as.character(sdat[idx, "period_end"])
    }
  }
  idx_keep <- which(!is.na(out$sd))
  out <- out[idx_keep,]
  
  return(out)
}


ShapeDataSD <- function(dat_clinical, 
                        dat_digital, 
                        days_between_visits_thr = 30) {
  dat_clinical$visit_dt <- lubridate::as_date(dat_clinical$visit_dt)
  dat_clinical$index_dt_curr <- lubridate::as_date(dat_clinical$index_dt_curr)
  
  dat_clinical <- dat_clinical[!is.na(dat_clinical$visit_dt),]
  ids_digital <- unique(dat_digital$record_id)
  ids_clinical <- unique(dat_clinical$record_id)
  ids <- intersect(ids_digital, ids_clinical)
  nids <- length(ids)
  
  dat <- data.frame(matrix(NA, nids, 16))
  colnames(dat) <- c("record_id", "pasc_group", "pasc_1", "pasc_2", "digital_measure", 
                     "numweeks_in_window", "numrecs_in_window",
                     "visit_date_1", "visit_date_2", "days_between_visits",
                     "window_start_date", "window_end_date",
                     "dhd_first_date", "dhd_last_date", "visit_month_difference",
                     "month")
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
        dat[i, "pasc_group"] <- GetPascGroup(pasc1, pasc2)
        
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
        dat[i, "pasc_group"] <- GetPascGroup(pasc1, pasc2)
        
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
  
  return(dat)
}


###################################################
###################################################
###################################################
library(tidyverse)
setwd("~/SleepPaper/CovData")
## load initial training data ids
training_ids1 <- read.table("training_set_participant_ids_05312024.txt", sep = ',', header = TRUE)
training_ids1 <- training_ids1[, 1]
length(training_ids1)

## load second set of training data ids
training_ids2 <- read.csv("/sbgenomics/project-files/SANDBOX_Analysis_files/SAGE_Megha/data_split/training_set_participant_ids_10_11_2024.csv")
training_ids2 <- training_ids2[, 1]
length(training_ids2)

intersect(training_ids1, training_ids2)

## load third set of training data ids
training_ids3 <- read.csv("/sbgenomics/project-files/SANDBOX_Analysis_files/SAGE_Megha/data_split/training_set_participant_ids_21_01_2025.csv")
training_ids3 <- training_ids3[, 1]
length(training_ids3)

intersect(training_ids1, training_ids3)
intersect(training_ids2, training_ids3)

## get all training ids
training_ids <- c(training_ids1, training_ids2, training_ids3)
length(training_ids)


## make sure to read in correct versions of fitbit/visits/window/sliding26weeks data
fitbit_path = "/sbgenomics/project-files/RECOVERAdult_Data_2024.12/RECOVERAdult_i2b2_fitbit.tsv"
visit_path = "/sbgenomics/project-files/RECOVERAdult_Data_2024.12/RECOVERAdult_BiostatsDerived_20241205/visits/visits_20241206.csv"

wdat_5d <- read.csv("/sbgenomics/project-files/SANDBOX_Analysis_files/Sage_Elias/windows_sleep_5days_3hr_02_11_2025.csv")
wdat_14d <- read.csv("/sbgenomics/project-files/SANDBOX_Analysis_files/Sage_Elias/windows_sleep_14days_3hr_02_11_2025.csv")

dat_midsleep = read.csv("/sbgenomics/project-files/SANDBOX_Analysis_files/SAGE_Megha/Derived_Measures/Sleep_SD/sliding26weeks_stats_midsleep.csv")
dat_duration = read.csv("/sbgenomics/project-files/SANDBOX_Analysis_files/SAGE_Megha/Derived_Measures/Sleep_SD/sliding26weeks_stats_duration.csv")

## read in visits data
dat_visit <- read.csv(visit_path)
dat_visit$pasc_jama = dat_visit$pasc_jama2024
dat_visit$pasc_score = dat_visit$pasc_score_2024
dim(dat_visit)
## keep only data from the training subjects
dat_visit <- dat_visit[dat_visit$record_id %in% training_ids,]
dim(dat_visit)
## keep only records collected 6 months after the first infection date
dat_visit <- KeepVisitsAfterXDaysFromFirstInfection(dat_visit, thr = 182)
dim(dat_visit)


## get all infection dates per participant
infection_dates <- GetInfectionDates(dat_visit)
dim(infection_dates)

## keep only the first infection date
first_infection_dates <- infection_dates[!duplicated(infection_dates$record_id),]
dim(first_infection_dates)
length(unique(first_infection_dates$record_id))


## load the fitbit data
dat_fitbit <- data.table::fread(fitbit_path,sep = '\t', header = T)
dat_fitbit$record_id <- dat_fitbit$PARTICIPANT_ID

dat_fitbit <- as.data.frame(dat_fitbit)

dim(dat_fitbit)

concepts <- unique(dat_fitbit$FITBIT_CONCEPT_CD)
idx_weekly <- which(unlist(lapply(strsplit(concepts, ":"), function(x) x[3])) == "weekly")
idx_mean <- which(unlist(lapply(strsplit(concepts, ":"), function(x) x[4])) == "mean")
idx_numrecords <- which(unlist(lapply(strsplit(concepts, ":"), function(x) x[4])) == "numrecords")
idx_concepts <- intersect(idx_weekly, c(idx_mean, idx_numrecords))
my_concepts <- concepts[idx_concepts]
my_concepts


#sel_variables <- c("restinghr", 
#                   "hrv",
#                   "spo2avg",
#                   "efficiency",
#                   "minsawake",
#                   "minsafterwakeup",
#                   "remsleepbrthrate",
#                   "sleepleveldeep")

sel_variables <- c("efficiency", "minsasleep", "minsawake", 
                   "sleepleveldeep", "sleeplevellight","sleeplevelrem",
                   "remfragmentationind", "remonsetlatency",
                   "remsleepbrthrate", "restinghr",
                   "hrv","spo2avg")

sel_concepts <- c(paste0("mhp:summary:weekly:mean:", sel_variables), 
                  paste0("mhp:summary:weekly:numrecords:", sel_variables))




## keep only the data from the selected variables
dat_fitbit <- dat_fitbit[dat_fitbit$FITBIT_CONCEPT_CD %in% sel_concepts,]
dim(dat_fitbit)

## keep only records collected 6 months after the first infection date
dat_fitbit <- KeepDigitalDataCollectedXDaysFromFirstInfection(dat_fitbit,
                                                              first_infection_dates,
                                                              thr = 182)
dim(dat_fitbit)


## SUMMARY_DATE is always a Sunday (it corresponds to the start_date of
## the week of summarized data). We add 3 to move it to the mid-week date
## (a Wednesday always)
dat_fitbit$weekly_date <- lubridate::as_date(dat_fitbit$SUMMARY_DATE)
dat_fitbit$weekly_date <- dat_fitbit$weekly_date + 3


## get covariates (month, numrecs_in_window, days_between_visits)
covdat_efficiency <- ShapeData(dat_clinical = dat_visit, 
                               dat_digital = dat_fitbit, 
                               measure_name = "efficiency", 
                               window_size = 182,
                               days_between_visits_thr = 30)


covdat_minsasleep <- ShapeData(dat_clinical = dat_visit, 
                               dat_digital = dat_fitbit, 
                               measure_name = "minsasleep", 
                               window_size = 182,
                               days_between_visits_thr = 30)

covdat_minsawake <- ShapeData(dat_clinical = dat_visit, 
                              dat_digital = dat_fitbit, 
                              measure_name = "minsawake", 
                              window_size = 182,
                              days_between_visits_thr = 30)

covdat_sleepleveldeep <- ShapeData(dat_clinical = dat_visit, 
                                   dat_digital = dat_fitbit, 
                                   measure_name = "sleepleveldeep", 
                                   window_size = 182,
                                   days_between_visits_thr = 30)

covdat_sleeplevellight <- ShapeData(dat_clinical = dat_visit, 
                                    dat_digital = dat_fitbit, 
                                    measure_name = "sleeplevellight", 
                                    window_size = 182,
                                    days_between_visits_thr = 30)

covdat_sleeplevelrem <- ShapeData(dat_clinical = dat_visit, 
                                  dat_digital = dat_fitbit, 
                                  measure_name = "sleeplevelrem", 
                                  window_size = 182,
                                  days_between_visits_thr = 30)

covdat_remfragmentationind = ShapeData(dat_clinical = dat_visit, 
                                       dat_digital = dat_fitbit, 
                                       measure_name = "remfragmentationind", 
                                       window_size = 182,
                                       days_between_visits_thr = 30)

covdat_remonsetlatency = ShapeData(dat_clinical = dat_visit, 
                                   dat_digital = dat_fitbit, 
                                   measure_name = "remonsetlatency", 
                                   window_size = 182,
                                   days_between_visits_thr = 30)

covdat_remsleepbrthrate <- ShapeData(dat_clinical = dat_visit, 
                                     dat_digital = dat_fitbit, 
                                     measure_name = "remsleepbrthrate", 
                                     window_size = 182,
                                     days_between_visits_thr = 30)

covdat_restinghr <- ShapeData(dat_clinical = dat_visit, 
                              dat_digital = dat_fitbit, 
                              measure_name = "restinghr", 
                              window_size = 182,
                              days_between_visits_thr = 30)

covdat_hrv <- ShapeData(dat_clinical = dat_visit, 
                        dat_digital = dat_fitbit, 
                        measure_name = "hrv", 
                        window_size = 182,
                        days_between_visits_thr = 30)

covdat_spo2avg <- ShapeData(dat_clinical = dat_visit, 
                            dat_digital = dat_fitbit, 
                            measure_name = "spo2avg", 
                            window_size = 182,
                            days_between_visits_thr = 30)

dat_midsleep_sd_5days <- GrabStandardDeviationData(dat_midsleep, wdat_5d)
dat_duration_sd_5days <- GrabStandardDeviationData(dat_duration, wdat_5d)
dat_midsleep_sd_14days <- GrabStandardDeviationData(dat_midsleep, wdat_14d)
dat_duration_sd_14days <- GrabStandardDeviationData(dat_duration, wdat_14d)

covdat_midsleep_sd_5days <- ShapeDataSD(dat_clinical = dat_visit, 
                                        dat_digital = dat_midsleep_sd_5days)

covdat_duration_sd_5days <- ShapeDataSD(dat_clinical = dat_visit, 
                                        dat_digital = dat_duration_sd_5days)

covdat_midsleep_sd_14days <- ShapeDataSD(dat_clinical = dat_visit, 
                                         dat_digital = dat_midsleep_sd_14days)

covdat_duration_sd_14days <- ShapeDataSD(dat_clinical = dat_visit, 
                                         dat_digital = dat_duration_sd_14days)



write.csv(covdat_efficiency, file="covdat_efficiency.csv")
write.csv(covdat_minsasleep, file="covdat_minsasleep.csv")
write.csv(covdat_minsawake, file="covdat_minsawake.csv")
write.csv(covdat_sleepleveldeep, file="covdat_sleepleveldeep.csv")
write.csv(covdat_sleeplevellight, file="covdat_sleeplevellight.csv")
write.csv(covdat_sleeplevelrem, file="covdat_sleeplevelrem.csv")
write.csv(covdat_remfragmentationind, file="covdat_remfragmentationind.csv")
write.csv(covdat_remonsetlatency, file="covdat_remonsetlatency.csv")
write.csv(covdat_remsleepbrthrate, file="covdat_remsleepbrthrate.csv")
write.csv(covdat_restinghr, file="covdat_restinghr.csv")
write.csv(covdat_midsleep_sd_5days, file="covdat_midsleep_sd_5days.csv")
write.csv(covdat_duration_sd_5days, file="covdat_duration_sd_5days.csv")
write.csv(covdat_midsleep_sd_14days, file="covdat_midsleep_sd_14days.csv")
write.csv(covdat_duration_sd_14days, file="covdat_duration_sd_14days.csv")
write.csv(covdat_hrv, file="covdat_hrv.csv")
write.csv(covdat_spo2avg, file="covdat_spo2avg.csv")
