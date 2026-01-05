
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


GetPascGroup <- function(pasc1, pasc2, pool_na = TRUE) {
  ## define PASC groupings
  grp <- paste(pasc1, pasc2, sep = "/")
  grp[which(grp == "PASC/PASC")] <- "+/+"
  grp[which(grp == "PASC/PASC Indeterminate")] <- "+/-"
  grp[which(grp == "PASC Indeterminate/PASC")] <- "-/+"
  grp[which(grp == "PASC Indeterminate/PASC Indeterminate")] <- "-/-"
  if (pool_na) {
    grp[which(grp == "NA/PASC")] <- "?"
    grp[which(grp == "NA/PASC Indeterminate")] <- "?"
    grp[which(grp == "PASC/NA")] <- "?"
    grp[which(grp == "PASC Indeterminate/NA")] <- "?"
    grp[which(grp == "NA/NA")] <- "?"
  }
  else {
    grp[which(grp == "NA/PASC")] <- "?/+"
    grp[which(grp == "NA/PASC Indeterminate")] <- "?/-"
    grp[which(grp == "PASC/NA")] <- "+/?"
    grp[which(grp == "PASC Indeterminate/NA")] <- "-/?"
    grp[which(grp == "NA/NA")] <- "?/?"
  }
  
  return(grp)
}


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



CategorizeBMI <- function(dat) {
  num_bmi <- dat$bmi
  cat_bmi <- rep(NA, length(num_bmi))
  cat_bmi[which(num_bmi < 18.5)] <- "Underweight"
  cat_bmi[which(num_bmi >= 18.5 & num_bmi < 25)] <- "Healthy Weight"
  cat_bmi[which(num_bmi >= 25 & num_bmi < 30)] <- "Overweight"
  cat_bmi[which(num_bmi >= 30)] <- "Obesity"
  dat$bmi <- cat_bmi
  
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


ShapeDataScore <- function(dat_clinical, 
                           dat_digital, 
                           measure_name, 
                           window_size,
                           days_between_visits_thr = 30,
                           pool_na = TRUE,
                           pasc_score_name = "pasc_score_2024") {
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
  colnames(dat) <- c("record_id", "pasc_score", "pasc_score_1", "pasc_score_2", "digital_measure", 
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
        pasc_score_1 <- sdat_clinical[idx1, pasc_score_name]
        
        ## pasc status at second selected visit
        pasc_score_2 <- sdat_clinical[idx2, pasc_score_name]
        
        ## define PASC groupings
        dat[i, "pasc_score"] <- mean(c(pasc_score_1, pasc_score_2), na.rm = TRUE)
        
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
        pasc_score_1 <- sdat_clinical[idx1, pasc_score_name]
        
        ## pasc status at second selected visit
        pasc_score_2 <- NA
        
        ## define PASC groupings
        dat[i, "pasc_score"] <- pasc_score_1
        
        ## get the dates of the first and second selected visits
        visit_dt_1 <- sdat_clinical$visit_dt[idx1]
        visit_dt_2 <- sdat_clinical$index_dt_curr[idx1] + 30 * sdat_clinical$visit_month_curr[idx1] + 90
      }
      
      dat[i, "pasc_score_1"] <- pasc_score_1
      dat[i, "pasc_score_2"] <- pasc_score_2
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
  
  ## rescale the remonsetlatency measure to minutes
  if (measure_name == "remonsetlatency") {
    dat$digital_measure <- dat$digital_measure/60
  }
  
  return(dat)
}


ShapeDataSDScore <- function(dat_clinical, 
                             dat_digital,
                             measure_name,
                             days_between_visits_thr = 30,
                             pool_na = TRUE,
                             pasc_score_name = "pasc_score_2024") {
  dat_clinical$visit_dt <- lubridate::as_date(dat_clinical$visit_dt)
  dat_clinical$index_dt_curr <- lubridate::as_date(dat_clinical$index_dt_curr)
  
  dat_clinical <- dat_clinical[!is.na(dat_clinical$visit_dt),]
  ids_digital <- unique(dat_digital$record_id)
  ids_clinical <- unique(dat_clinical$record_id)
  ids <- intersect(ids_digital, ids_clinical)
  nids <- length(ids)
  
  dat <- data.frame(matrix(NA, nids, 16))
  colnames(dat) <- c("record_id", "pasc_score", "pasc_score_1", "pasc_score_2", "digital_measure", 
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
        pasc_score_1 <- sdat_clinical[idx1, pasc_score_name]
        
        ## pasc status at second selected visit
        pasc_score_2 <- sdat_clinical[idx2, pasc_score_name]
        
        ## 
        dat[i, "pasc_score"] <- mean(c(pasc_score_1, pasc_score_2), na.rm = TRUE)
        
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
        pasc_score_1 <- sdat_clinical[idx1, pasc_score_name]
        
        ## pasc status at second selected visit
        pasc_score_2 <- NA
        
        ## define PASC groupings
        dat[i, "pasc_score"] <- pasc_score_1
        
        ## get the dates of the first and second selected visits
        visit_dt_1 <- sdat_clinical$visit_dt[idx1]
        visit_dt_2 <- sdat_clinical$index_dt_curr[idx1] + 30 * sdat_clinical$visit_month_curr[idx1] + 90
      }
      
      dat[i, "pasc_score_1"] <- pasc_score_1
      dat[i, "pasc_score_2"] <- pasc_score_2
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
  
  ## rescale the SD measures to minutes
  if (measure_name == "duration_sd") {
    dat$digital_measure <- dat$digital_measure/60000
  }
  if (measure_name == "midsleep_sd") {
    dat$digital_measure <- dat$digital_measure * 60
  }
  
  return(dat)
}





RunRegr <- function(dat_clinical, 
                    dat_digital,
                    dat_covariates,
                    measure_names,
                    covariate_names,
                    dhd_covariate_names,
                    window_size = 182,
                    ids_to_keep_list = NULL,
                    pasc_score_name = "pasc_score_2024",
                    scale_covariates = FALSE) {
  
  n_measures <- length(measure_names)
  
  processed_datasets <- vector(mode = "list", length = n_measures)
  names(processed_datasets) <- measure_names
  
  model_fits <- vector(mode = "list", length = n_measures)
  names(model_fits) <- measure_names
  
  outputs <- data.frame(matrix(NA, n_measures, 4))
  names(outputs) <- c("digital_measure",
                      "total n", 
                      "analysis n",
                      "test_pval")
  outputs[, 1] <- measure_names
  
  if (is.null(covariate_names) & is.null(dhd_covariate_names)) {
    model_covariates <- NULL
    model_formula <- as.formula("pasc_score ~ digital_measure")
  }
  else {
    model_covariates <- c(covariate_names, dhd_covariate_names)
    cov_formula <- paste(model_covariates, collapse = " + ")
    model_formula <- as.formula(paste0("pasc_score ~ digital_measure + ", 
                                       cov_formula))
  }
  
  for (i in seq(n_measures)) {
    
    cat("########################################### ", i, "\n")
    cat("shaping data for ", measure_names[i], "\n")
    adat <- ShapeDataScore(dat_clinical, 
                           dat_digital, 
                           measure_name = measure_names[i], 
                           window_size,
                           pool_na = FALSE,
                           pasc_score_name = pasc_score_name)
    
    
    ## keep only participants that passed the quality control filter
    if (!is.null(ids_to_keep_list[[i]])) {
      ids_to_keep <- ids_to_keep_list[[i]]
      adat <- adat[adat$record_id %in% ids_to_keep,]
    }
    
    ## add covariates data
    adat <- merge(adat, dat_covariates[, c("record_id", covariate_names), drop = FALSE], by = "record_id")
    
    outputs[i, "total n"] <- nrow(adat)
    
    adat <- adat[, c("record_id", 
                     "pasc_score", 
                     "digital_measure",
                     model_covariates)]
    
    adat <- na.omit(adat)
    outputs[i, "analysis n"] <- nrow(adat)
    
    if (scale_covariates) {
      adat <- MyScaling(adat, model_covariates)
    }
    
    cat("running regression for ", measure_names[i], "\n")
    fit <- try(lm(model_formula, data = adat), silent = TRUE)
    if (!inherits(fit, "try-error")) {
      su <- summary(fit)
      outputs[i, "test_pval"] <- su$coefficients["digital_measure", "Pr(>|t|)"]
      model_fits[[i]] <- fit
    }
    
    processed_datasets[[i]] <- adat
  }
  
  return(list(outputs = outputs,
              processed_datasets = processed_datasets,
              model_fits = model_fits))
}


RunRegrSD <- function(dat_clinical, 
                      dat_digital,
                      dat_covariates,
                      measure_name,
                      covariate_names,
                      dhd_covariate_names,
                      window_size = 182,
                      ids_to_keep_list = NULL,
                      pasc_score_name = "pasc_score_2024",
                      scale_covariates = FALSE) {
  
  n_measures <- length(measure_name)
  
  processed_datasets <- vector(mode = "list", length = n_measures)
  names(processed_datasets) <- measure_name
  
  model_fits <- vector(mode = "list", length = n_measures)
  names(model_fits) <- measure_name
  
  outputs <- data.frame(matrix(NA, n_measures, 4))
  names(outputs) <- c("digital_measure",
                      "total n", 
                      "analysis n",
                      "test_pval")
  outputs[, 1] <- measure_name
  
  if (is.null(covariate_names) & is.null(dhd_covariate_names)) {
    model_covariates <- NULL
    model_formula <- as.formula("pasc_score ~ digital_measure")
  }
  else {
    model_covariates <- c(covariate_names, dhd_covariate_names)
    cov_formula <- paste(model_covariates, collapse = " + ")
    model_formula <- as.formula(paste0("pasc_score ~ digital_measure + ", 
                                       cov_formula))
  }
  
  for (i in seq(n_measures)) {
    
    cat("########################################### ", i, "\n")
    cat("shaping data for ", measure_name[i], "\n")
    adat <- ShapeDataSDScore(dat_clinical, 
                             dat_digital, 
                             measure_name, 
                             pool_na = FALSE,
                             pasc_score_name = pasc_score_name)
    
    ## keep only participants that passed the quality control filter
    if (!is.null(ids_to_keep_list[[i]])) {
      ids_to_keep <- ids_to_keep_list[[i]]
      adat <- adat[adat$record_id %in% ids_to_keep,]
    }
    
    ## add covariates data
    adat <- merge(adat, dat_covariates[, c("record_id", covariate_names), drop = FALSE], by = "record_id")
    
    outputs[i, "total n"] <- nrow(adat)
    
    adat <- adat[, c("record_id", 
                     "pasc_score", 
                     "digital_measure",
                     model_covariates)]
    
    adat <- na.omit(adat)
    outputs[i, "analysis n"] <- nrow(adat)
    
    if (scale_covariates) {
      adat <- MyScaling(adat, model_covariates)
    }
    
    cat("running regression for ", measure_name[i], "\n")
    fit <- try(lm(model_formula, data = adat), silent = TRUE)
    if (!inherits(fit, "try-error")) {
      su <- summary(fit)
      outputs[i, "test_pval"] <- su$coefficients["digital_measure", "Pr(>|t|)"]
      model_fits[[i]] <- fit
    }
    
    processed_datasets[[i]] <- adat
  }
  
  return(list(outputs = outputs,
              processed_datasets = processed_datasets,
              model_fits = model_fits))
}


RunLogisticRegr <- function(dat_clinical, 
                            dat_digital,
                            dat_covariates,
                            measure_names,
                            covariate_names,
                            dhd_covariate_names,
                            window_size = 182,
                            ids_to_keep_list = NULL,
                            scale_covariates = FALSE) {
  
  n_measures <- length(measure_names)
  
  processed_datasets <- vector(mode = "list", length = n_measures)
  names(processed_datasets) <- measure_names
  
  model_fits <- vector(mode = "list", length = n_measures)
  names(model_fits) <- measure_names
  
  outputs <- data.frame(matrix(NA, n_measures, 4))
  names(outputs) <- c("digital_measure",
                      "total n", 
                      "analysis n",
                      "test_pval")
  outputs[, 1] <- measure_names
  
  if (is.null(covariate_names) & is.null(dhd_covariate_names)) {
    model_covariates <- NULL
    model_formula <- as.formula("pasc_recoded ~ digital_measure")
  }
  else {
    model_covariates <- c(covariate_names, dhd_covariate_names)
    cov_formula <- paste(model_covariates, collapse = " + ")
    model_formula <- as.formula(paste0("pasc_recoded ~ digital_measure + ", 
                                       cov_formula))
  }
  
  for (i in seq(n_measures)) {
    
    cat("########################################### ", i, "\n")
    cat("shaping data for ", measure_names[i], "\n")
    adat <- ShapeData(dat_clinical, 
                      dat_digital, 
                      measure_name = measure_names[i], 
                      window_size,
                      pool_na = FALSE)
    
    ## recode the pasc group data
    idx_1 <- which(adat$pasc_group == "-/-")
    idx_2 <- which(adat$pasc_group == "-/+")
    idx_3 <- which(adat$pasc_group == "+/-")
    idx_4 <- which(adat$pasc_group == "+/+")
    adat$pasc_recoded <- NA
    adat$pasc_recoded[idx_1] <- 0
    adat$pasc_recoded[idx_2] <- 1
    adat$pasc_recoded[idx_3] <- 1
    adat$pasc_recoded[idx_4] <- 1
    idx_5 <- which(adat$pasc_group == "-/?")
    idx_6 <- which(adat$pasc_group == "+/?")    
    adat$pasc_recoded[idx_5] <- 0
    adat$pasc_recoded[idx_6] <- 1
    
    ## keep only participants that passed the quality control filter
    if (!is.null(ids_to_keep_list[[i]])) {
      ids_to_keep <- ids_to_keep_list[[i]]
      adat <- adat[adat$record_id %in% ids_to_keep,]
    }
    
    ## add covariates data
    adat <- merge(adat, dat_covariates[, c("record_id", covariate_names), drop = FALSE], by = "record_id")
    
    adat <- adat[, c("record_id", 
                     "pasc_group", 
                     "pasc_recoded", 
                     "digital_measure",
                     model_covariates)]
    
    outputs[i, "total n"] <- nrow(adat)
    
    adat <- na.omit(adat)
    outputs[i, "analysis n"] <- nrow(adat)
    
    if (scale_covariates) {
      adat <- MyScaling(adat, model_covariates)
    }
    
    cat("running ordinal logistic regression for ", measure_names[i], "\n")
    fit <- try(glm(model_formula, family = "binomial", data = adat), silent = TRUE)
    if (!inherits(fit, "try-error")) {
      su <- summary(fit)
      outputs[i, "test_pval"] <- su$coefficients["digital_measure", "Pr(>|z|)"]
      model_fits[[i]] <- fit
    }
    
    processed_datasets[[i]] <- adat
    
  }
  
  return(list(outputs = outputs,
              processed_datasets = processed_datasets,
              model_fits = model_fits))
}



RunLogisticRegrSD <- function(dat_clinical, 
                              dat_digital,
                              dat_covariates,
                              measure_name,
                              covariate_names,
                              dhd_covariate_names,
                              window_size = 182,
                              ids_to_keep_list = NULL,
                              scale_covariates = FALSE) {
  
  n_measures <- length(measure_name)
  
  processed_datasets <- vector(mode = "list", length = n_measures)
  names(processed_datasets) <- measure_name
  
  model_fits <- vector(mode = "list", length = n_measures)
  names(model_fits) <- measure_name
  
  outputs <- data.frame(matrix(NA, n_measures, 4))
  names(outputs) <- c("digital_measure",
                      "total n", 
                      "analysis n",
                      "test_pval")
  outputs[, 1] <- measure_name
  
  if (is.null(covariate_names) & is.null(dhd_covariate_names)) {
    model_covariates <- NULL
    model_formula <- as.formula("pasc_recoded ~ digital_measure")
  }
  else {
    model_covariates <- c(covariate_names, dhd_covariate_names)
    cov_formula <- paste(model_covariates, collapse = " + ")
    model_formula <- as.formula(paste0("pasc_recoded ~ digital_measure + ", 
                                       cov_formula))
  }
  
  for (i in seq(n_measures)) {
    
    cat("########################################### ", i, "\n")
    cat("shaping data for ", measure_name[i], "\n")
    adat <- ShapeDataSD(dat_clinical, dat_digital, measure_name, pool_na = FALSE)
    
    ## recode the pasc group data
    idx_1 <- which(adat$pasc_group == "-/-")
    idx_2 <- which(adat$pasc_group == "-/+")
    idx_3 <- which(adat$pasc_group == "+/-")
    idx_4 <- which(adat$pasc_group == "+/+")
    adat$pasc_recoded <- NA
    adat$pasc_recoded[idx_1] <- 0
    adat$pasc_recoded[idx_2] <- 1
    adat$pasc_recoded[idx_3] <- 1
    adat$pasc_recoded[idx_4] <- 1
    idx_5 <- which(adat$pasc_group == "-/?")
    idx_6 <- which(adat$pasc_group == "+/?")
    adat$pasc_recoded[idx_5] <- 0
    adat$pasc_recoded[idx_6] <- 1
    
    ## keep only participants that passed the quality control filter
    if (!is.null(ids_to_keep_list[[i]])) {
      ids_to_keep <- ids_to_keep_list[[i]]
      adat <- adat[adat$record_id %in% ids_to_keep,]
    }
    
    ## add covariates data
    adat <- merge(adat, dat_covariates[, c("record_id", covariate_names), drop = FALSE], by = "record_id")
    
    outputs[i, "total n"] <- nrow(adat)
    
    adat <- adat[, c("record_id", 
                     "pasc_group", 
                     "pasc_recoded", 
                     "digital_measure",
                     model_covariates)]
    
    adat <- na.omit(adat)
    outputs[i, "analysis n"] <- nrow(adat)
    
    if (scale_covariates) {
      adat <- MyScaling(adat, model_covariates)
    }
    
    cat("running logistic regression for ", measure_name[i], "\n")
    fit <- try(glm(model_formula, family = "binomial", data = adat), silent = TRUE)
    if (!inherits(fit, "try-error")) {
      su <- summary(fit)
      outputs[i, "test_pval"] <- su$coefficients["digital_measure", "Pr(>|z|)"]
      model_fits[[i]] <- fit
    }
    
    processed_datasets[[i]] <- adat
  }
  
  return(list(outputs = outputs,
              processed_datasets = processed_datasets,
              model_fits = model_fits))
}



## scale the data from the numeric covariates
MyScaling <- function(dat, covariate_names) {
  covariate_names <- c("digital_measure", covariate_names)
  covariate_types <- sapply(dat[, covariate_names, drop = FALSE], "class")
  idx_numeric <- which(covariate_types == "numeric")
  idx_integer <- which(covariate_types == "integer")
  numeric_covariates <- covariate_names[c(idx_numeric, idx_integer)]
  dat[, numeric_covariates] <- scale(dat[, numeric_covariates])
  
  return(dat)
}


CreateEffectSizeTableLR <- function(m_sleep,
                                    m_midsleep_sd,
                                    m_duration_sd,
                                    m_hr,
                                    m_spo2, 
                                    adjusted_pvals,
                                    ordered_variables) {
  
  GetSignificance <- function(x) {
    x$sig <- " "
    x$sig[which(x[, "p-value (adjusted)"] < 0.05)] <- "*"
    x$sig[which(x[, "p-value (adjusted)"] < 0.01)] <- "**"
    x$sig[which(x[, "p-value (adjusted)"] < 0.001)] <- "***"
    
    return(x)
  }
  
  ## fixed internal order
  all_variables <- c("Sleep Efficiency", 
                     "WASO", 
                     "REM Sleep Breathing Rate",
                     "Minutes in Deep Sleep", 
                     "REM Fragmentation Index", 
                     "REM Onset Latency",
                     "Sleep Duration",
                     "Minutes in Light Sleep",
                     "Minutes in REM Sleep",
                     "SD of Mid-Sleep",
                     "SD of Sleep Duration",
                     "Resting Heart Rate",
                     "Heart Rate Variability",
                     "SpO2")
  
  out <- data.frame(matrix(NA, 14, 7))
  rownames(out) <- all_variables
  colnames(out) <- c("Estimate", 
                     "Std. Error", 
                     "z-value", 
                     "p-value (adjusted)", 
                     "lower bound (95% CI)", 
                     "upper bound (95% CI)",
                     "nobs")
  
  ## get outputs from model fit summaries 
  su <- summary(m_sleep$model_fits$efficiency)
  out[1, 1:4] <- su$coefficients["digital_measure",]
  
  su <- summary(m_sleep$model_fits$minsawake)
  out[2, 1:4] <- su$coefficients["digital_measure",]
  
  su <- summary(m_sleep$model_fits$remsleepbrthrate)
  out[3, 1:4] <- su$coefficients["digital_measure",]
  
  su <- summary(m_sleep$model_fits$sleepleveldeep)
  out[4, 1:4] <- su$coefficients["digital_measure",]
  
  su <- summary(m_sleep$model_fits$remfragmentationind)
  out[5, 1:4] <- su$coefficients["digital_measure",]
  
  su <- summary(m_sleep$model_fits$remonsetlatency)
  out[6, 1:4] <- su$coefficients["digital_measure",]
  
  su <- summary(m_sleep$model_fits$minsasleep)
  out[7, 1:4] <- su$coefficients["digital_measure",]
  
  su <- summary(m_sleep$model_fits$sleeplevellight)
  out[8, 1:4] <- su$coefficients["digital_measure",]
  
  su <- summary(m_sleep$model_fits$sleeplevelrem)
  out[9, 1:4] <- su$coefficients["digital_measure",]
  
  su <- summary(m_midsleep_sd$model_fits$midsleep_sd)
  out[10, 1:4] <- su$coefficients["digital_measure",]
  
  su <- summary(m_duration_sd$model_fits$duration_sd)
  out[11, 1:4] <- su$coefficients["digital_measure",]
  
  su <- summary(m_hr$model_fits$restinghr)
  out[12, 1:4] <- su$coefficients["digital_measure",]
  
  su <- summary(m_hr$model_fits$hrv)
  out[13, 1:4] <- su$coefficients["digital_measure",]
  
  su <- summary(m_spo2$model_fits$spo2avg)
  out[14, 1:4] <- su$coefficients["digital_measure",]
  
  ## replace the p-values with the adjusted p-values
  out[, 4] <- adjusted_pvals
  
  ## compute confidence intervals
  out[1, 5:6] <- confint.default(m_sleep$model_fits$efficiency)["digital_measure",]
  out[2, 5:6] <- confint.default(m_sleep$model_fits$minsawake)["digital_measure",]
  out[3, 5:6] <- confint.default(m_sleep$model_fits$remsleepbrthrate)["digital_measure",]
  out[4, 5:6] <- confint.default(m_sleep$model_fits$sleepleveldeep)["digital_measure",]
  out[5, 5:6] <- confint.default(m_sleep$model_fits$remfragmentationind)["digital_measure",]
  out[6, 5:6] <- confint.default(m_sleep$model_fits$remonsetlatency)["digital_measure",]
  out[7, 5:6] <- confint.default(m_sleep$model_fits$minsasleep)["digital_measure",]
  out[8, 5:6] <- confint.default(m_sleep$model_fits$sleeplevellight)["digital_measure",]
  out[9, 5:6] <- confint.default(m_sleep$model_fits$sleeplevelrem)["digital_measure",]
  out[10, 5:6] <- confint.default(m_midsleep_sd$model_fits$midsleep_sd)["digital_measure",]
  out[11, 5:6] <- confint.default(m_duration_sd$model_fits$duration_sd)["digital_measure",]
  out[12, 5:6] <- confint.default(m_hr$model_fits$restinghr)["digital_measure",]
  out[13, 5:6] <- confint.default(m_hr$model_fits$hrv)["digital_measure",]
  out[14, 5:6] <- confint.default(m_spo2$model_fits$spo2avg)["digital_measure",]
  
  out[1:9, 7] <- m_sleep$outputs[, "analysis n"]
  out[10, 7] <- m_midsleep_sd$outputs[, "analysis n"]
  out[11, 7] <- m_duration_sd$outputs[, "analysis n"]
  out[12:13, 7] <- m_hr$outputs[, "analysis n"]
  out[14, 7] <- m_spo2$outputs[, "analysis n"]
  
  out <- out[, c(7, 1:6)]
  
  out <- GetSignificance(out)
  
  out$iv <- rownames(out)
  rownames(out) <- NULL
  
  out <- out[, c(9, 1:5, 8, 6:7)]
  
  ## reorder the variables
  out <- out[match(ordered_variables, out$iv),]
  
  return(out)
}


CreateEffectSizeTableRegr <- function(m_sleep,
                                      m_midsleep_sd,
                                      m_duration_sd,
                                      m_hr,
                                      m_spo2, 
                                      adjusted_pvals,
                                      ordered_variables) {
  GetSignificance <- function(x) {
    x$sig <- " "
    x$sig[which(x[, "p-value (adjusted)"] < 0.05)] <- "*"
    x$sig[which(x[, "p-value (adjusted)"] < 0.01)] <- "**"
    x$sig[which(x[, "p-value (adjusted)"] < 0.001)] <- "***"
    
    return(x)
  }
  
  ## fixed internal order
  all_variables <- c("Sleep Efficiency", 
                     "WASO", 
                     "REM Sleep Breathing Rate",
                     "Minutes in Deep Sleep", 
                     "REM Fragmentation Index", 
                     "REM Onset Latency",
                     "Sleep Duration",
                     "Minutes in Light Sleep",
                     "Minutes in REM Sleep",
                     "SD of Mid-Sleep",
                     "SD of Sleep Duration",
                     "Resting Heart Rate",
                     "Heart Rate Variability",
                     "SpO2")
  
  out <- data.frame(matrix(NA, 14, 7))
  rownames(out) <- all_variables
  colnames(out) <- c("Estimate", 
                     "Std. Error", 
                     "t-value", 
                     "p-value (adjusted)", 
                     "lower bound (95% CI)", 
                     "upper bound (95% CI)", 
                     "nobs")
  
  su <- summary(m_sleep$model_fits$efficiency)
  out[1, 1:4] <- su$coefficients["digital_measure",]
  
  su <- summary(m_sleep$model_fits$minsawake)
  out[2, 1:4] <- su$coefficients["digital_measure",]
  
  su <- summary(m_sleep$model_fits$remsleepbrthrate)
  out[3, 1:4] <- su$coefficients["digital_measure",]
  
  su <- summary(m_sleep$model_fits$sleepleveldeep)
  out[4, 1:4] <- su$coefficients["digital_measure",]
  
  su <- summary(m_sleep$model_fits$remfragmentationind)
  out[5, 1:4] <- su$coefficients["digital_measure",]
  
  su <- summary(m_sleep$model_fits$remonsetlatency)
  out[6, 1:4] <- su$coefficients["digital_measure",]
  
  su <- summary(m_sleep$model_fits$minsasleep)
  out[7, 1:4] <- su$coefficients["digital_measure",]
  
  su <- summary(m_sleep$model_fits$sleeplevellight)
  out[8, 1:4] <- su$coefficients["digital_measure",]
  
  su <- summary(m_sleep$model_fits$sleeplevelrem)
  out[9, 1:4] <- su$coefficients["digital_measure",]
  
  su <- summary(m_midsleep_sd$model_fits$midsleep_sd)
  out[10, 1:4] <- su$coefficients["digital_measure",]
  
  su <- summary(m_duration_sd$model_fits$duration_sd)
  out[11, 1:4] <- su$coefficients["digital_measure",]
  
  su <- summary(m_hr$model_fits$restinghr)
  out[12, 1:4] <- su$coefficients["digital_measure",]
  
  su <- summary(m_hr$model_fits$hrv)
  out[13, 1:4] <- su$coefficients["digital_measure",]
  
  su <- summary(m_spo2$model_fits$spo2avg)
  out[14, 1:4] <- su$coefficients["digital_measure",]
  
  ## replace the p-values with the adjusted p-values
  out[, 4] <- adjusted_pvals
  
  ## compute confidence intervals
  out[1, 5:6] <- confint.default(m_sleep$model_fits$efficiency)["digital_measure",]
  out[2, 5:6] <- confint.default(m_sleep$model_fits$minsawake)["digital_measure",]
  out[3, 5:6] <- confint.default(m_sleep$model_fits$remsleepbrthrate)["digital_measure",]
  out[4, 5:6] <- confint.default(m_sleep$model_fits$sleepleveldeep)["digital_measure",]
  out[5, 5:6] <- confint.default(m_sleep$model_fits$remfragmentationind)["digital_measure",]
  out[6, 5:6] <- confint.default(m_sleep$model_fits$remonsetlatency)["digital_measure",]
  out[7, 5:6] <- confint.default(m_sleep$model_fits$minsasleep)["digital_measure",]
  out[8, 5:6] <- confint.default(m_sleep$model_fits$sleeplevellight)["digital_measure",]
  out[9, 5:6] <- confint.default(m_sleep$model_fits$sleeplevelrem)["digital_measure",]
  out[10, 5:6] <- confint.default(m_midsleep_sd$model_fits$midsleep_sd)["digital_measure",]
  out[11, 5:6] <- confint.default(m_duration_sd$model_fits$duration_sd)["digital_measure",]
  out[12, 5:6] <- confint.default(m_hr$model_fits$restinghr)["digital_measure",]
  out[13, 5:6] <- confint.default(m_hr$model_fits$hrv)["digital_measure",]
  out[14, 5:6] <- confint.default(m_spo2$model_fits$spo2avg)["digital_measure",]
  
  out[1:9, 7] <- m_sleep$outputs[, "analysis n"]
  out[10, 7] <- m_midsleep_sd$outputs[, "analysis n"]
  out[11, 7] <- m_duration_sd$outputs[, "analysis n"]
  out[12:13, 7] <- m_hr$outputs[, "analysis n"]
  out[14, 7] <- m_spo2$outputs[, "analysis n"]
  
  out <- out[, c(7, 1:6)]
  
  out <- GetSignificance(out)
  
  out$iv <- rownames(out)
  rownames(out) <- NULL
  
  out <- out[, c(9, 1:5, 8, 6:7)]
  
  ## reorder the variables
  out <- out[match(ordered_variables, out$iv),]
  
  return(out)
}

