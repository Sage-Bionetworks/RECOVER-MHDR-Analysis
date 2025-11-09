# recode some clinical measures using rules provided by James
recode_dat_symptom = function(dat_symptom){
  # A = now variables; B = severity variables
  # A = 1 & B meets the criterion -> 1
  # A = 1 & B does not meet the criterion -> 0
  # A = 1 & B = NA -> NA
  # A = 0 -> 0
  # A = NA & B meets the criterion -> NA
  # A = NA & B does not meet the criterion -> 0
  # A = NA & B = NA -> NA
  
  dat_symptom$ps_think___now_sev <- ifelse(dat_symptom$ps_think___now==1 & dat_symptom$NQOL_CF_Tscore<=40, 1, 0)
  
  dat_symptom$pain_chest___now_sev <- ifelse(dat_symptom$pain_chest___now==1 & dat_symptom$saq_sumscore<75, 1, 0)
  
  dat_symptom$ps_fatigue___now_sev <- ifelse(dat_symptom$ps_fatigue___now==1 & dat_symptom$promis_global08>=3, 1, 0)
  
  dat_symptom$ps_sob___now_sev <- ifelse(dat_symptom$ps_sob___now==1 & dat_symptom$mmrc_dyspnea>1, 1, 0)
  
  dat_symptom$ps_sleepdist___now_sev <- ifelse(dat_symptom$ps_sleepdist___now==1 & dat_symptom$promis_sleepdist_sf8a_Tscore>=60, 1, 0)
  
  dat_symptom$pain_head___now_sev <- ifelse(dat_symptom$pain_head___now==1 & dat_symptom$hit6_total>=56, 1, 0)
  
  dat_symptom = dat_symptom %>%
    mutate(promis_global07_re = recode(promis_global07,
                                       `0` = 5, `1` = 4, `2` = 4,
                                       `3` = 4, `4` = 3, `5`= 3, `6` = 3,
                                       `7` = 2, `8` = 2,`9` = 2, `10` = 1),
           promis_global08_re = recode(promis_global08,
                                       `5` = 1, `4` = 2, `3`= 3, `2` = 4, `1`= 5),
           promis_global_ph = promis_global03 + promis_global06 + promis_global07_re + promis_global08_re,
           promis_global10_re = recode(promis_global10, `5` = 1, `4` = 2, `3` = 3, `2` = 4, `1` = 5),
           promis_global_mh = promis_global02 + promis_global04 + promis_global05 + promis_global10_re
    ) # the higher, the better
  
  ## convert global_ph and global_mh to T-scores
  global_ph_conversion = read.csv("promis_global_physical_conversion_table.csv")
  global_mh_conversion = read.csv("promis_global_mental_conversion_table.csv")
  
  dat_symptom = dat_symptom %>%
    left_join(global_ph_conversion, by = c("promis_global_ph" = "raw_sum_score")) %>%
    mutate(promis_global_ph_Tscore = t_score) %>%
    select(-t_score) 
  
  dat_symptom = dat_symptom %>%
    left_join(global_mh_conversion, by = c("promis_global_mh" = "raw_sum_score")) %>%
    mutate(promis_global_mh_Tscore = t_score) %>%
    select(-t_score) 
  
  return(dat_symptom)
}



# get the weighted average of each digital measure across time points for each participant 
aggregateData <- function(dat_fitbit_values,
                          dat_fitbit_numrecs,
                          concepts) {
  
  dat = merge(dat_fitbit_values, dat_fitbit_numrecs, by=c("record_id", "concept", "SUMMARY_DATE"))
  # compute weighted average
  out = dat %>%
    group_by(record_id, concept) %>%
    mutate(weights = SUMMARY_VALUE.y/sum(SUMMARY_VALUE.y, na.rm = TRUE)) %>%
    summarise(weighted_ave = sum(SUMMARY_VALUE.x * weights, na.rm = TRUE))
  # covert to wide format
  out_wide = pivot_wider(out, names_from = concept, values_from = weighted_ave)
  
  return(out_wide)
}


# recode binary symptom variables &
# get the means of continuous symptom variables (assuming the last two concepts are global health measures)
aggregateData2 = function(dat_symptom, 
                          concepts){
  
  # check whether this visit is the first or the second visit in the time window
  dat_symptom$visit_num = ifelse(dat_symptom$visit_dt==dat_symptom$visit_date_1, 1, 2)
  
  ids = unique(dat_symptom$record_id)
  out = data.frame(matrix(NA, nrow=length(ids), ncol=length(concepts)+1))
  colnames(out) = c("record_id", concepts)
  
  for(p in 1:length(ids)){
    out[p, "record_id"] = ids[p]
    
    for(i in 1:length(concepts)){
      sdat = dat_symptom[dat_symptom$record_id==ids[p], c("record_id", concepts[i], "visit_num")]
      colnames(sdat)[2] = "symptom"
      # symptom values from the first visit
      symptom1 = sdat$symptom[sdat$visit_num==1] # either 0, 1, NA, or empty (i.e., no first visit)
      # symptom values from the second visit
      symptom2 = sdat$symptom[sdat$visit_num==2] # either 0, 1, NA, or empty (i.e., no second visit)
      # combine symptom values from the two visits
      grp = paste(symptom1, symptom2, sep = "/")
      
      # assuming the last two concepts are global health measures
      if(i <= (length(concepts)-2)){
        out[p, concepts[i]] = ifelse(grp=="1/1"|grp=="1/0"|grp=="0/1"|grp=="1/NA"|grp=="1/", 1,
                                     ifelse(grp=="0/0"|grp=="0/NA"|grp=="0/", 0, 
                                            NA)
        )
      }else{
        out[p, concepts[i]] = mean(sdat$symptom, na.rm=T)
      }
      
    }
  }
  
  return(out)
}

# used for sensitivity analysis (code 0/- as NA)
aggregateData2_v2 = function(dat_symptom, 
                             concepts){
  
  # check whether this visit is the first or the second visit in the time window
  dat_symptom$visit_num = ifelse(dat_symptom$visit_dt==dat_symptom$visit_date_1, 1, 2)
  
  ids = unique(dat_symptom$record_id)
  out = data.frame(matrix(NA, nrow=length(ids), ncol=length(concepts)+1))
  colnames(out) = c("record_id", concepts)
  
  for(p in 1:length(ids)){
    out[p, "record_id"] = ids[p]
    
    for(i in 1:length(concepts)){
      sdat = dat_symptom[dat_symptom$record_id==ids[p], c("record_id", concepts[i], "visit_num")]
      colnames(sdat)[2] = "symptom"
      # symptom values from the first visit
      symptom1 = sdat$symptom[sdat$visit_num==1] # either 0, 1, NA, or empty (i.e., no first visit)
      # symptom values from the second visit
      symptom2 = sdat$symptom[sdat$visit_num==2] # either 0, 1, NA, or empty (i.e., no second visit)
      # combine symptom values from the two visits
      grp = paste(symptom1, symptom2, sep = "/")
      
      # assuming the last two concepts are global health measures
      if(i <= (length(concepts)-2)){
        out[p, concepts[i]] = ifelse(grp=="1/1"|grp=="1/0"|grp=="0/1"|grp=="1/NA"|grp=="1/", 1,
                                     ifelse(grp=="0/0", 0,  # 0/NA and 0/ are treated as NA
                                            NA)
        )
      }else{
        out[p, concepts[i]] = mean(sdat$symptom, na.rm=T)
      }
      
    }
  }
  
  return(out)
}

# get SD measures (Elias' code)
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


# process covariates data
proc_cov = function(dat_cov){
  
  dat_cov <- CategorizeBMI(dat_cov) 
  
  dat_cov$bmi = factor(dat_cov$bmi,
                       levels=c("Underweight",
                                "Healthy Weight",
                                "Overweight",
                                "Obesity")
  )
  
  dat_cov$alcohol = factor(dat_cov$alcohol,
                           levels=c("Never",
                                    "Monthly or Less than Monthly",
                                    "Daily or Weekly")
  )
  
  dat_cov$smoking = as.factor(dat_cov$smoking)
  
  return(dat_cov)
}

## process clinical data
proc_clinical = function(dat_symptom, clinical_measures, window){
  # only keep symptom data in the time window
  dat_symptom_new <- dat_symptom %>%
    right_join(window, by="record_id") %>%
    filter(visit_dt == visit_date_1 |
             visit_dt == visit_date_2)
  # get the mean/aggregated value for each participant
  clinical_mean = aggregateData2(dat_symptom_new, clinical_measures)
  
  return(clinical_mean)
}


# process sleep fitbit data (assuming the last two concepts are SD measures)
proc_digital_sleep = function(dat_fitbit, dat_midsleep, dat_duration, digital_measures, window){
  dat_fitbit_values <- dat_fitbit %>%
    filter(FITBIT_CONCEPT_CD %in% paste0("mhp:summary:weekly:mean:", digital_measures[1:(length(digital_measures)-2)])) %>%
    right_join(window, by="record_id") %>%
    # only keep fitbit data in the time window
    filter(SUMMARY_DATE >= window_start_date &    
             SUMMARY_DATE <= window_end_date) %>%
    group_by(FITBIT_CONCEPT_CD) %>%
    # extract the concept names (i.e., remove "mhp:summary:weekly:mean")
    mutate(concept = strsplit(FITBIT_CONCEPT_CD, ":")[[1]][5]) %>%
    ungroup() %>%
    dplyr::select(record_id, concept, SUMMARY_DATE, SUMMARY_VALUE)
  
  dat_fitbit_numrecs <- dat_fitbit %>%
    filter(FITBIT_CONCEPT_CD %in% paste0("mhp:summary:weekly:numrecords:", digital_measures[1:(length(digital_measures)-2)])) %>%
    right_join(window, by="record_id") %>%
    filter(SUMMARY_DATE >= window_start_date & 
             SUMMARY_DATE <= window_end_date) %>%
    group_by(FITBIT_CONCEPT_CD) %>%
    mutate(concept = strsplit(FITBIT_CONCEPT_CD, ":")[[1]][5]) %>%
    ungroup() %>%
    dplyr::select(record_id, concept, SUMMARY_DATE, SUMMARY_VALUE)
  
  # compute the weighted average of digital measures for each participant
  digital_mean <- aggregateData(dat_fitbit_values, dat_fitbit_numrecs, digital_measures[1:(length(digital_measures)-2)])
  
  # get the two SD measures
  dat_midsleep_sd <- GrabStandardDeviationData(dat_midsleep, window)
  dat_duration_sd <- GrabStandardDeviationData(dat_duration, window)
  colnames(dat_duration_sd)[2] = "minsasleepSD"
  colnames(dat_midsleep_sd)[2] = "midsleepSD"
  # merge to get the means of all digital measures 
  digital_mean <- merge(digital_mean, dat_duration_sd[,1:2], by="record_id", all.x=T)
  digital_mean <- merge(digital_mean, dat_midsleep_sd[,1:2], by="record_id", all.x=T)
  
  # convert the following measures to minutes
  digital_mean$remonsetlatency <- digital_mean$remonsetlatency/60
  digital_mean$minsasleepSD <- digital_mean$minsasleepSD/60000
  digital_mean$midsleepSD <- digital_mean$midsleepSD*60
  
  return(digital_mean)
}

# process hrv and spo2 fitbit data
proc_digital_hrvspo2 = function(dat_fitbit, digital_measures, window){
  dat_fitbit_values <- dat_fitbit %>%
    filter(FITBIT_CONCEPT_CD %in% paste0("mhp:summary:weekly:mean:", digital_measures)) %>%
    right_join(window, by="record_id") %>%
    filter(SUMMARY_DATE >= window_start_date & 
             SUMMARY_DATE <= window_end_date) %>%
    group_by(FITBIT_CONCEPT_CD) %>%
    mutate(concept = strsplit(FITBIT_CONCEPT_CD, ":")[[1]][5]) %>%
    ungroup() %>%
    dplyr::select(record_id, concept, SUMMARY_DATE, SUMMARY_VALUE)
  
  dat_fitbit_numrecs <- dat_fitbit %>%
    filter(FITBIT_CONCEPT_CD %in% paste0("mhp:summary:weekly:numrecords:", digital_measures)) %>%
    right_join(window, by="record_id") %>%
    filter(SUMMARY_DATE >= window_start_date & 
             SUMMARY_DATE <= window_end_date) %>%
    group_by(FITBIT_CONCEPT_CD) %>%
    mutate(concept = strsplit(FITBIT_CONCEPT_CD, ":")[[1]][5]) %>%
    ungroup() %>%
    dplyr::select(record_id, concept, SUMMARY_DATE, SUMMARY_VALUE)
  
  digital_mean <- aggregateData(dat_fitbit_values, dat_fitbit_numrecs, digital_measures)
  
  return(digital_mean)
}

# regression analysis (assuming the last two clinical measures are global health measures)
ind_reg = function(digital_mean,
                   clinical_mean,
                   dat_cov,
                   digital_measures, 
                   digital_names,
                   clinical_measures,
                   clinical_names,
                   daynum, 
                   covariate_names = NULL, 
                   model,
                   scale = TRUE){
  
  # set up model formulas
  if(is.null(covariate_names)){ # model 1
    model_formula <- as.formula("dv ~ iv")
  }else{  # model 2-6
    cov_formula <- paste(covariate_names, collapse = " + ")
    model_formula <- as.formula(paste("dv ~ iv + ", cov_formula))
  }
  
  resulttable_model = NULL
  for(j in 1:length(digital_measures)){
    
    print(digital_measures[j])
    
    # read in covariates (month, numrecs_in_window, days_between_visits) from external files 
    if(digital_measures[j] == "minsasleepSD"){
      covdat_j = read.csv(paste0("CovData/covdat_duration_sd_", daynum, "days.csv"))
    }
    
    if(digital_measures[j] == "midsleepSD"){
      covdat_j = read.csv(paste0("CovData/covdat_midsleep_sd_", daynum, "days.csv"))
    }
    
    if(!(digital_measures[j] %in% c("minsasleepSD", "midsleepSD"))){
      covdat_j = read.csv(paste0("CovData/covdat_", digital_measures[j], ".csv"))
    }
    
    covdat_j$month = as.factor(covdat_j$month)
    
    # merge with dat_cov (containing biosex, age_enroll, race_unique_an, bmi, smoking, alcohol) to get all covariates
    dat_cov_all = merge(dat_cov, covdat_j[,c("record_id","numrecs_in_window", "days_between_visits", "month")], 
                        by="record_id", all.x=T)
    
    # merge the iv and covariates
    dat_digital = digital_mean[, c("record_id", digital_measures[j])]
    colnames(dat_digital)[2] = "iv"
    dat_digital_withallcov = merge(dat_digital, dat_cov_all, by="record_id", all.x=T)
    
    for(i in 1:length(clinical_measures)){
      #print(clinical_measures[i])
      
      # merge dv, iv, and covariates
      dat_clinical = clinical_mean[, c("record_id", clinical_measures[i])]
      colnames(dat_clinical)[2] = "dv"
      df = merge(dat_digital_withallcov, dat_clinical, by="record_id")
      
      
      if(i<=(length(clinical_measures)-2)){
        if(scale==TRUE){ # scale numerical variables
          num_vars = c("iv", "age_enroll", "numrecs_in_window", "days_between_visits")
          df[, num_vars] = scale(df[, num_vars])
        }
        
        # logistic regression
        m = glm(model_formula, data = df, family = "binomial")
        
      }else{
        
        if(scale==TRUE){ # scale numerical variables
          num_vars = c("dv", "iv", "age_enroll", "numrecs_in_window", "days_between_visits")
          df[, num_vars] = scale(df[, num_vars])
        }
        
        # linear regression
        m = lm(model_formula, data = df)
      }
      
      # get coefficients, standard errors, t values, p values, CIs
      ctable = data.frame(cbind(summary(m)$coefficients, confint.default(m)))
      ctable = ctable[2, ] # only keep results for the iv
      colnames(ctable) = c("Estimate", "Std.Error", "t value", "p.value", "lower", "upper")
      ctable$nobs = nobs(m)  # number of complete samples in the analysis
      ctable$dv = clinical_names[i] # rename dv
      ctable$iv = digital_names[j]  # rename iv
      rownames(ctable) = NULL
      
      resulttable_model = rbind(resulttable_model, ctable)
      
    } # end of loop over i
    
  } # end of loop over j
  
  resulttable_model$model = model
  return(resulttable_model)
}

# correlation analysis without covariates
GetCorrMatrix = function(digital_mean,
                         clinical_mean,
                         #dat_cov,
                         digital_measures, 
                         clinical_measures){
  
  # merge digital and clinical data
  df = merge(clinical_mean, digital_mean, by="record_id")
  
  # get correlation (r) and significance (P) matrix
  rcorr_mat_list <- Hmisc::rcorr(as.matrix(df[,-1]))
  mat = rcorr_mat_list$r
  pmat = rcorr_mat_list$P
  
  # only keep correlations between clinical and digital measures
  mat = data.frame(mat[clinical_measures, digital_measures]) 
  colnames(mat) = digital_measures
  pmat = data.frame(pmat[clinical_measures, digital_measures]) 
  colnames(pmat) = digital_measures
  
  rcorr_mat_list = list(r = mat, P = pmat)
  return(rcorr_mat_list)
  
}

# partial correlation analysis accouting for covariates
GetPartialCorrMatrix = function(digital_mean,
                                clinical_mean,
                                dat_cov,
                                digital_measures, 
                                clinical_measures,
                                daynum,
                                covariate_names = NULL){
  
  # set up model formulas
  cov_formula <- paste(covariate_names, collapse = " + ")
  model_x_formula <- as.formula(paste("iv ~ ", cov_formula))
  model_y_formula <- as.formula(paste("dv ~ ", cov_formula))
  
  # create empty partial correlation and significance matrices
  mat <- data.frame(matrix(NA, 
                           nrow=length(clinical_measures), 
                           ncol=length(digital_measures) 
  )
  )
  rownames(mat) = c(clinical_measures)
  colnames(mat) = c(digital_measures)
  pmat = mat
  
  for(j in 1:length(digital_measures)){
    
    print(digital_measures[j])
    
    # get all covariates
    if(digital_measures[j] == "minsasleepSD"){
      covdat_j = read.csv(paste0("CovData/covdat_duration_sd_", daynum, "days.csv"))
    }
    
    if(digital_measures[j] == "midsleepSD"){
      covdat_j = read.csv(paste0("CovData/covdat_midsleep_sd_", daynum, "days.csv"))
    }
    
    if(!(digital_measures[j] %in% c("minsasleepSD", "midsleepSD"))){
      covdat_j = read.csv(paste0("CovData/covdat_", digital_measures[j], ".csv"))
    }
    
    covdat_j$month = as.factor(covdat_j$month)
    dat_cov_all = merge(dat_cov, covdat_j[,c("record_id","numrecs_in_window", "days_between_visits", "month")], 
                        by="record_id", all.x=T)
    
    # merge iv and covariates
    dat_digital = digital_mean[, c("record_id", digital_measures[j])]
    colnames(dat_digital)[2] = "iv"
    dat_digital_withallcov = merge(dat_digital, dat_cov_all, by="record_id", all.x=T)
    
    for(i in 1:length(clinical_measures)){
      print(clinical_measures[i])
      
      # merge dv, iv, and covariates
      dat_clinical = clinical_mean[, c("record_id", clinical_measures[i])]
      colnames(dat_clinical)[2] = "dv"
      data = merge(dat_digital_withallcov, dat_clinical, by="record_id")
      df = na.omit(data)
      
      ## manually calculate partial correlations and p values
      # regress iv and dv on covariates
      model_x <- lm(model_x_formula, df)
      model_y <- lm(model_y_formula, df)
      
      # extract residuals
      res_x <- residuals(model_x)
      res_y <- residuals(model_y)
      
      # compute partial correlation
      partial_corr <- cor(res_x, res_y)
      
      # compute t-statistic
      n <- nrow(df)                    # Sample size
      k <- ncol(model.matrix(model_x_formula, df)) - 1  # Number of covariates
      dof <- n - k - 2                    # Degrees of freedom
      t_stat <- (partial_corr * sqrt(dof)) / sqrt(1 - partial_corr^2)
      
      # compute p-value
      p_value <- 2 * pt(-abs(t_stat), dof)
      
      mat[i,j] <- partial_corr
      pmat[i,j] <- p_value
      
    }
  }
  # get correlation and significance matrix
  rcorr_mat_list = list(r = mat, P = pmat)
  
  return(rcorr_mat_list)
  
}

GetFinalPartialCorrMatrix = function(rcorr_mat_sleep,
                                     rcorr_mat_spo2,
                                     rcorr_mat_hrv,
                                     digital_measures_rearranged,
                                     digital_names_rearranged,
                                     clinical_names){
  
  rcorr_mat_final = cbind(rcorr_mat_sleep, rcorr_mat_spo2)
  rcorr_mat_final = cbind(rcorr_mat_final, rcorr_mat_hrv)
  rcorr_mat_final = rcorr_mat_final[, digital_measures_rearranged]
  rownames(rcorr_mat_final) = clinical_names
  colnames(rcorr_mat_final) = digital_names_rearranged
  rcorr_mat_final = as.matrix(rcorr_mat_final)
  
  return(rcorr_mat_final)
  
}



# scree plot
screeplot = function(hc,  # output of hclust()
                     max_k # maximum number of groups shown in the plot
){ 
  df_scree = data.frame(cbind(height = hc$height,
                              group = length(hc$height):1)
  )
  
  gg = ggplot(df_scree, aes(x=group, y=height)) +
    geom_point() +
    geom_line() +
    xlim(0, max_k) +
    ggtitle("Scree Plot") +
    theme_classic()
  
  print(gg)
}


# Compute cluster centers
compute_cluster_centers <- function(df, 
                                    dynamic_clusters # output of cutreeDynamic()
) { 
  # Identify continuous and binary columns
  continuous_cols <- sapply(df, is.numeric)
  binary_cols <- sapply(df, is.factor)
  
  # Get the column names for continuous and binary variables
  continuous_colnames <- colnames(df)[continuous_cols]
  binary_colnames <- colnames(df)[binary_cols]
  
  centers <- tapply(seq_len(nrow(df)), 
                    dynamic_clusters, 
                    FUN = function(indices) {
                      # Calculate centers for continuous variables
                      continuous_centers <- colMeans(df[indices, continuous_cols])
                      
                      # Calculate centers for binary variables (proportion of 1s)
                      binary_centers <- colMeans(df[indices, binary_cols] == 1)
                      
                      # Return combined result
                      c(continuous_centers, binary_centers)
                    })
  
  # Combine list elements into a data frame
  centers_df <- do.call(rbind, centers)
  
  # Assign the correct column names to the result
  colnames(centers_df) <- c(continuous_colnames, binary_colnames)
  
  return(centers_df)
}

# radar plots
radarplot = function(df,
                     dynamic_clusters, # output of cutreeDynamic()
                     colors,
                     linetypes,
                     title
){
  # compute cluster centers
  centers <- compute_cluster_centers(df, dynamic_clusters)
  
  # specify inner and outer boundary of radar plots (-2 and 2)
  maxrow = rep(2,ncol(df))
  minrow = rep(-2,ncol(df))
  maxminrows = data.frame(rbind(maxrow, minrow))
  colnames(maxminrows) = colnames(centers)
  centers = rbind(maxminrows, centers)
  
  # radar plot
  radarchart(centers,
             title = title,
             #cglty = 1,       # Grid line type
             #cglcol = "gray", # Grid line color
             pcol = colors,      # Color for each line
             plwd = 2,        # Width for each line
             plty = linetypes,        # Line type for each line
             #pfcol = areas   # Color of the areas
  )     
  legend("topright",
         legend = paste("Cluster", 1:length(colors)),
         bty = "n", lty=linetypes, pch = 20, col = colors,
         text.col = "grey25", pt.cex = 2)
  
}

# Criterion profile analysis
cpa_new = function(dat_cpa,
                   dat_cpa_iv,
                   iv_names,
                   clinical_measure,
                   clinical_name,
                   family,
                   covariate_names = NULL){
  
  # set up iv and covariate formulas
  iv_formula = paste(colnames(df_cpa_iv)[-1], collapse = " + ")
  cov_formula <- paste(covariate_names, collapse = " + ")
  
  # set up model formulas
  if(is.null(covariate_names)){
    model_formula <- paste(clinical_measure, "~", iv_formula) 
  }else{
    model_formula <- paste(clinical_measure, "~", iv_formula, "+", cov_formula) 
  }
  
  # use glm() to get estimates and CIs
  mod = glm(model_formula, data=df_cpa, family = family)
  vif_results = car::vif(mod)
  print(vif_results)
  
  # compute the criterion pattern score (xc)
  k=100 # multiply regression coefficients by k to get criterion pattern scores
  b <- coef(mod)[2:(length(iv_names)+1)] # non-intercept coefficients 
  bstar <- b - mean(b)
  xc <- k*bstar 
  
  # compute standard errors for xc
  vcov_mat <- vcov(mod)[2:(length(iv_names)+1), 2:(length(iv_names)+1)]  # extract variance-covariance matrix for b
  J <- diag(length(b)) - matrix(1, nrow=length(b), ncol=length(b)) / length(b)  # centering matrix
  vcov_bstar <- J %*% vcov_mat %*% t(J) # compute variance-covariance matrix for bstar
  se_bstar <- sqrt(diag(vcov_bstar)) # compute standard errors for bstar
  se_xc <- abs(k) * se_bstar  # scaling by k
  
  # compute confidence intervals
  alpha <- 0.05  # 95% CI
  t_crit <- qt(1 - alpha/2, df = df.residual(mod))
  lower_xc <- xc - t_crit * se_xc
  upper_xc <- xc + t_crit * se_xc
  
  # p-values
  t_stat <- xc / se_xc
  p_values <- 2 * (1 - pt(abs(t_stat), df=df.residual(mod)))
  
  # create a table to store estimates, standard errors, t values, p.values, CIs
  ctable <- data.frame(cbind(
    xc,
    se_xc,
    t_stat,
    p_values,
    lower_xc,
    upper_xc)
  )
  
  colnames(ctable) = c("Estimate", "Std.Error", "t value", "p.value", "lower", "upper")    
  ctable$dv = clinical_name
  ctable$iv = iv_names
  rownames(ctable) = NULL
  
  return(ctable)
}

# ANOVA to test pattern effects
cpa_anova = function(dat_cpa_dummy,
                     dat_cpa_iv,
                     iv_names,
                     clinical_measure,
                     clinical_name,
                     family,
                     covariate_names = NULL){
  
  # set up iv and covariate formulas
  iv_formula = paste(colnames(df_cpa_iv)[-1], collapse = " + ")
  df_cpa_dummy_cov=df_cpa_dummy %>%
    select(matches(paste(covariate_names, collapse = "|"))) 
  cov_formula_dummy <- paste(colnames(df_cpa_dummy_cov), collapse = " + ")
  
  # set up model formulas
  if(is.null(covariate_names)){
    cpa_formula <- paste(clinical_measure, "~", iv_formula) 
  }else{
    cpa_formula <- paste(clinical_measure, "~", iv_formula, "+", cov_formula_dummy) 
  }
  
  mod_cpa = cpa(cpa_formula, data=df_cpa_dummy, family=family, na.action = 'na.omit')
  cat("dv:", clinical_name)
  anova(mod_cpa)
  
}


####### Archived
GetPartialCorrMatrixOld = function(dat_fitbit, 
                                   dat_symptom, 
                                   measure, 
                                   digital_measures, 
                                   clinical_measures,
                                   daynum,
                                   covariate_names = NULL){
  
  ## read in time window data
  window <- read.csv(paste0("/sbgenomics/project-files/SANDBOX_Analysis_files/Sage_Elias/windows_",
                            measure,
                            "_",
                            daynum,
                            "days_3hr_02_11_2025.csv")
  ) 
  
  # process covariates
  dat_cov <- proc_cov(dat_core, measure, daynum)
  
  # process digital data
  if(measure == "sleep"){
    digital_mean <- proc_digital_sleep(dat_fitbit, digital_measures, window)
  }else{
    digital_mean <- proc_digital_hrvspo2(dat_fitbit, digital_measures, window)
  }
  
  # process clinical data
  clinical_mean <- proc_clinical(dat_symptom, clinical_measures, window)  
  
  # set up model formulas
  cov_formula <- paste(covariate_names, collapse = " + ")
  model_x_formula <- as.formula(paste("iv ~ ", cov_formula))
  model_y_formula <- as.formula(paste("dv ~ ", cov_formula))
  
  # create empty partial correlation and significance matrices
  size <- length(digital_measures) + length(clinical_measures)
  mat <- data.frame(matrix(NA, nrow=size, ncol=size))
  rownames(mat) = c(clinical_measures, digital_measures)
  colnames(mat) = c(clinical_measures, digital_measures)
  pmat = mat
  
  for(j in 1:length(digital_measures)){
    
    print(digital_measures[j])
    
    # get all covariates
    if(digital_measures[j] == "minsasleepSD"){
      covdat_j = read.csv(paste0("CodeFromElias/covdat_duration_sd_", daynum, "days.csv"))
    }
    
    if(digital_measures[j] == "midsleepSD"){
      covdat_j = read.csv(paste0("CodeFromElias/covdat_midsleep_sd_", daynum, "days.csv"))
    }
    
    if(!(digital_measures[j] %in% c("minsasleepSD", "midsleepSD"))){
      covdat_j = read.csv(paste0("CodeFromElias/covdat_", digital_measures[j], ".csv"))
    }
    
    covdat_j$month = as.factor(covdat_j$month)
    dat_cov_all = merge(dat_cov, covdat_j[,c("record_id","numrecs_in_window", "days_between_visits", "month")], 
                        by="record_id", all.x=T)
    dat_cov_all = dat_cov_all[, c("record_id", covariate_names)]
    
    # merge digital means and covariates
    dat_digital = digital_mean[, c("record_id", digital_measures[j])]
    colnames(dat_digital)[2] = "iv"
    dat_digital_withallcov = merge(dat_digital, dat_cov_all, by="record_id", all.x=T)
    
    for(i in 1:length(clinical_measures)){
      print(clinical_measures[i])
      
      # merge clinical, digital, and covariates
      dat_clinical = clinical_mean[, c("record_id", clinical_measures[i])]
      colnames(dat_clinical)[2] = "dv"
      data = merge(dat_digital_withallcov, dat_clinical, by="record_id")
      df = na.omit(data)
      
      ## manually calculate partial correlations and p values
      # regress iv and dv on covariates
      model_x <- lm(model_x_formula, df)
      model_y <- lm(model_y_formula, df)
      
      # extract residuals
      res_x <- residuals(model_x)
      res_y <- residuals(model_y)
      
      # compute partial correlation
      partial_corr <- cor(res_x, res_y)
      
      # compute t-statistic
      n <- nrow(df)                    # Sample size
      k <- ncol(model.matrix(model_x_formula, df)) - 1  # Number of covariates
      dof <- n - k - 2                    # Degrees of freedom
      t_stat <- (partial_corr * sqrt(dof)) / sqrt(1 - partial_corr^2)
      
      # compute p-value
      p_value <- 2 * pt(-abs(t_stat), dof)
      
      mat[i,length(clinical_measures)+j] <- partial_corr
      pmat[i,length(clinical_measures)+j] <- p_value
      
      ############## Alternative: use pcor.test() but it warns singular variance-covariance matrix
      # # convert categorical variables into dummy variables
      # cat_cov_list = c("biosex", "race_unique_an", "bmi", "smoking", "alcohol", "month")
      # num_cov_list  = c("age_enroll", "numrecs_in_window", "days_between_visits")
      # cat_cov = intersect(covariate_names, cat_cov_list)
      # num_cov = intersect(covariate_names, num_cov_list)
      # formula_str <- paste("~", paste(cat_cov, collapse = " + "))  # Exclude intercept
      # cat_cov_dummies <- model.matrix(as.formula(formula_str), df)[,-1]
      # all_cov <- cbind(df[, num_cov], cat_cov_dummies)
      # 
      # # get partial correlation and significance matrix
      # mat[i,length(clinical_measures)+j] = as.numeric(pcor.test(df$dv, df$iv, as.matrix(all_cov))$estimate)
      # pmat[i,length(clinical_measures)+j] = as.numeric(pcor.test(df$dv, df$iv, as.matrix(all_cov))$p.value)
      
    }
  }
  # get correlation and significance matrix
  rcorr_mat_list = list(r = mat, P = pmat)
  
  return(rcorr_mat_list)
  
}



GetFinalPartialCorrMatrixOld = function(rcorr_mat_sleep,
                                        rcorr_mat_hrv,
                                        rcorr_mat_spo2,
                                        sleep_measures,
                                        clinical_measures,
                                        all_measures,
                                        all_names,
                                        customize_names){
  
  m = nrow(rcorr_mat_sleep)
  m_hrv = nrow(rcorr_mat_hrv)
  m_spo2 = nrow(rcorr_mat_spo2)
  
  rcorr_mat = cbind(rcorr_mat_sleep, 
                    c(rcorr_mat_hrv[1:(m_hrv-2), (m_hrv-1)], rep(NA, length(sleep_measures))),
                    c(rcorr_mat_hrv[1:(m_hrv-2), m_hrv], rep(NA, length(sleep_measures))),
                    c(rcorr_mat_spo2[1:(m_spo2-1), m_spo2], rep(NA, length(sleep_measures)))
  )
  
  rcorr_mat_final = data.frame(rbind(rcorr_mat, 
                                     rep(NA, m+3),
                                     rep(NA, m+3),
                                     rep(NA, m+3)
  )
  )
  
  colnames(rcorr_mat_final)[(m+1):(m+3)] = c("hrv", "restinghr", "spo2avg")
  rownames(rcorr_mat_final)[(m+1):(m+3)] = c("hrv", "restinghr", "spo2avg")
  
  rcorr_mat_final = rcorr_mat_final[, all_measures]
  
  rownames(rcorr_mat_final) = all_names
  colnames(rcorr_mat_final) = customize_names
  
  rcorr_mat_final = as.matrix(rcorr_mat_final)
  
  return(rcorr_mat_final)
  
}



GetCorrMatrixOld = function(dat_fitbit, 
                            dat_symptom, 
                            measure, 
                            digital_measures, 
                            clinical_measures, 
                            daynum){
  
  ## read in time window data
  window <- read.csv(paste0("/sbgenomics/project-files/SANDBOX_Analysis_files/Sage_Elias/windows_",
                            measure,
                            "_",
                            daynum,
                            "days_3hr_02_11_2025.csv")
  ) 
  
  # process digital data
  if(measure == "sleep"){
    digital_mean <- proc_digital_sleep(dat_fitbit, digital_measures, window)
  }else{
    digital_mean <- proc_digital_hrvspo2(dat_fitbit, digital_measures, window)
  }
  
  # process clinical data
  clinical_mean <- proc_clinical(dat_symptom, clinical_measures, window)  
  
  # merge digital and clinical data
  df = merge(clinical_mean, digital_mean, by="record_id")
  
  # get correlation and significance matrix
  rcorr_mat_list <- Hmisc::rcorr(as.matrix(df[,-1]))
  
  return(rcorr_mat_list)
  
}



GetFinalCorrMatrixOld = function(rcorr_mat_sleep,
                                 rcorr_mat_hrv,
                                 rcorr_mat_spo2,
                                 sleep_measures,
                                 clinical_measures,
                                 all_measures,
                                 all_names,
                                 customize_names){
  
  m = nrow(rcorr_mat_sleep)
  k = length(clinical_measures)
  rcorr_mat_sleep[, 1:k] <- NA 
  rcorr_mat_sleep[(k+1):m, ] <- NA
  
  m_hrv = nrow(rcorr_mat_hrv)
  m_spo2 = nrow(rcorr_mat_spo2)
  
  rcorr_mat = cbind(rcorr_mat_sleep, 
                    c(rcorr_mat_hrv[1:(m_hrv-2), (m_hrv-1)], rep(NA, length(sleep_measures))),
                    c(rcorr_mat_hrv[1:(m_hrv-2), m_hrv], rep(NA, length(sleep_measures))),
                    c(rcorr_mat_spo2[1:(m_spo2-1), m_spo2], rep(NA, length(sleep_measures)))
  )
  
  rcorr_mat_final = data.frame(rbind(rcorr_mat, 
                                     rep(NA, m+3),
                                     rep(NA, m+3),
                                     rep(NA, m+3)
  )
  )
  
  colnames(rcorr_mat_final)[(m+1):(m+3)] = c("hrv", "restinghr", "spo2avg")
  rownames(rcorr_mat_final)[(m+1):(m+3)] = c("hrv", "restinghr", "spo2avg")
  
  rcorr_mat_final = rcorr_mat_final[, all_measures]
  
  rownames(rcorr_mat_final) = all_names
  colnames(rcorr_mat_final) = customize_names
  
  rcorr_mat_final = as.matrix(rcorr_mat_final)
  
  return(rcorr_mat_final)
  
}