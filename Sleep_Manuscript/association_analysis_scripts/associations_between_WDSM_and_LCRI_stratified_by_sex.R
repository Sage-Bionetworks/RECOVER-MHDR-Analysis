
setwd("~/sleep_paper/analyses")

source("utility_functions_for_sleep_paper.R")

RunStratLogisticRegrSex <- function(dat_clinical, 
                                    dat_digital,
                                    dat_covariates,
                                    measure_names,
                                    covariate_names,
                                    dhd_covariate_names,
                                    window_size = 182,
                                    ids_to_keep_list = NULL,
                                    scale_covariates = FALSE,
                                    sex) {
  
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
    adat <- merge(adat, dat_covariates[, c("record_id", "biosex", covariate_names), drop = FALSE], by = "record_id")
    
    adat <- adat[, c("record_id", 
                     "pasc_group", 
                     "pasc_recoded", 
                     "digital_measure",
                     "biosex",
                     model_covariates)]
    
    sex_strata <- which(adat$biosex == sex)
    adat <- adat[sex_strata,]
    
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


RunStratLogisticRegrSDSex <- function(dat_clinical, 
                                      dat_digital,
                                      dat_covariates,
                                      measure_name,
                                      covariate_names,
                                      dhd_covariate_names,
                                      window_size = 182,
                                      ids_to_keep_list = NULL,
                                      scale_covariates = FALSE,
                                      sex) {
  
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
    adat <- merge(adat, dat_covariates[, c("record_id", "biosex", covariate_names), drop = FALSE], by = "record_id")
    
    adat <- adat[, c("record_id", 
                     "pasc_group", 
                     "pasc_recoded", 
                     "digital_measure",
                     "biosex",
                     model_covariates)]
    
    sex_strata <- which(adat$biosex == sex)
    adat <- adat[sex_strata,]
    
    outputs[i, "total n"] <- nrow(adat)
    
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


##############################################################
## fit logistic regression models
##############################################################

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

m1_logreg_sleep_f <- RunStratLogisticRegrSex(dat_clinical = dat_visit, 
                                             dat_digital = dat_fitbit,
                                             dat_covariates = dat_cov,
                                             measure_names = sleep_variables, 
                                             covariate_names = NULL,
                                             dhd_covariate_names = NULL,
                                             window_size = 182,
                                             ids_to_keep_list = ids_to_keep_list,
                                             scale_covariates = scale_covariates,
                                             sex = "female")

m1_logreg_sleep_m <- RunStratLogisticRegrSex(dat_clinical = dat_visit, 
                                             dat_digital = dat_fitbit,
                                             dat_covariates = dat_cov,
                                             measure_names = sleep_variables, 
                                             covariate_names = NULL,
                                             dhd_covariate_names = NULL,
                                             window_size = 182,
                                             ids_to_keep_list = ids_to_keep_list,
                                             scale_covariates = scale_covariates,
                                             sex = "male")

dat <- read.csv("/sbgenomics/project-files/SANDBOX_Analysis_files/SAGE_Megha/Derived_Measures/Sleep_SD/sliding26weeks_stats_midsleep.csv")
wdat <- read.csv(paste0(path_elias, file_name_windows_sleep_5days))

dat_midsleep_sd <- GrabStandardDeviationData(dat, wdat)

m1_logreg_midsleep_sd_f <- RunStratLogisticRegrSDSex(dat_clinical = dat_visit, 
                                                     dat_digital = dat_midsleep_sd,
                                                     dat_covariates = dat_cov,
                                                     measure_name = "midsleep_sd",
                                                     covariate_names = NULL,
                                                     dhd_covariate_names = NULL,
                                                     window_size = 182,
                                                     ids_to_keep_list = ids_to_keep_list,
                                                     scale_covariates = scale_covariates,
                                                     sex = "female")

m1_logreg_midsleep_sd_m <- RunStratLogisticRegrSDSex(dat_clinical = dat_visit, 
                                                     dat_digital = dat_midsleep_sd,
                                                     dat_covariates = dat_cov,
                                                     measure_name = "midsleep_sd",
                                                     covariate_names = NULL,
                                                     dhd_covariate_names = NULL,
                                                     window_size = 182,
                                                     ids_to_keep_list = ids_to_keep_list,
                                                     scale_covariates = scale_covariates,
                                                     sex = "male")


dat <- read.csv("/sbgenomics/project-files/SANDBOX_Analysis_files/SAGE_Megha/Derived_Measures/Sleep_SD/sliding26weeks_stats_duration.csv")
wdat <- read.csv(paste0(path_elias, file_name_windows_sleep_5days))

dat_duration_sd <- GrabStandardDeviationData(dat, wdat)

m1_logreg_duration_sd_f <- RunStratLogisticRegrSDSex(dat_clinical = dat_visit, 
                                                     dat_digital = dat_duration_sd,
                                                     dat_covariates = dat_cov,
                                                     measure_name = "duration_sd",
                                                     covariate_names = NULL,
                                                     dhd_covariate_names = NULL,
                                                     window_size = 182,
                                                     ids_to_keep_list = ids_to_keep_list,
                                                     scale_covariates = scale_covariates,
                                                     sex = "female")

m1_logreg_duration_sd_m <- RunStratLogisticRegrSDSex(dat_clinical = dat_visit, 
                                                     dat_digital = dat_duration_sd,
                                                     dat_covariates = dat_cov,
                                                     measure_name = "duration_sd",
                                                     covariate_names = NULL,
                                                     dhd_covariate_names = NULL,
                                                     window_size = 182,
                                                     ids_to_keep_list = ids_to_keep_list,
                                                     scale_covariates = scale_covariates,
                                                     sex = "male")


hr_variables <- c("restinghr", "hrv")
dat <- read.csv(paste0(path_mahmud, file_name_hrv_5days))
ids <- unique(dat$ParticipantIdentifier)
ids_to_keep_list <- vector(mode = "list", length = length(hr_variables))
for (i in seq(length(hr_variables))) {
  ids_to_keep_list[[i]] <- ids
}

m1_logreg_hr_f <- RunStratLogisticRegrSex(dat_clinical = dat_visit, 
                                          dat_digital = dat_fitbit,
                                          dat_covariates = dat_cov,
                                          measure_names = hr_variables, 
                                          covariate_names = NULL,
                                          dhd_covariate_names = NULL,
                                          window_size = 182,
                                          ids_to_keep_list = ids_to_keep_list,
                                          scale_covariates = scale_covariates,
                                          sex = "female")

m1_logreg_hr_m <- RunStratLogisticRegrSex(dat_clinical = dat_visit, 
                                          dat_digital = dat_fitbit,
                                          dat_covariates = dat_cov,
                                          measure_names = hr_variables, 
                                          covariate_names = NULL,
                                          dhd_covariate_names = NULL,
                                          window_size = 182,
                                          ids_to_keep_list = ids_to_keep_list,
                                          scale_covariates = scale_covariates,
                                          sex = "male")


spo2_variables <- c("spo2avg")
dat <- read.csv(paste0(path_mahmud, file_name_spo2_5days))
ids <- unique(dat$ParticipantIdentifier)
ids_to_keep_list <- vector(mode = "list", length = length(spo2_variables))
for (i in seq(length(spo2_variables))) {
  ids_to_keep_list[[i]] <- ids
}

m1_logreg_spo2_f <- RunStratLogisticRegrSex(dat_clinical = dat_visit, 
                                            dat_digital = dat_fitbit,
                                            dat_covariates = dat_cov,
                                            measure_names = spo2_variables, 
                                            covariate_names = NULL,
                                            dhd_covariate_names = NULL,
                                            window_size = 182,
                                            ids_to_keep_list = ids_to_keep_list,
                                            scale_covariates = scale_covariates,
                                            sex = "female")

m1_logreg_spo2_m <- RunStratLogisticRegrSex(dat_clinical = dat_visit, 
                                            dat_digital = dat_fitbit,
                                            dat_covariates = dat_cov,
                                            measure_names = spo2_variables, 
                                            covariate_names = NULL,
                                            dhd_covariate_names = NULL,
                                            window_size = 182,
                                            ids_to_keep_list = ids_to_keep_list,
                                            scale_covariates = scale_covariates,
                                            sex = "male")


###########################################################
###########################################################
###########################################################

m1_logreg_f <- rbind(m1_logreg_sleep_f$outputs, 
                     m1_logreg_midsleep_sd_f$outputs,
                     m1_logreg_duration_sd_f$outputs,
                     m1_logreg_hr_f$outputs, 
                     m1_logreg_spo2_f$outputs)

m1_logreg_m <- rbind(m1_logreg_sleep_m$outputs, 
                     m1_logreg_midsleep_sd_m$outputs,
                     m1_logreg_duration_sd_m$outputs,
                     m1_logreg_hr_m$outputs, 
                     m1_logreg_spo2_m$outputs)

logreg_n <- cbind(m1_logreg_f[, c(1, 3)], m1_logreg_m[, 3])
colnames(logreg_n) <- c("digital measure", "female", "male")


mt_method <- "BH"

m1_logreg_f$apval <- p.adjust(m1_logreg_f$test_pval, method = mt_method)
m1_logreg_m$apval <- p.adjust(m1_logreg_m$test_pval, method = mt_method)

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


eff_f <- CreateEffectSizeTableLR(m_sleep = m1_logreg_sleep_f,
                                 m_midsleep_sd = m1_logreg_midsleep_sd_f,
                                 m_duration_sd = m1_logreg_duration_sd_f,
                                 m_hr = m1_logreg_hr_f,
                                 m_spo2 = m1_logreg_spo2_f,
                                 adjusted_pvals = m1_logreg_f$apval,
                                 ordered_variables = ordered_variables)

eff_m <- CreateEffectSizeTableLR(m_sleep = m1_logreg_sleep_m,
                                 m_midsleep_sd = m1_logreg_midsleep_sd_m,
                                 m_duration_sd = m1_logreg_duration_sd_m,
                                 m_hr = m1_logreg_hr_m,
                                 m_spo2 = m1_logreg_spo2_m,
                                 adjusted_pvals = m1_logreg_m$apval,
                                 ordered_variables = ordered_variables)

eff_f$model <- rep("female", 14)
eff_m$model <- rep("male", 14)

resall <- rbind(eff_f, eff_m)

library(ggplot2)

resall$iv <- factor(resall$iv, levels = ordered_variables)
resall$model <- factor(resall$model, levels = c("female", "male"))
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
  scale_x_discrete(labels = c("female", "male")) + 
  labs(x = paste("Outcome:", "Long COVID Research Index status"), 
       y = "Estimate (in log odds scale)" 
  ) +
  scale_y_continuous(limits = c(-1.2, 1.2),
                     name = "Estimate (log odds scale)",
                     breaks = y_left_ticks, 
                     sec.axis = sec_axis(~ exp(.), 
                                         name = "Estimate (odds scale)",
                                         breaks = y_right_ticks)
  ) +
  theme_classic()


#pdf("logistic_regression_effects_strat_sex.pdf", width = 9.5, height = 7)
print(gg)
#dev.off()

