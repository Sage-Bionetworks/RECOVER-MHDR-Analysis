
setwd("~/sleep_paper/analyses")

source("utility_functions_for_sleep_paper.R")

visit_data_version <- "/sbgenomics/project-files/RECOVERAdult_Data_2024.12/RECOVERAdult_BiostatsDerived_20241205/visits/visits_20241206.csv"
fitbit_data_version <- "/sbgenomics/project-files/RECOVERAdult_Data_2024.12/RECOVERAdult_i2b2_fitbit.tsv"
core_data_version <- "/sbgenomics/project-files/RECOVERAdult_Data_2024.12/RECOVERAdult_BiostatsDerived_20241205/core/core_proc_20241206.csv"

## filtering results
path_mahmud <- "/sbgenomics/project-files/SANDBOX_Analysis_files/Sage_Mahmud/"
file_name_sleep_14days <- "final_matched_data_consecutive_14days_3hr_updated_V1_20250210.csv"
file_name_hrv_14days <- "final_matched_data_14days_3hr_hrv_updated_V1_20250210.csv"
file_name_spo2_14days <- "final_matched_data_14days_3hr_sp02_updated_V1_20250210.csv"

## covariates and windows files
path_elias <- "/sbgenomics/project-files/SANDBOX_Analysis_files/Sage_Elias/"
## covariates
file_name_covariates_sleep_14days <- "covariates_sleep_14days_3hr_02_11_2025.csv"
file_name_covariates_hrv_14days <- "covariates_hrv_14days_3hr_02_11_2025.csv"
file_name_covariates_spo2_14days <- "covariates_spo2_14days_3hr_02_11_2025.csv"
## windows
file_name_windows_sleep_14days <- "windows_sleep_14days_3hr_02_11_2025.csv"


cov_nms_1 <- NULL
dhd_cov_nms_1 <- NULL

cov_nms_2 <- c("biosex", "age_enroll", "race_unique_an")
dhd_cov_nms_2 <- NULL

cov_nms_3 <- c("biosex", "age_enroll", "race_unique_an", "bmi")
dhd_cov_nms_3 <- NULL

cov_nms_4 <- c("biosex", "age_enroll", "race_unique_an", "bmi", "alcohol", "smoking")
dhd_cov_nms_4 <- NULL

cov_nms_5 <- c("biosex", "age_enroll", "race_unique_an", "bmi", "alcohol", "smoking")
dhd_cov_nms_5 <- c("month")

cov_nms_6 <- c("biosex", "age_enroll", "race_unique_an", "bmi", "alcohol", "smoking")
dhd_cov_nms_6 <- c("month", "numrecs_in_window", "days_between_visits")

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
## sleep variables, 14days
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
dat <- read.csv(paste0(path_mahmud, file_name_sleep_14days))
ids <- unique(dat$ParticipantIdentifier)
ids_to_keep_list <- vector(mode = "list", length = length(sleep_variables))
for (i in seq(length(sleep_variables))) {
  ids_to_keep_list[[i]] <- ids
}

## load covariate data
cov_dat <- read.csv(paste0(path_elias, file_name_covariates_sleep_14days))
dat_cov <- merge(dat_core, cov_dat, by = "record_id")
dat_cov <- CategorizeBMI(dat_cov)

m1_sleep_14days <- RunRegr(dat_clinical = dat_visit, 
                          dat_digital = dat_fitbit,
                          dat_covariates = dat_cov,
                          measure_names = sleep_variables, 
                          covariate_names = cov_nms_1,
                          dhd_covariate_names = dhd_cov_nms_1,
                          window_size = 182,
                          ids_to_keep_list = ids_to_keep_list,
                          scale_covariates = scale_covariates)

m2_sleep_14days <- RunRegr(dat_clinical = dat_visit, 
                          dat_digital = dat_fitbit,
                          dat_covariates = dat_cov,
                          measure_names = sleep_variables, 
                          covariate_names = cov_nms_2,
                          dhd_covariate_names = dhd_cov_nms_2,
                          window_size = 182,
                          ids_to_keep_list = ids_to_keep_list,
                          scale_covariates = scale_covariates)

m3_sleep_14days <- RunRegr(dat_clinical = dat_visit, 
                          dat_digital = dat_fitbit,
                          dat_covariates = dat_cov,
                          measure_names = sleep_variables, 
                          covariate_names = cov_nms_3,
                          dhd_covariate_names = dhd_cov_nms_3,
                          window_size = 182,
                          ids_to_keep_list = ids_to_keep_list,
                          scale_covariates = scale_covariates)

m6_sleep_14days <- RunRegr(dat_clinical = dat_visit, 
                          dat_digital = dat_fitbit,
                          dat_covariates = dat_cov,
                          measure_names = sleep_variables, 
                          covariate_names = cov_nms_6,
                          dhd_covariate_names = dhd_cov_nms_6,
                          window_size = 182,
                          ids_to_keep_list = ids_to_keep_list,
                          scale_covariates = scale_covariates)

dat <- read.csv("/sbgenomics/project-files/SANDBOX_Analysis_files/SAGE_Megha/Derived_Measures/Sleep_SD/sliding26weeks_stats_midsleep.csv")
wdat <- read.csv(paste0(path_elias, file_name_windows_sleep_14days))

dat_midsleep_sd <- GrabStandardDeviationData(dat, wdat)

m1_midsleep_sd_14days <- RunRegrSD(dat_clinical = dat_visit, 
                                  dat_digital = dat_midsleep_sd,
                                  dat_covariates = dat_cov,
                                  measure_name = "midsleep_sd",
                                  covariate_names = cov_nms_1,
                                  dhd_covariate_names = dhd_cov_nms_1,
                                  window_size = 182,
                                  ids_to_keep_list = ids_to_keep_list,
                                  scale_covariates = scale_covariates)

m2_midsleep_sd_14days <- RunRegrSD(dat_clinical = dat_visit, 
                                  dat_digital = dat_midsleep_sd,
                                  dat_covariates = dat_cov,
                                  measure_name = "midsleep_sd",
                                  covariate_names = cov_nms_2,
                                  dhd_covariate_names = dhd_cov_nms_2,
                                  window_size = 182,
                                  ids_to_keep_list = ids_to_keep_list,
                                  scale_covariates = scale_covariates)

m3_midsleep_sd_14days <- RunRegrSD(dat_clinical = dat_visit, 
                                  dat_digital = dat_midsleep_sd,
                                  dat_covariates = dat_cov,
                                  measure_name = "midsleep_sd",
                                  covariate_names = cov_nms_3,
                                  dhd_covariate_names = dhd_cov_nms_3,
                                  window_size = 182,
                                  ids_to_keep_list = ids_to_keep_list,
                                  scale_covariates = scale_covariates)

m6_midsleep_sd_14days <- RunRegrSD(dat_clinical = dat_visit, 
                                  dat_digital = dat_midsleep_sd,
                                  dat_covariates = dat_cov,
                                  measure_name = "midsleep_sd",
                                  covariate_names = cov_nms_6,
                                  dhd_covariate_names = dhd_cov_nms_6,
                                  window_size = 182,
                                  ids_to_keep_list = ids_to_keep_list,
                                  scale_covariates = scale_covariates)

dat <- read.csv("/sbgenomics/project-files/SANDBOX_Analysis_files/SAGE_Megha/Derived_Measures/Sleep_SD/sliding26weeks_stats_duration.csv")
wdat <- read.csv(paste0(path_elias, file_name_windows_sleep_14days))

dat_duration_sd <- GrabStandardDeviationData(dat, wdat)

m1_duration_sd_14days <- RunRegrSD(dat_clinical = dat_visit, 
                                  dat_digital = dat_duration_sd,
                                  dat_covariates = dat_cov,
                                  measure_name = "duration_sd",
                                  covariate_names = cov_nms_1,
                                  dhd_covariate_names = dhd_cov_nms_1,
                                  window_size = 182,
                                  ids_to_keep_list = ids_to_keep_list,
                                  scale_covariates = scale_covariates)

m2_duration_sd_14days <- RunRegrSD(dat_clinical = dat_visit, 
                                  dat_digital = dat_duration_sd,
                                  dat_covariates = dat_cov,
                                  measure_name = "duration_sd",
                                  covariate_names = cov_nms_2,
                                  dhd_covariate_names = dhd_cov_nms_2,
                                  window_size = 182,
                                  ids_to_keep_list = ids_to_keep_list,
                                  scale_covariates = scale_covariates)

m3_duration_sd_14days <- RunRegrSD(dat_clinical = dat_visit, 
                                  dat_digital = dat_duration_sd,
                                  dat_covariates = dat_cov,
                                  measure_name = "duration_sd",
                                  covariate_names = cov_nms_3,
                                  dhd_covariate_names = dhd_cov_nms_3,
                                  window_size = 182,
                                  ids_to_keep_list = ids_to_keep_list,
                                  scale_covariates = scale_covariates)

m6_duration_sd_14days <- RunRegrSD(dat_clinical = dat_visit, 
                                  dat_digital = dat_duration_sd,
                                  dat_covariates = dat_cov,
                                  measure_name = "duration_sd",
                                  covariate_names = cov_nms_6,
                                  dhd_covariate_names = dhd_cov_nms_6,
                                  window_size = 182,
                                  ids_to_keep_list = ids_to_keep_list,
                                  scale_covariates = scale_covariates)

##############################################################
## hr variables, 14days
##############################################################

hr_variables <- c("restinghr", "hrv")

dat <- read.csv(paste0(path_mahmud, file_name_hrv_14days))
ids <- unique(dat$ParticipantIdentifier)
ids_to_keep_list <- vector(mode = "list", length = length(hr_variables))
for (i in seq(length(hr_variables))) {
  ids_to_keep_list[[i]] <- ids
}

cov_dat <- read.csv(paste0(path_elias, file_name_covariates_hrv_14days))
dat_cov <- merge(dat_core, cov_dat, by = "record_id")
dat_cov <- CategorizeBMI(dat_cov)

m1_hr_14days <- RunRegr(dat_clinical = dat_visit, 
                       dat_digital = dat_fitbit,
                       dat_covariates = dat_cov,
                       measure_names = hr_variables, 
                       covariate_names = cov_nms_1,
                       dhd_covariate_names = dhd_cov_nms_1,
                       window_size = 182,
                       ids_to_keep_list = ids_to_keep_list,
                       scale_covariates = scale_covariates)

m2_hr_14days <- RunRegr(dat_clinical = dat_visit, 
                       dat_digital = dat_fitbit,
                       dat_covariates = dat_cov,
                       measure_names = hr_variables, 
                       covariate_names = cov_nms_2,
                       dhd_covariate_names = dhd_cov_nms_2,
                       window_size = 182,
                       ids_to_keep_list = ids_to_keep_list,
                       scale_covariates = scale_covariates)

m3_hr_14days <- RunRegr(dat_clinical = dat_visit, 
                       dat_digital = dat_fitbit,
                       dat_covariates = dat_cov,
                       measure_names = hr_variables, 
                       covariate_names = cov_nms_3,
                       dhd_covariate_names = dhd_cov_nms_3,
                       window_size = 182,
                       ids_to_keep_list = ids_to_keep_list,
                       scale_covariates = scale_covariates)

m6_hr_14days <- RunRegr(dat_clinical = dat_visit, 
                       dat_digital = dat_fitbit,
                       dat_covariates = dat_cov,
                       measure_names = hr_variables, 
                       covariate_names = cov_nms_6,
                       dhd_covariate_names = dhd_cov_nms_6,
                       window_size = 182,
                       ids_to_keep_list = ids_to_keep_list,
                       scale_covariates = scale_covariates)


##############################################################
## spo2 variables, 14days
##############################################################

spo2_variables <- c("spo2avg")

dat <- read.csv(paste0(path_mahmud, file_name_spo2_14days))
ids <- unique(dat$ParticipantIdentifier)
ids_to_keep_list <- vector(mode = "list", length = length(spo2_variables))
for (i in seq(length(spo2_variables))) {
  ids_to_keep_list[[i]] <- ids
}

cov_dat <- read.csv(paste0(path_elias, file_name_covariates_spo2_14days))
dat_cov <- merge(dat_core, cov_dat, by = "record_id")
dat_cov <- CategorizeBMI(dat_cov)

m1_spo2_14days <- RunRegr(dat_clinical = dat_visit, 
                         dat_digital = dat_fitbit,
                         dat_covariates = dat_cov,
                         measure_names = spo2_variables, 
                         covariate_names = cov_nms_1,
                         dhd_covariate_names = dhd_cov_nms_1,
                         window_size = 182,
                         ids_to_keep_list = ids_to_keep_list,
                         scale_covariates = scale_covariates)

m2_spo2_14days <- RunRegr(dat_clinical = dat_visit, 
                         dat_digital = dat_fitbit,
                         dat_covariates = dat_cov,
                         measure_names = spo2_variables, 
                         covariate_names = cov_nms_2,
                         dhd_covariate_names = dhd_cov_nms_2,
                         window_size = 182,
                         ids_to_keep_list = ids_to_keep_list,
                         scale_covariates = scale_covariates)

m3_spo2_14days <- RunRegr(dat_clinical = dat_visit, 
                         dat_digital = dat_fitbit,
                         dat_covariates = dat_cov,
                         measure_names = spo2_variables, 
                         covariate_names = cov_nms_3,
                         dhd_covariate_names = dhd_cov_nms_3,
                         window_size = 182,
                         ids_to_keep_list = ids_to_keep_list,
                         scale_covariates = scale_covariates)

m6_spo2_14days <- RunRegr(dat_clinical = dat_visit, 
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

m1_14days <- rbind(m1_sleep_14days$outputs, 
                  m1_midsleep_sd_14days$outputs,
                  m1_duration_sd_14days$outputs,
                  m1_hr_14days$outputs, 
                  m1_spo2_14days$outputs)

m2_14days <- rbind(m2_sleep_14days$outputs,
                  m2_midsleep_sd_14days$outputs,
                  m2_duration_sd_14days$outputs,
                  m2_hr_14days$outputs, 
                  m2_spo2_14days$outputs)

m3_14days <- rbind(m3_sleep_14days$outputs, 
                  m3_midsleep_sd_14days$outputs,
                  m3_duration_sd_14days$outputs,
                  m3_hr_14days$outputs, 
                  m3_spo2_14days$outputs)

m6_14days <- rbind(m6_sleep_14days$outputs,
                  m6_midsleep_sd_14days$outputs,
                  m6_duration_sd_14days$outputs,
                  m6_hr_14days$outputs, 
                  m6_spo2_14days$outputs)


mt_method <- "BH"

m1_14days$apval <- p.adjust(m1_14days$test_pval, method = mt_method)

m2_14days$apval <- p.adjust(m2_14days$test_pval, method = mt_method)

m3_14days$apval <- p.adjust(m3_14days$test_pval, method = mt_method)

m6_14days$apval <- p.adjust(m6_14days$test_pval, method = mt_method)

nms <- m1_14days$digital_measure

digital_measure_pvals_14days <- vector(mode = "list", length = length(nms))
names(digital_measure_pvals_14days) <- nms

for (i in seq(length(nms))) {
  aux5 <- c(m1_14days[i, "apval"],
            m2_14days[i, "apval"],
            m3_14days[i, "apval"],
            m6_14days[i, "apval"])
  names(aux5) <- paste0("Model ", c(1, 2, 3, 6))
  digital_measure_pvals_14days[[i]] <- aux5
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

apvals1 <- unlist(lapply(digital_measure_pvals_14days, function(x) x[1]))
eff1 <- CreateEffectSizeTableRegr(m_sleep = m1_sleep_14days,
                                m_midsleep_sd = m1_midsleep_sd_14days,
                                m_duration_sd = m1_duration_sd_14days,
                                m_hr = m1_hr_14days,
                                m_spo2 = m1_spo2_14days,
                                adjusted_pvals = apvals1,
                                ordered_variables = ordered_variables)

apvals2 <- unlist(lapply(digital_measure_pvals_14days, function(x) x[2]))
eff2 <- CreateEffectSizeTableRegr(m_sleep = m2_sleep_14days,
                                  m_midsleep_sd = m2_midsleep_sd_14days,
                                  m_duration_sd = m2_duration_sd_14days,
                                  m_hr = m2_hr_14days,
                                  m_spo2 = m2_spo2_14days,
                                  adjusted_pvals = apvals2,
                                  ordered_variables = ordered_variables)

apvals3 <- unlist(lapply(digital_measure_pvals_14days, function(x) x[3]))
eff3 <- CreateEffectSizeTableRegr(m_sleep = m3_sleep_14days,
                                  m_midsleep_sd = m3_midsleep_sd_14days,
                                  m_duration_sd = m3_duration_sd_14days,
                                  m_hr = m3_hr_14days,
                                  m_spo2 = m3_spo2_14days,
                                  adjusted_pvals = apvals3,
                                  ordered_variables = ordered_variables)

apvals6 <- unlist(lapply(digital_measure_pvals_14days, function(x) x[4]))
eff6 <- CreateEffectSizeTableRegr(m_sleep = m6_sleep_14days,
                                  m_midsleep_sd = m6_midsleep_sd_14days,
                                  m_duration_sd = m6_duration_sd_14days,
                                  m_hr = m6_hr_14days,
                                  m_spo2 = m6_spo2_14days,
                                  adjusted_pvals = apvals6,
                                  ordered_variables = ordered_variables)

eff1$model <- rep("m1", 14)
eff2$model <- rep("m2", 14)
eff3$model <- rep("m3", 14)
eff6$model <- rep("m6", 14)

resall <- rbind(eff1, eff2, eff3, eff6)

library(ggplot2)

resall$iv <- factor(resall$iv, levels = ordered_variables)
resall$model <- factor(resall$model, levels = c("m1", "m2", "m3", "full"))
resall$lower <- resall[, "lower bound (95% CI)"]
resall$upper <- resall[, "upper bound (95% CI)"]

gg = ggplot(resall,
            aes(x = factor(model), y = Estimate)
) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  geom_text(
    aes(label = sig, 
        y = ifelse(upper > 0, upper + 0.05, lower - 0.325)), 
    size = 5
  ) +
  geom_hline(yintercept = 0, 
             linetype = "dashed", color = "red") +
  facet_wrap(~ iv, ncol=5, scales = "fixed") +
  scale_x_discrete(labels = c("m1", "m2", "m3", "full")) + 
  labs(x = paste("Outcome:", "Long COVID Research Index score"), 
       y = "Estimate" 
  ) +
  theme_classic()


#pdf("linear_regression_effects_14days.pdf", width = 9.5, height = 7)
print(gg)
#dev.off()


