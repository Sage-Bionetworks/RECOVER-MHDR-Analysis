library(tidyverse)
library(ggplot2)
library(corrplot) # correlation plots 

setwd("~/SleepPaper")
source("UtilityFunctions.R")

# set up the output path
dir.create('/sbgenomics/output-files/SANDBOX_Analysis_files/')
output_path <- "/sbgenomics/output-files/SANDBOX_Analysis_files/PSU_Yanling/"
dir.create(output_path)
# specify filtering criterion (e.g., 5-day filtering)
daynum = 5 # or 14
# specify which models to run
m_select = c(1:3,6) 

#################################### selected digital/clinical measures and covariates
# 16 clinical measures (14 binary + 2 continuous) and their informative names
# Note: always put the two promis global measures at the end of the list
clinical_measures <- c("ps_sense___now", # smell/taste
                       "ps_malaise___now",
                       "ps_cough___now",
                       "ps_think___now_sev", # brain fog
                       "ps_thirst___now",
                       "ps_heart___now", # palpitations
                       "pain_chest___now_sev",
                       "ps_fatigue___now_sev",
                       "ps_goofy___now", # dizziness
                       "ps_gastro___now", 
                       "pain_head___now_sev",
                       "ps_sob___now_sev", # shortness of breath
                       "ps_sleepapnea___now", # sleep apnea
                       "ps_sleepdist___now_sev", # sleep disturbance
                       "promis_global_ph_Tscore", 
                       "promis_global_mh_Tscore")

clinical_names = c("Smell/taste", 
                   "Post-exertional malaise", 
                   "Chronic cough", 
                   "Brain fog", 
                   "Thirst", 
                   "Palpitations",
                   "Chest pain", 
                   "Fatigue", 
                   "Dizziness", 
                   "Gastrointestinal symptoms", 
                   "Head pain", 
                   "Shortness of breath", 
                   "Sleep apnea", 
                   "Sleep disturbance", 
                   "Global physical health", 
                   "Global mental health")

# 11 sleep measures (9 weekly mean measures + 2 SD measures) and their informative names
# Note: always put the two SD measures at the end of the list
sleep_measures <- c("efficiency", "minsasleep", "minsawake", 
                    "sleeplevellight", "sleepleveldeep", "sleeplevelrem",
                    "remfragmentationind", "remonsetlatency",
                    "remsleepbrthrate", 
                    "minsasleepSD", "midsleepSD")

sleep_names <- c("Sleep Efficiency", "Sleep Duration", "WASO", 
                 "Minutes in Light Sleep", "Minutes in Deep Sleep", "Minutes in REM Sleep",
                 "REM Fragmentation Index", "REM Onset Latency",
                 "REM Sleep Breathing Rate", 
                 "SD of Sleep Duration", "SD of Mid-Sleep")

# one spo2 measure 
spo2_measures <- c("spo2avg")
spo2_names <- c("SpO2")

# two hrv measures 
hrv_measures <- c("hrv", "restinghr")
hrv_names <- c("Heart Rate Variability", "Resting Heart Rate")


# covariates in models 1-6
covariate_names = list(
  NULL,                                                                               #m1 
  c("biosex", "age_enroll", "race_unique_an"),                                        #m2
  c("biosex", "age_enroll", "race_unique_an", "bmi"),                                 #m3
  c("biosex", "age_enroll", "race_unique_an", "bmi", "smoking", "alcohol"),           #m4
  c("biosex", "age_enroll", "race_unique_an", "bmi", "smoking", "alcohol", "month"),  #m5
  c("biosex", "age_enroll", "race_unique_an", "bmi", "smoking", "alcohol", "month",   #m6/full model
    "numrecs_in_window", "days_between_visits")
)


##########################################
########################################## 
##########################################

# make sure to read in correct versions of core/fitbit/visits/symptoms/windows/covariates/sliding26weeks data
core_path = "/sbgenomics/project-files/RECOVERAdult_Data_2024.12/RECOVERAdult_BiostatsDerived_20241205/core/core_proc_20241206.csv"
fitbit_path = "/sbgenomics/project-files/RECOVERAdult_Data_2024.12/RECOVERAdult_i2b2_fitbit.tsv"
visit_path = "/sbgenomics/project-files/RECOVERAdult_Data_2024.12/RECOVERAdult_BiostatsDerived_20241205/visits/visits_20241206.csv"
symptom_path = "/sbgenomics/project-files/RECOVERAdult_Data_2024.12/RECOVERAdult_BiostatsDerived_20241205/symptoms/symptoms_20241206.csv"

# read in window data
window_sleep <- read.csv(paste0("/sbgenomics/project-files/SANDBOX_Analysis_files/Sage_Elias/windows_sleep_", daynum, "days_3hr_02_11_2025.csv"))
window_spo2 <- read.csv(paste0("/sbgenomics/project-files/SANDBOX_Analysis_files/Sage_Elias/windows_spo2_", daynum, "days_3hr_02_11_2025.csv"))
window_hrv <- read.csv(paste0("/sbgenomics/project-files/SANDBOX_Analysis_files/Sage_Elias/windows_hrv_", daynum, "days_3hr_02_11_2025.csv"))

# read in covariates data
dat_cov_sleep <- read.csv(paste0("/sbgenomics/project-files/SANDBOX_Analysis_files/Sage_Elias/covariates_sleep_", daynum, "days_3hr_02_11_2025.csv")) 
dat_cov_spo2 <- read.csv(paste0("/sbgenomics/project-files/SANDBOX_Analysis_files/Sage_Elias/covariates_spo2_", daynum, "days_3hr_02_11_2025.csv")) 
dat_cov_hrv <- read.csv(paste0("/sbgenomics/project-files/SANDBOX_Analysis_files/Sage_Elias/covariates_hrv_", daynum, "days_3hr_02_11_2025.csv")) 

# read in sliding window SD data
dat_midsleep = read.csv("/sbgenomics/project-files/SANDBOX_Analysis_files/SAGE_Megha/Derived_Measures/Sleep_SD/sliding26weeks_stats_midsleep.csv")
dat_duration = read.csv("/sbgenomics/project-files/SANDBOX_Analysis_files/SAGE_Megha/Derived_Measures/Sleep_SD/sliding26weeks_stats_duration.csv")

# read in demographics data and other covariates
dat_core <- read.csv(core_path)
dat_core <- dat_core[, c("record_id", "biosex", "age_enroll", "race_unique_an")]

## recode biosex (same as Elias's)
idx_female <- which(dat_core$biosex == 1)
idx_na <- which(is.na(dat_core$biosex))
dat_core$biosex <- "male"
dat_core$biosex[idx_female] <- "female"
dat_core$biosex[idx_na] <- NA
dat_core$biosex <- as.factor(dat_core$biosex)

dat_core$race_unique_an = factor(dat_core$race_unique_an, 
                                 levels = c("Non-Hispanic White",
                                            "Hispanic",
                                            "Non-Hispanic Black",
                                            "Non-Hispanic Asian", 
                                            "Mixed race/Other/Missing")
)

## merge with other covariates (bmi, smoking, alcohol)
dat_cov_sleep = proc_cov(dat_cov_sleep)
dat_cov_spo2 = proc_cov(dat_cov_spo2)
dat_cov_hrv = proc_cov(dat_cov_hrv)
dat_cov_sleep = merge(dat_cov_sleep, dat_core, by="record_id", all.x=T)
dat_cov_spo2 = merge(dat_cov_spo2, dat_core, by="record_id", all.x=T)
dat_cov_hrv = merge(dat_cov_hrv, dat_core, by="record_id", all.x=T)

# read in the digital health data
dat_fitbit <- data.table::fread(fitbit_path, sep = '\t', header = T)
dat_fitbit$record_id <- dat_fitbit$PARTICIPANT_ID
dat_fitbit$SUMMARY_DATE <- as.Date(dat_fitbit$SUMMARY_DATE)
dat_fitbit$SUMMARY_DATE <- dat_fitbit$SUMMARY_DATE + 3  # match Elias's code
dat_fitbit$SUMMARY_VALUE <- as.numeric(dat_fitbit$SUMMARY_VALUE)


# read in the visits data
dat_visit <- read.csv(visit_path)
dat_visit$visit_dt <- as.Date(dat_visit$visit_dt)
dat_visit$pasc_jama = dat_visit$pasc_jama2024
dat_visit$pasc_score = dat_visit$pasc_score_2024

# read in the symptoms data
dat_symptom <- read.csv(symptom_path)
dat_symptom$ps_colldt <- as.Date(dat_symptom$ps_colldt)

## replace two problematic ps_colldts with NAs
dat_symptom[dat_symptom$record_id=="RA11006-00041"&dat_symptom$redcap_event_name=="followup_1_arm_1", "ps_colldt"] = NA #as.Date("2022-07-28")
dat_symptom[dat_symptom$record_id=="RA11006-00009"&dat_symptom$redcap_event_name=="followup_1_arm_1", "ps_colldt"] = NA

## remove records with missing ps_colldt
dat_symptom <- dat_symptom[!is.na(dat_symptom$ps_colldt), ]

## recode clinical measures with severity measures & convert global health measures to T-scores
dat_symptom = recode_dat_symptom(dat_symptom)

## only keep selected clinical measures and relevant variables
dat_symptom_new = dat_symptom[,c("record_id","redcap_event_name","ps_colldt",
                                 clinical_measures)] %>%
  left_join(dat_visit[,c("record_id","redcap_event_name",
                         "visit_dt", "pasc_jama", "pasc_score")], 
            by=c("record_id", "redcap_event_name"))


# get the mean of digital data
digital_mean_sleep <- proc_digital_sleep(dat_fitbit, dat_midsleep, dat_duration, sleep_measures, window_sleep)
digital_mean_spo2 <- proc_digital_hrvspo2(dat_fitbit, spo2_measures, window_spo2) 
digital_mean_hrv <- proc_digital_hrvspo2(dat_fitbit, hrv_measures, window_hrv) 

# get the mean of clinical data
clinical_mean_sleep <- proc_clinical(dat_symptom_new, clinical_measures, window_sleep)
clinical_mean_spo2 <- proc_clinical(dat_symptom_new, clinical_measures, window_spo2)
clinical_mean_hrv <- proc_clinical(dat_symptom_new, clinical_measures, window_hrv)

########################################## regression analysis (original/unscaled)
res_sleep = NULL
for(k in m_select){
  res_sleep_k = ind_reg(digital_mean_sleep,
                        clinical_mean_sleep,
                        dat_cov_sleep,
                        sleep_measures,
                        sleep_names,
                        clinical_measures,
                        clinical_names,
                        daynum,
                        covariate_names = covariate_names[[k]],
                        model = k,
                        scale=FALSE)
  res_sleep = rbind(res_sleep, res_sleep_k)
}


res_spo2 = NULL
for(k in m_select){
  res_spo2_k = ind_reg(digital_mean_spo2,
                       clinical_mean_spo2,
                       dat_cov_spo2,
                       spo2_measures,
                       spo2_names,
                       clinical_measures,
                       clinical_names,
                       daynum,
                       covariate_names = covariate_names[[k]],
                       model = k,
                       scale=FALSE)
  res_spo2 = rbind(res_spo2, res_spo2_k)
}

res_hrv = NULL
for(k in m_select){
  res_hrv_k = ind_reg(digital_mean_hrv,
                      clinical_mean_hrv,
                      dat_cov_hrv,
                      hrv_measures,
                      hrv_names,
                      clinical_measures,
                      clinical_names,
                      daynum,
                      covariate_names = covariate_names[[k]],
                      model = k,
                      scale=FALSE)
  res_hrv = rbind(res_hrv, res_hrv_k)
}

# results for all models
resall = rbind(res_sleep, res_spo2, res_hrv)

# order of IVs in tables and plots
digital_measures_rearranged = c(sleep_measures[c(1:2,5:6,9)], 
                                spo2_measures,
                                hrv_measures,
                                sleep_measures[c(10:11,8,7,4,3)])

digital_names_rearranged = c(sleep_names[c(1:2,5:6,9)], 
                             spo2_names,
                             hrv_names,
                             sleep_names[c(10:11,8,7,4,3)])


resall = resall %>%
  group_by(model) %>% # adjust p values within each model 
  mutate(p.value.adjusted = p.adjust(p.value, method = "fdr")) %>% 
  ungroup() %>%
  mutate(
    sig = case_when(
      p.value.adjusted < 0.001 ~ "***",
      p.value.adjusted < 0.01 ~ "**",
      p.value.adjusted < 0.05 ~ "*",
      TRUE ~ ""  
    )) %>%
  mutate(iv = factor(iv, levels = digital_names_rearranged) # reorder ivs 
  ) 


## save results in Seven Bridges and then convert them to GT tables on your local computer (refer to "generate_gt_tables.R")
resall_unscaled = resall
write.csv(resall_unscaled, file = paste0(output_path, "regression_results_allmodels_original.csv"))


########################################## regression analysis (scaled)
res_sleep = NULL
for(k in m_select){
  res_sleep_k = ind_reg(digital_mean_sleep,
                        clinical_mean_sleep,
                        dat_cov_sleep,
                        sleep_measures,
                        sleep_names,
                        clinical_measures,
                        clinical_names,
                        daynum,
                        covariate_names = covariate_names[[k]],
                        model = k,
                        scale=TRUE)
  res_sleep = rbind(res_sleep, res_sleep_k)
}


res_spo2 = NULL
for(k in m_select){
  res_spo2_k = ind_reg(digital_mean_spo2,
                       clinical_mean_spo2,
                       dat_cov_spo2,
                       spo2_measures,
                       spo2_names,
                       clinical_measures,
                       clinical_names,
                       daynum,
                       covariate_names = covariate_names[[k]],
                       model = k,
                       scale=TRUE)
  res_spo2 = rbind(res_spo2, res_spo2_k)
}

res_hrv = NULL
for(k in m_select){
  res_hrv_k = ind_reg(digital_mean_hrv,
                      clinical_mean_hrv,
                      dat_cov_hrv,
                      hrv_measures,
                      hrv_names,
                      clinical_measures,
                      clinical_names,
                      daynum,
                      covariate_names = covariate_names[[k]],
                      model = k,
                      scale=TRUE)
  res_hrv = rbind(res_hrv, res_hrv_k)
}

resall = rbind(res_sleep, res_spo2, res_hrv)

resall = resall %>%
  group_by(model) %>% # adjust p values within each model 
  mutate(p.value.adjusted = p.adjust(p.value, method = "fdr")) %>%
  ungroup() %>%
  mutate(
    sig = case_when(
      p.value.adjusted < 0.001 ~ "***",
      p.value.adjusted < 0.01 ~ "**",
      p.value.adjusted < 0.05 ~ "*",
      TRUE ~ ""  
    )) %>%
  mutate(iv = factor(iv, levels = digital_names_rearranged) # reorder ivs
  ) 


# save results for m2 and m3 in Seven Bridges, which will be used to generate forest plots 
resall_scaled = resall
for(k in 2:3){
  output_file = resall_scaled[resall_scaled$model==k, ]
  write.csv(output_file, file = paste0(output_path, "regression_results_model", k, "_scaled.csv"))
}


## bar plot 
## set up output path
output_path_barplots <- paste0(output_path, "bar_plots/")
dir.create(output_path_barplots)

## rename a bit
clinical_names_filename = c("Smell_taste", 
                            "Post-exertional_malaise", 
                            "Chronic_cough", 
                            "Brain_fog", 
                            "Thirst", 
                            "Palpitations",
                            "Chest_pain", 
                            "Fatigue", 
                            "Dizziness", 
                            "Gastrointestinal_symptoms", 
                            "Head_pain", 
                            "Shortness_of_breath", 
                            "Sleep_apnea", 
                            "Sleep_disturbance", 
                            "Global_physical_health", 
                            "Global_mental_health"
)

## set the positions of the tick marks on both left and right y-axes
y_left_ticks <- c(-1, -0.5, 0, 0.5, 1)
y_right_ticks <- round(exp(y_left_ticks), 2)


for(i in 1:(length(clinical_names)-2)){
  gg = ggplot(resall_scaled[resall_scaled$dv==clinical_names[i], ],
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
    labs(x = paste("Outcome:", clinical_names[i])) +
    scale_y_continuous(limits = c(-1, 1),
                       name = "Estimate (log odds scale)",
                       breaks = y_left_ticks,
                       sec.axis = sec_axis(~ exp(.),
                                           name = "Estimate (odds scale)",
                                           breaks = y_right_ticks)
    ) +
    theme_classic()
  
  pdf(paste0(output_path_barplots, "regression_plots_", clinical_names_filename[i], ".pdf"), width=10, height=6)
  print(gg)
  dev.off()
}

for(i in (length(clinical_names)-1):length(clinical_names)){
  gg = ggplot(resall_scaled[resall_scaled$dv==clinical_names[i], ],
              aes(x = factor(model), y = Estimate)
  ) +
    geom_bar(stat = "identity", width = 0.5) +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
    geom_text(
      aes(label = sig, 
          y = ifelse(upper > 0, upper + 0.02, lower - 0.05)), 
      size = 5
    ) +
    geom_hline(yintercept = 0, 
               linetype = "dashed", color = "red") +
    facet_wrap(~ iv, ncol=5, scales = "fixed") +
    scale_x_discrete(labels = c("m1", "m2", "m3", "full")) + 
    labs(x = paste("Outcome:", clinical_names[i])) +
    scale_y_continuous(limits = c(-0.4, 0.4),
                       name = "Estimate",
                       breaks = c(-0.4, -0.2, 0, 0.2, 0.4)
    ) +
    theme_classic()
  
  pdf(paste0(output_path_barplots, "regression_plots_", clinical_names_filename[i], ".pdf"), width=10, height=6)
  print(gg)
  dev.off()
}



########################################## correlation plot
col_reversed <- colorRampPalette(rev(c("#67001F", "#B2182B", "#D6604D", "#F4A582", 
                                       "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE", 
                                       "#4393C3", "#2166AC", "#053061")))(200)

# set up the output path
output_path_heatmaps <- paste0(output_path, "heatmaps/")
dir.create(output_path_heatmaps)

for(k in m_select){
  # get (partial) correlations and significance
  if(k == 1){ # model 1
    rcorr_mat_list_sleep = GetCorrMatrix(digital_mean_sleep,
                                         clinical_mean_sleep,
                                         sleep_measures, 
                                         clinical_measures)
    
    rcorr_mat_list_spo2 = GetCorrMatrix(digital_mean_spo2,
                                        clinical_mean_spo2,
                                        spo2_measures, 
                                        clinical_measures)
    
    rcorr_mat_list_hrv = GetCorrMatrix(digital_mean_hrv,
                                       clinical_mean_hrv,
                                       hrv_measures, 
                                       clinical_measures)
    
  }else{  # model 2-6 (partial correlations) 
    rcorr_mat_list_sleep = GetPartialCorrMatrix(digital_mean_sleep,
                                                clinical_mean_sleep,
                                                dat_cov_sleep, 
                                                sleep_measures,
                                                clinical_measures,
                                                daynum, 
                                                covariate_names[[k]])
    
    rcorr_mat_list_spo2 = GetPartialCorrMatrix(digital_mean_spo2,
                                               clinical_mean_spo2,
                                               dat_cov_spo2, 
                                               spo2_measures, 
                                               clinical_measures, 
                                               daynum, 
                                               covariate_names[[k]])
    
    rcorr_mat_list_hrv = GetPartialCorrMatrix(digital_mean_hrv,
                                              clinical_mean_hrv,
                                              dat_cov_hrv,
                                              hrv_measures,
                                              clinical_measures, 
                                              daynum,
                                              covariate_names[[k]])
  }
  
  # extract correlation (r) and significance matrix (P)
  rcorr_mat_sleep = rcorr_mat_list_sleep$r
  rcorr_pmat_sleep = rcorr_mat_list_sleep$P
  
  rcorr_mat_spo2 = rcorr_mat_list_spo2$r
  rcorr_pmat_spo2 = rcorr_mat_list_spo2$P
  
  rcorr_mat_hrv = rcorr_mat_list_hrv$r
  rcorr_pmat_hrv = rcorr_mat_list_hrv$P
  
  # rearrange the order of digital measures in the heatmap 
  rcorr_mat_final = GetFinalPartialCorrMatrix(rcorr_mat_sleep,
                                              rcorr_mat_spo2,
                                              rcorr_mat_hrv,
                                              digital_measures_rearranged,
                                              digital_names_rearranged,
                                              clinical_names) 
  
  rcorr_pmat_final = GetFinalPartialCorrMatrix(rcorr_pmat_sleep,
                                               rcorr_pmat_spo2,
                                               rcorr_pmat_hrv,
                                               digital_measures_rearranged,
                                               digital_names_rearranged,
                                               clinical_names) 
  
  # store adjusted p values in rcorr_pmat_adjusted  
  p.adjusted <- p.adjust(as.vector(rcorr_pmat_final), method = "fdr")
  rcorr_pmat_adjusted <- rcorr_pmat_final
  rcorr_pmat_adjusted[] <- p.adjusted
  
  # save heatmaps
  pdf(file = paste0(output_path_heatmaps, "rcorr_sig_plot_model", k, ".pdf"),   
      width = 10, 
      height = 8) 
  corrplot(rcorr_mat_final, 
           method = 'color',
           #order = 'AOE',
           is.corr = FALSE,
           tl.srt = 45,
           tl.cex = 0.8,
           tl.col = "black",
           p.mat = rcorr_pmat_adjusted,
           insig = "label_sig", 
           sig.level = 0.05, 
           pch.cex = 1, 
           pch.col = "black", 
           na.label = " ", 
           na.label.col = "white",
           col = col_reversed)
  dev.off()
  
}




