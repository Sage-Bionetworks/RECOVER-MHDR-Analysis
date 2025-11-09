install.packages(c("dynamicTreeCut","fmsb", "mclust"))
install.packages('profileR')

library(tidyverse)
library(ggplot2)
library(dynamicTreeCut) # hierarchical clustering
library(fmsb) # radar plot
library(mclust) # BIC plot
library(profileR) # CPA 
library(car) # check vif

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


########################################## cluster analysis

# selected digital measures in cluster analysis
sleep_measures_selected <- c("efficiency", "minsasleep", #"minsawake", 
                             #"sleeplevellight", 
                             "sleepleveldeep", "sleeplevelrem",
                             #"remfragmentationind", 
                             "remonsetlatency",
                             "remsleepbrthrate", 
                             "minsasleepSD"#, "midsleepSD"
)

sleep_names_selected <- c("Sleep Efficiency", "Sleep Duration", #"WASO", 
                          #"Minutes in Light Sleep", 
                          "Minutes in Deep Sleep", "Minutes in REM Sleep",
                          #"REM Fragmentation Index", 
                          "REM Onset Latency",
                          "REM Sleep Breathing Rate", 
                          "SD of Sleep Duration"#, "SD of Mid-Sleep"
)

clinical_measures_selected <- c("ps_sleepapnea___now", # sleep apnea
                                "ps_sleepdist___now_sev", # sleep disturbance
                                "promis_global_ph_Tscore",
                                "promis_global_mh_Tscore")

clinical_names_selected = c("Sleep apnea", 
                            "Sleep disturbance", 
                            "Global physical health", 
                            "Global mental health")

# prepare data for cluster analysis
## merge the means of all selected digital measures
sleep_mean <- proc_digital_sleep(dat_fitbit, 
                                 dat_midsleep, 
                                 dat_duration, 
                                 sleep_measures, # use the full list to make sure the last two are always SD measures
                                 window_spo2)  # use window for spo2
sleep_mean <- sleep_mean[, c("record_id", sleep_measures_selected)]

hrv_mean <- proc_digital_hrvspo2(dat_fitbit, 
                                 hrv_measures, 
                                 window_spo2)  # use window for spo2

all_mean = merge(sleep_mean, hrv_mean, by="record_id", all.x = T) 
colnames(all_mean) = c("record_id", sleep_names_selected, hrv_names)

## remove NAs and scale numerical variables
df = na.omit(all_mean)
num_cols <- sapply(df, is.numeric)
df[, num_cols] <- scale(df[, num_cols])

print(paste("Number of Complete Cases = ", nrow(df)))

# hierarchical clustering
#dist_matrix <- daisy(df[,-1], metric = "gower") # used when there are categorical variables 
#hc = hclust(as.dist(dist_matrix), method="ward.D2") 

dist_matrix <- dist(df[,-1])
hc = hclust(dist_matrix, method="ward.D2") # ward.D2: minimize within-cluster variance

# scree plot
output_path_clusterplots <- paste0(output_path, "cluster_plots/")
dir.create(output_path_clusterplots)

pdf(paste0(output_path_clusterplots, "scree_plot.pdf"))
screeplot(hc, max_k = 20)
dev.off()

# check BIC
mc = Mclust(as.matrix(df[,-1]), G=1:20)
summary(mc) # outputs the optimal number of clusters and corresponding BIC
pdf(paste0(output_path_clusterplots, "BIC.pdf"), width=10, height=6)
plot(mc, what = "BIC", ylim=c(-33000, -23000)) # plot BIC; each color represents a type of Gaussian finite mixture model fitted for model-based clustering
dev.off()

# prune trees
dynamic_clusters = as.numeric(cutreeDynamic(hc, 
                                            minClusterSize = 150, # change to 130 for 5 groups
                                            distM=as.matrix(dist_matrix)
                                            #deepSplit=2
))

table(dynamic_clusters)   # number of subjects in each cluster

# radar plot
colors <- c("red", "blue", "green", "orange")#, "purple") 
linetypes <- c("solid", "dashed", "dotted", "dotdash")#, "longdash")

pdf(paste0(output_path_clusterplots, "radar_plot.pdf"), width=10, height=8)
radarplot(df[, -1], dynamic_clusters, colors, linetypes, title="Cluster Summary")
dev.off()

# Summary statistics

### recode the pasc group data
### -/-, -/? -> 0
### -/+, +/-, +/+, +/? -> 1
adat = read.csv("CovData/covdat_spo2avg.csv")
idx_1 <- which(adat$pasc_group == "-/-")
idx_2 <- which(adat$pasc_group == "-/+")
idx_3 <- which(adat$pasc_group == "+/-")
idx_4 <- which(adat$pasc_group == "+/+")
idx_5 <- which(adat$pasc_group == "-/?")
idx_6 <- which(adat$pasc_group == "+/?")   
adat$pasc_recoded <- NA
adat$pasc_recoded[idx_1] <- 0
adat$pasc_recoded[idx_2] <- 1
adat$pasc_recoded[idx_3] <- 1
adat$pasc_recoded[idx_4] <- 1
adat$pasc_recoded[idx_5] <- 0
adat$pasc_recoded[idx_6] <- 1

### merge cluster membership, PASC status, demographics, clinical measures
clusters <- df %>%
  select(record_id) %>%
  mutate(cluster = factor(dynamic_clusters)) %>%  # add cluster membership
  left_join(adat %>% select(record_id, pasc_recoded)) %>%  # add PASC status
  left_join(dat_core) %>% # add demographics
  left_join(clinical_mean_spo2 %>% select(record_id, all_of(clinical_measures_selected)))   # add clinical measures

str(clusters)

# save results in Seven Bridges, which will be used to generate cluster summary tables 
write.csv(clusters, file=paste0(output_path, "cluster_summary.csv"))

# density plots of age, global physical/mental health for each cluster
clusters_long_cont <- clusters %>%
  select(cluster, age_enroll, promis_global_ph_Tscore, promis_global_mh_Tscore) %>%
  pivot_longer(cols=-cluster, names_to = "Variable", values_to = "Value") %>%
  mutate(Variable = case_when(
    Variable == "age_enroll" ~ "Age",
    Variable == "promis_global_ph_Tscore" ~ "Global Physical Health",
    Variable == "promis_global_mh_Tscore" ~ "Global Mental Health",
    TRUE ~ Variable
  ))

pdf(paste0(output_path_clusterplots, "cluster_summary_density_plot.pdf"), width=8, height=4)
ggplot(clusters_long_cont, aes(x = Value, color = cluster)) +
  geom_density(alpha=0.8) +
  facet_wrap(~Variable, scales = "fixed") +
  labs(y = "Density", x = "", color = "Group") + 
  scale_color_manual(values = colors) + 
  theme_classic() 
dev.off()

# clustering with only PASC+
df_pasc=df %>%
  left_join(adat %>% select(record_id, pasc_recoded)) %>%
  filter(!is.na(pasc_recoded) & pasc_recoded==1) %>%
  select(-pasc_recoded)

dist_matrix_pasc <- dist(df_pasc[,-1])
hc_pasc = hclust(dist_matrix_pasc, method="ward.D2") # ward.D2: minimize within-cluster variance

# scree plot
output_path_clusterplots_new <- paste0(output_path, "cluster_plots_pasc/")
dir.create(output_path_clusterplots_new)

pdf(paste0(output_path_clusterplots_new, "scree_plot.pdf"))
screeplot(hc_pasc, max_k = 20)
dev.off()

# check BIC
mc_pasc = Mclust(as.matrix(df_pasc[,-1]), G=1:20)
summary(mc_pasc) # outputs the optimal number of clusters and corresponding BIC
pdf(paste0(output_path_clusterplots_new, "BIC.pdf"), width=10, height=6)
plot(mc_pasc, what = "BIC", ylim=c(-11000, -8000)) # plot BIC; each color represents a type of Gaussian finite mixture model fitted for model-based clustering
dev.off()

# prune trees
dynamic_clusters_pasc = as.numeric(cutreeDynamic(hc_pasc, 
                                                 minClusterSize = 50, # change to 130 for 5 groups
                                                 distM=as.matrix(dist_matrix_pasc)
                                                 #deepSplit=2
)) %>%
  dplyr::recode(`3` = 4,
                `4` = 3,
                `1` = 2,
                `2` = 1)

table(dynamic_clusters_pasc)   # number of subjects in each cluster

# radar plot

colors <- c("red", "blue", "cyan", "orange")#, "purple") 
linetypes <- c("solid", "dashed", "dotted", "dotdash")#, "longdash")

pdf(paste0(output_path_clusterplots_new, "radar_plot.pdf"), width=10, height=8)
radarplot(df_pasc[, -1], dynamic_clusters_pasc, colors, linetypes, title="Cluster Summary")
dev.off()

# Summary statistics

### merge cluster membership, PASC status, demographics, clinical measures
clusters_pasc <- df_pasc %>%
  select(record_id) %>%
  mutate(cluster = factor(dynamic_clusters_pasc)) %>%  # add cluster membership
  left_join(adat %>% select(record_id, pasc_recoded)) %>%  # add PASC status
  left_join(dat_core) %>% # add demographics
  left_join(clinical_mean_spo2 %>% select(record_id, all_of(clinical_measures_selected)))   # add clinical measures

str(clusters_pasc)

# save results in Seven Bridges, which will be used to generate cluster summary tables 
write.csv(clusters_pasc, file=paste0(output_path, "cluster_summary_pasc.csv"))

# density plots of age, global physical/mental health for each cluster
clusters_long_cont_pasc <- clusters_pasc %>%
  select(cluster, age_enroll, promis_global_ph_Tscore, promis_global_mh_Tscore) %>%
  pivot_longer(cols=-cluster, names_to = "Variable", values_to = "Value") %>%
  mutate(Variable = case_when(
    Variable == "age_enroll" ~ "Age",
    Variable == "promis_global_ph_Tscore" ~ "Global Physical Health",
    Variable == "promis_global_mh_Tscore" ~ "Global Mental Health",
    TRUE ~ Variable
  ))

pdf(paste0(output_path_clusterplots_new, "cluster_summary_density_plot.pdf"), width=8, height=4)
ggplot(clusters_long_cont_pasc, aes(x = Value, color = cluster)) +
  geom_density(alpha=0.8) +
  facet_wrap(~Variable, scales = "fixed") +
  labs(y = "Density", x = "", color = "Group") + 
  scale_color_manual(values = colors) + 
  theme_classic() 
dev.off()

########################################## Criterion Profile Analysis

############ prepare data for CPA
iv_names = c(sleep_names_selected, hrv_names)
## iv 
df_cpa_iv = merge(sleep_mean, hrv_mean, by="record_id", all.x = T)
## dv
df_cpa_dv = clinical_mean_spo2[, c("record_id", clinical_measures_selected)]
## covariates
adat$month = as.factor(adat$month)
dat_cov_all = merge(dat_cov_spo2, adat[,c("record_id","numrecs_in_window", "days_between_visits", "month")], 
                    by="record_id", all.x=T)

## merge iv, dv, covariates
df_cpa = merge(df_cpa_iv, df_cpa_dv, by="record_id", all.x=T)
df_cpa = merge(df_cpa, dat_cov_all, by="record_id", all.x=T)

## scale numerical variables
df_cpa$ps_sleepapnea___now = as.factor(df_cpa$ps_sleepapnea___now)
df_cpa$ps_sleepdist___now_sev = as.factor(df_cpa$ps_sleepdist___now_sev)
str(df_cpa)
num_cols <- sapply(df_cpa, is.numeric)
df_cpa[, num_cols] <- scale(df_cpa[, num_cols])

## prepare data for the cpa() function which only takes numeric variables
df_cpa_dummy = df_cpa
df_cpa_dummy$ps_sleepapnea___now = as.numeric(as.character(df_cpa_dummy$ps_sleepapnea___now))
df_cpa_dummy$ps_sleepdist___now_sev = as.numeric(as.character(df_cpa_dummy$ps_sleepdist___now_sev))
library(fastDummies)
# Create dummy variables while keeping NAs
df_cpa_dummy = dummy_cols(df_cpa_dummy, 
                          select_columns = c("biosex", "race_unique_an", 
                                             "bmi", "smoking", "alcohol", "month"), 
                          remove_first_dummy = TRUE,
                          ignore_na = FALSE)
df_cpa_dummy = df_cpa_dummy %>%
  select(-ends_with("_NA"), -all_of(c("biosex", "race_unique_an", 
                                      "bmi", "smoking", "alcohol", "month"))) %>%
  rename_all(~ str_replace_all(., "[[:space:]-/]", "_"))

str(df_cpa_dummy)

####### CPA
#anova(mod)
#R2.full = 0: Tests whether the full model is better than a model with no predictors
#R2.pat = 0: Tests whether the pattern model is significant
#R2.lvl = 0: Tests whether the level model is significant
#R2.full = R2.lvl: Tests whether the full model is equivalent to the level model
#R2.full = R2.pat: Tests whether the full model is equivalent to the pattern model

resulttable_cpa = NULL
for(i in 1:length(clinical_measures_selected)){
  family = ifelse(i <= (length(clinical_measures_selected) - 2),
                  "binomial",
                  "gaussian")
  # cpa estimates, std.error, t value, p.value, CI  
  ctable = cpa_new(dat_cpa,
                   dat_cpa_iv,
                   iv_names,
                   clinical_measure=clinical_measures_selected[i],
                   clinical_name=clinical_names_selected[i],
                   family,
                   covariate_names = covariate_names[[3]]) 
  resulttable_cpa = rbind(resulttable_cpa, ctable)
  # ANOVA test for pattern effects
  cpa_anova(dat_cpa_dummy,
            dat_cpa_iv,
            iv_names,
            clinical_measure=clinical_measures_selected[i],
            clinical_name=clinical_measures_selected[i],
            family,
            covariate_names = covariate_names[[3]])
}


resulttable_cpa$iv = factor(resulttable_cpa$iv,
                            levels = iv_names)
resulttable_cpa$dv = factor(resulttable_cpa$dv,
                            levels = clinical_names_selected)
resulttable_cpa = resulttable_cpa %>%
  mutate(
    sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ ""  
    )) 

# criterion pattern score plots
output_path_cpaplots = paste0(output_path,"cpa_plots/")
dir.create(output_path_cpaplots)

pdf(paste0(output_path_cpaplots, "cpa_plot_sleep.pdf"), width=12, height=6)
ggplot(resulttable_cpa[resulttable_cpa$dv %in% clinical_names_selected[1:2], ], 
       aes(x = iv, y = Estimate, group=dv)) +
  geom_line() +
  geom_point(shape=1, size=2) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) + 
  geom_text(
    aes(label = sig, 
        y = ifelse(upper > 0, upper + 5, lower - 5)), 
    size = 5
  ) +
  geom_hline(yintercept = 0, 
             linetype = "dashed", color = "red") +
  facet_wrap(~dv, ncol=2, scales = "fixed") + 
  labs(x = "Independent Variable", y = "Criterion Pattern Score") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
dev.off()

resulttable2 = resulttable_cpa[resulttable_cpa$dv %in% clinical_names_selected[3:4], ]
resulttable2 = resulttable2 %>%
  mutate(
    Estimate_new = -Estimate,  # Flip the sign of estimate
    lower_new = Estimate_new - abs(Estimate - lower),  # Adjust lower
    upper_new = Estimate_new + abs(Estimate - upper)   # Adjust upper
  )
pdf(paste0(output_path_cpaplots, "cpa_plot_global_health.pdf"), width=12, height=6)
ggplot(resulttable2, 
       aes(x = iv, y = Estimate_new, group=dv)) +
  geom_line() +
  geom_point(shape=1, size=2) +
  geom_errorbar(aes(ymin = lower_new, ymax = upper_new), width = 0.2) + 
  geom_text(
    aes(label = sig, 
        y = ifelse(upper_new > 0, upper_new + 2, lower_new - 2)), 
    size = 5
  ) +
  geom_hline(yintercept = 0, 
             linetype = "dashed", color = "red") +
  facet_wrap(~dv, ncol=2, scales = "fixed") + 
  labs(x = "Independent Variable", y = "Criterion Pattern Score") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
dev.off()


