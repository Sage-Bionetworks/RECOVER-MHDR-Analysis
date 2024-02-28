## Record count day wise

############
## Compare all nrows and build a funnel plot
############
### get rows for each dataset in jsons

############# EnrolledParticipants
all_nrows_json <- read.csv('all_nrows.csv', stringsAsFactors = F) %>% 
  dplyr::select(-X) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(dataset_type = stringr::str_split(name_in, '_')[[1]][1]) %>% 
  dplyr::mutate(date = stringr::str_split(name_in, '_')[[1]][2]) %>% 
  dplyr::mutate(date = stringr::str_split(date, '\\.')[[1]][1]) %>% 
  dplyr::mutate(date = lubridate::as_date(date)) %>% 
  dplyr::mutate(cohort = ifelse(grepl('adults',file_in),'adults','pediatric')) %>% 
  dplyr::ungroup()


ggplot(data = all_nrows_json, aes(x = date, y = nrows)) +
  geom_bar(stat = "identity", fill = "purple") +
  labs(title = "Enrollment",
       x = "Date", y = "Participants") + facet_wrap(~cohort) + theme_minimal()

############# FitbitECG

all_nrows_json <- read.csv('all_nrows_fitbitecg.csv', stringsAsFactors = F) %>% 
  dplyr::select(-X) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(dataset_type = stringr::str_split(name_in, '_')[[1]][1]) %>% 
  dplyr::mutate(date = stringr::str_split(name_in, '_')[[1]][2]) %>% 
  dplyr::mutate(date = stringr::str_split(date, '\\.')[[1]][1]) %>% 
  dplyr::mutate(date = stringr::str_split(date, '-')[[1]][2]) %>% 
  dplyr::mutate(date = lubridate::as_date(date)) %>% 
  dplyr::mutate(cohort = ifelse(grepl('adults',file_in),'adults','pediatric')) %>% 
  dplyr::ungroup()

all_nrows_json_fitbitecg <- all_nrows_json %>% 
  dplyr::select(-file_in, -name_in) %>% 
  unique() %>% 
  dplyr::group_by(cohort, dataset_type, date) %>% 
  dplyr::count() %>% 
  dplyr::rename(nrows = n) %>% 
  dplyr::ungroup()

ggplot(data = all_nrows_json_fitbitecg, aes(x = date, y = nrows)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "FitbitECG",
       x = "Date", y = "FitbitECG rows") + facet_wrap(~cohort) + theme_minimal()

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
  dplyr::mutate(date = stringr::str_split(name_in, '_')[[1]][3]) %>% 
  dplyr::mutate(date = stringr::str_split(date, '\\.')[[1]][1]) %>% 
  dplyr::mutate(date = stringr::str_split(date, '-')[[1]][2]) %>% 
  dplyr::mutate(date = lubridate::as_date(date)) %>% 
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
  dplyr::mutate(date = stringr::str_split(name_in, '_')[[1]][2]) %>% 
  dplyr::mutate(date = stringr::str_split(date, '\\.')[[1]][1]) %>% 
  dplyr::mutate(date = stringr::str_split(date, '-')[[1]][2]) %>% 
  dplyr::mutate(date = lubridate::as_date(date)) %>% 
  dplyr::mutate(cohort = ifelse(grepl('adults',file_in),'adults','pediatric')) %>% 
  dplyr::ungroup()

all_nrows_json_healthkitv2activitysummaries <- all_nrows_json %>% 
  dplyr::select(-file_in, -name_in, -V1) %>% 
  unique() %>% 
  dplyr::group_by(cohort, dataset_type, date) %>% 
  dplyr::count() %>%
  dplyr::rename(nrows = n) %>% 
  dplyr::ungroup()

ggplot(data = all_nrows_json_healthkitv2activitysummaries, aes(x = date, y = nrows)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "healthkitv2activitysummaries",
       x = "Date", y = "healthkitv2activitysummaries rows") + facet_wrap(~cohort) + theme_minimal()


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

