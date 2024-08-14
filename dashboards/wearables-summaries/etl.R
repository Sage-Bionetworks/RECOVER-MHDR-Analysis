source("~/RECOVER-MHDR-Analysis/dashboards/wearables-summaries/connect_to_remote.R")

library(tidyverse)

data_measures <- 
  yaml::read_yaml("~/RECOVER-MHDR-Analysis/dashboards/wearables-summaries/measures-per-platform.yaml")

data_measures_df <- 
  tibble(
    platform = 
      rep(names(data_measures), 
          c(sum(lengths(data_measures$Fitbit)), 
            sum(lengths(data_measures$Healthkit)))
      ),
    category = 
      unlist(
        lapply(data_measures, function(p) {
          rep(names(p), lengths(p))
        })
      ),
    measure = 
      unlist(
        lapply(data_measures, function(p) {
          lapply(p, function(x) {
            sapply(x, function(y) y$measure)
          })
        })
      ),
    dataset = 
      unlist(
        lapply(data_measures, function(p) {
          lapply(p, function(x) {
            sapply(x, function(y) y$dataset)
          })
        })
      ),
    lowerbound = 
      unlist(
        lapply(data_measures, function(p) {
          lapply(p, function(x) {
            sapply(x, function(y) as.numeric(y$lowerbound))
          })
        })
      ),
    upperbound = 
      unlist(
        lapply(data_measures, function(p) {
          lapply(p, function(x) {
            sapply(x, function(y) as.numeric(y$upperbound))
          })
        })
      )
  )

# Extract datasets
datasets <- 
  unique(
    data_measures_df$dataset[!is.na(data_measures_df$dataset)]
  )

dataset_paths <- tibble(dataset = character(), path = character())
dataset_paths <- 
  sapply(datasets, function(dataset) {
    path_exists <- 
      stringr::str_detect(
        valid_paths, 
        stringr::regex(paste0(dataset,"$"))
      )
    matching_path <-
      valid_paths[path_exists]
  }, simplify = FALSE) %>% 
  as_tibble() %>% 
  pivot_longer(
    cols = everything(), 
    names_to = "dataset", 
    values_to = "path"
  )

datasets <- 
  dataset_paths %>% 
  split(dataset_paths$dataset) %>% 
  purrr::map(\(df) arrow::open_dataset(s3$path(df$path)))

reduced_datasets <- 
  names(datasets) %>%
  sapply(function(dataset) {
    measures <-
      data_measures_df$measure[
        which(data_measures_df$dataset==dataset)
      ] %>%
      as.vector() %>% 
      sapply(function(measure) {
        strsplit(measure, "==") %>% 
          unlist() %>% 
          first()
      }) %>% 
      unique()
    
    non_measure_cols <- c("ParticipantIdentifier",
                          "StartDate",
                          "EndDate",
                          "Date",
                          "cohort")
    
    dataset <-
      datasets[[dataset]] %>% 
      select(any_of(c(non_measure_cols, measures)))
    
  }, simplify = FALSE)

# Fitbit
datasets_fitbit <- reduced_datasets[str_detect(names(reduced_datasets), "fitbit")]

## Heart Rate
lower <- 
  unique(
    data_measures_df$lowerbound[
      stringr::str_detect(data_measures_df$measure, "HeartRate") & data_measures_df$category=="HeartRate" & data_measures_df$platform=="Fitbit"
    ]
  )

upper <- 
  unique(
    data_measures_df$upperbound[
      stringr::str_detect(data_measures_df$measure, "HeartRate") & data_measures_df$category=="HeartRate" & data_measures_df$platform=="Fitbit"
    ]
  )

tmp_hr <- 
  list(
    RestingHeartRate = 
      datasets_fitbit$fitbitdailydata %>% 
      select(ParticipantIdentifier, Date, RestingHeartRate, cohort) %>% 
      mutate(RestingHeartRate = as.numeric(RestingHeartRate)) %>% 
      filter(RestingHeartRate >= lower & RestingHeartRate <= upper) %>% 
      rename(Datetime = Date, 
             HeartRate = RestingHeartRate) %>% 
      collect() %>% 
      mutate(Datetime = lubridate::ymd_hms(Datetime)) %>% 
      distinct() %>% 
      mutate(date = lubridate::date(Datetime),
             cohort = str_remove(cohort, regex("_.*"))),
    AverageHeartRate = 
      datasets_fitbit$fitbitactivitylogs %>% 
      select(ParticipantIdentifier, StartDate, AverageHeartRate, cohort) %>% 
      mutate(AverageHeartRate=as.numeric(AverageHeartRate)) %>% 
      filter(AverageHeartRate >= lower & AverageHeartRate <= upper) %>% 
      rename(HeartRate = AverageHeartRate) %>% 
      collect() %>% 
      mutate(StartDate = lubridate::ymd_hms(StartDate)) %>% 
      distinct() %>% 
      mutate(date = lubridate::date(StartDate),
             cohort = str_remove(cohort, regex("_.*")))
  )

## Physical Activity
lower <- 
  list(
    minsactive = 
      unique(
        data_measures_df$lowerbound[
          stringr::str_detect(data_measures_df$measure, "^(Minutes).*(Sedentary|Active)$") & data_measures_df$category=="PhysicalActivity" & data_measures_df$platform=="Fitbit"
        ]
      ),
    steps = 
      unique(
        data_measures_df$lowerbound[
          stringr::str_detect(data_measures_df$measure, "Steps") & data_measures_df$category=="PhysicalActivity" & data_measures_df$platform=="Fitbit"
        ]
      )
  )

upper <- 
  list(
    minsactive = 
      unique(
        data_measures_df$upperbound[
          stringr::str_detect(data_measures_df$measure, "^(Minutes).*(Sedentary|Active)$") & data_measures_df$category=="PhysicalActivity" & data_measures_df$platform=="Fitbit"
        ]
      ),
    steps = 
      unique(
        data_measures_df$upperbound[
          stringr::str_detect(data_measures_df$measure, "Steps") & data_measures_df$category=="PhysicalActivity" & data_measures_df$platform=="Fitbit"
        ]
      )
  )

tmp_physact <- 
  list(
    MinsActivity =
      datasets_fitbit$fitbitdailydata %>%
      select(-any_of(c("RestingHeartRate", "Steps"))) %>%
      mutate(across(-c(ParticipantIdentifier, Date, cohort), as.numeric)) %>%
      filter(across(-c(ParticipantIdentifier, Date, cohort), ~ . >= lower$minsactive & . <= upper$minsactive)) %>%
      rename(Datetime = Date) %>%
      collect() %>%
      mutate(Datetime = lubridate::ymd_hms(Datetime)) %>%
      distinct() %>%
      mutate(date = lubridate::date(Datetime),
             cohort = str_remove(cohort, regex("_.*"))),
    Steps =
      datasets_fitbit$fitbitdailydata %>%
      select(ParticipantIdentifier, Date, Steps, cohort) %>%
      mutate(Steps = as.numeric(Steps)) %>%
      filter(Steps >= lower$steps & Steps <= upper$steps) %>%
      rename(Datetime = Date) %>%
      collect() %>%
      mutate(Datetime = lubridate::ymd_hms(Datetime)) %>%
      distinct() %>%
      mutate(date = lubridate::date(Datetime),
             cohort = str_remove(cohort, regex("_.*")))
  )

## Sleep
lower <- 
  list(
    duration = 
      unique(
        data_measures_df$lowerbound[
          stringr::str_detect(data_measures_df$measure, "Duration") & data_measures_df$category=="Sleep" & data_measures_df$platform=="Fitbit"
        ]
      ),
    efficiency = 
      unique(
        data_measures_df$lowerbound[
          stringr::str_detect(data_measures_df$measure, "Efficiency") & data_measures_df$category=="Sleep" & data_measures_df$platform=="Fitbit"
        ]
      )
  )

upper <- 
  list(
    duration = 
      unique(
        data_measures_df$upperbound[
          stringr::str_detect(data_measures_df$measure, "Duration") & data_measures_df$category=="Sleep" & data_measures_df$platform=="Fitbit"
        ]
      ),
    efficiency = 
      unique(
        data_measures_df$upperbound[
          stringr::str_detect(data_measures_df$measure, "Efficiency") & data_measures_df$category=="Sleep" & data_measures_df$platform=="Fitbit"
        ]
      )
  )

tmp_sleep <- 
  list(
    Duration =
      datasets_fitbit$fitbitsleeplogs %>%
      select(-any_of(c("EndDate", "Efficiency"))) %>%
      mutate(Duration = as.numeric(Duration)) %>%
      # filter(Duration >= lower$duration & Duration <= upper$duration) %>%
      collect() %>%
      mutate(StartDate = lubridate::ymd_hms(StartDate)) %>%
      distinct() %>%
      mutate(date = lubridate::date(StartDate),
             cohort = str_remove(cohort, regex("_.*"))),
    Efficiency =
      datasets_fitbit$fitbitsleeplogs %>%
      select(-any_of(c("EndDate", "Duration"))) %>%
      mutate(Efficiency = as.numeric(Efficiency)) %>%
      # filter(Efficiency >= lower$efficiency & Efficiency <= upper$efficiency) %>%
      collect() %>%
      mutate(StartDate = lubridate::ymd_hms(StartDate)) %>%
      distinct() %>%
      mutate(date = lubridate::date(StartDate),
             cohort = str_remove(cohort, regex("_.*")))
  )

summary_stats <- 
  list(
    Fitbit = 
      list(
        HeartRate = tmp_hr,
        PhysicalActivity = tmp_physact,
        Sleep = tmp_sleep
      )
  )

# Healthkit
datasets_hk <- reduced_datasets[str_detect(names(reduced_datasets), "healthkit")]

## Heart Rate
lower <- 
  unique(
    data_measures_df$lowerbound[
      str_detect(data_measures_df$category, "HeartRate") & str_detect(data_measures_df$platform, "Healthkit")
    ]
  ) %>% 
  na.omit() %>% 
  as.numeric()

upper <- 
  unique(
    data_measures_df$upperbound[
      str_detect(data_measures_df$category, "HeartRate") & str_detect(data_measures_df$platform, "Healthkit")
    ]
  ) %>% 
  na.omit() %>% 
  as.numeric()

tmp_hr <- 
  list(
    HeartRate = 
      datasets_hk$healthkitv2samples %>% 
      select(-any_of(c("Date"))) %>% 
      filter(Type == "HeartRate") %>% 
      mutate(Value = as.numeric(Value)) %>% 
      filter(Value >= lower & Value <= upper) %>% 
      collect() %>%
      mutate(StartDate = lubridate::ymd_hms(StartDate)) %>%
      distinct() %>%
      mutate(date = lubridate::date(StartDate),
             cohort = str_remove(cohort, regex("_.*")))
  )

## Physical Activity
lower <- 
  unique(
    data_measures_df$lowerbound[
      str_detect(data_measures_df$category, "PhysicalActivity") & str_detect(data_measures_df$platform, "Healthkit")
    ]
  ) %>% 
  na.omit() %>% 
  as.numeric()

upper <- 
  unique(
    data_measures_df$upperbound[
      str_detect(data_measures_df$category, "PhysicalActivity") & str_detect(data_measures_df$platform, "Healthkit")
    ]
  ) %>% 
  na.omit() %>% 
  as.numeric()

tmp_physact <- 
  list(
    DailySteps = 
      datasets_hk$healthkitv2statistics %>% 
      select(-any_of(c("Date"))) %>% 
      filter(Type == "DailySteps") %>% 
      mutate(Value = as.numeric(Value)) %>% 
      filter(Value >= lower & Value <= upper) %>% 
      collect() %>%
      mutate(StartDate = lubridate::ymd_hms(StartDate)) %>%
      distinct() %>%
      mutate(date = lubridate::date(StartDate),
             cohort = str_remove(cohort, regex("_.*")))
  )

summary_stats$Healthkit <- 
  list(
    HeartRate = tmp_hr,
    PhysicalActivity = tmp_physact
  )
