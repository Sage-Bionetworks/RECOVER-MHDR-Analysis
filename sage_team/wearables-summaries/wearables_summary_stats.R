source("sage_team/wearables-summaries/connect_to_remote.R")

library(tidyverse)

data_measures <- 
  yaml::read_yaml("sage_team/wearables-summaries/measures-per-platform.yaml")

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
      )
  )

# Read a dataset into a data frame
datasets <- 
  unique(
    data_measures_df$dataset[data_measures_df$dataset!="NA"]
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
    
    dataset <- 
      datasets[[dataset]] %>% 
      select(any_of(c("ParticipantIdentifier", "StartDate", "EndDate", "Date", measures)))
  }, simplify = FALSE)

reduced_datasets$healthkitv2samples <- 
  reduced_datasets$healthkitv2samples %>% 
  filter(Type=="HeartRate")

reduced_datasets$healthkitv2statistics <- 
  reduced_datasets$healthkitv2statistics %>% 
  filter(Type=="DailySteps")

