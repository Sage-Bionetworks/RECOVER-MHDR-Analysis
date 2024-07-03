source("sage_team/wearables-summaries/connect_to_remote.R")

library(tidyverse)

data_measures <- 
  yaml::read_yaml("sage_team/wearables-summaries/measures-per-platform.yaml")

data_measures_df <- 
  tibble(
    platform = rep(names(data_measures), c(sum(lengths(data_measures$Fitbit)), sum(lengths(data_measures$Healthkit)))),
    category = unlist(lapply(data_measures, function(p) rep(names(p), lengths(p)))),
    measure = unlist(lapply(data_measures, function(p) lapply(p, function(x) sapply(x, function(y) y$measure)))),
    dataset = unlist(lapply(data_measures, function(p) lapply(p, function(x) sapply(x, function(y) y$dataset))))
  )

# Read a dataset into a data frame
dataset_path <- 
  valid_paths[stringr::str_detect(valid_paths, dataset)]

dataset <- 
  arrow::open_dataset(
    s3$path(
      dataset_path
    )
  )
