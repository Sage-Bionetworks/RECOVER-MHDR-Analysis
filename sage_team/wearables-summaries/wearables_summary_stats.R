source("sage_team/wearables-summaries/connect_to_remote.R")

# Read a dataset into a data frame
dataset_path <- 
  valid_paths[
    stringr::str_detect(
      valid_paths, 
      "intradaycombined"
    )
  ]

dataset <- 
  arrow::open_dataset(
    s3$path(
      dataset_path
    )
  )
