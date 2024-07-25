# Login to Synapse
synapser::synLogin()

# Get a storage token
sts_token <- 
  synapser::synGetStsStorageToken(
    entity = 'syn52506069',
    permission = 'read_only',
    output_format = 'json'
  )

# Connect to the S3 bucket using the token
s3 <- 
  arrow::S3FileSystem$create(
    access_key = sts_token$accessKeyId,
    secret_key = sts_token$secretAccessKey,
    session_token = sts_token$sessionToken,
    region="us-east-1"
  )

bucket_path <- 
  paste0(
    sts_token$bucket,
    '/',
    sts_token$baseKey,
    '/'
  )

# List archive folders and their paths
archive_folders <- 
  s3$GetFileInfo(
    arrow::FileSelector$create(
      bucket_path, 
      recursive=F
    )
  )

cat("\n----Archive versions---\n")
i <- 0
valid_paths <- character()
for (archive in archive_folders) {
  if (!grepl('.*owner.*', archive$path, perl = T, ignore.case = T)) {
    i <- i+1
    cat(i)
    cat(":", archive$path, "\n")
    valid_paths <- c(valid_paths, archive$path)
  }
}

archive_path <- valid_paths[[length(valid_paths)]]

# List datasets in a specific archive
dataset_list <- 
  s3$GetFileInfo(
    arrow::FileSelector$create(
      archive_path, 
      recursive=F
    )
  )

cat("\n----Datasets---\n")
i <- 0
valid_paths <- character()
for (dataset in dataset_list) {
  i <- i+1
  cat(i)
  cat(":", dataset$path, "\n")
  valid_paths <- c(valid_paths, dataset$path)
}
