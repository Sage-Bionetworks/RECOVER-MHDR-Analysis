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
