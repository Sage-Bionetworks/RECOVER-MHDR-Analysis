tictoc::tic("INFO: Fitbit DailyData Metrics markdown knit2synapse")

synapseclient <- reticulate::import("synapseclient")
syn_temp <- synapseclient$Synapse()
syn_temp$login(authToken = Sys.getenv("SYNAPSE_AUTH_TOKEN"))
synapser::synLogin()

### Please install v1.28 of reticulate, as this is the needed version for synapser
### devtools::install_version("reticulate", version = "1.28", repos = "http://cran.us.r-project.org")
DASHBOARD_FOLDER_SYNID <- "syn58643693"
FOLDER_NAME <- "Fitbit DailyData Metrics"

# knit2synapse:::createAndKnitToFolderEntityClient(
#   file = "dashboards/fitbit-dailydata-metrics/fitbit-dailydata-metrics.Rmd",
#   parentId = DASHBOARD_FOLDER_SYNID,
#   folderName = FOLDER_NAME)

knit2synapse::knitfile2synapse(
  file = "dashboards/fitbit-dailydata-metrics/fitbit-dailydata-metrics.Rmd",
  owner = "syn51105296",
  parentWikiId = '627717',
  wikiName = FOLDER_NAME,
  overwrite = TRUE)

tictoc::toc()
