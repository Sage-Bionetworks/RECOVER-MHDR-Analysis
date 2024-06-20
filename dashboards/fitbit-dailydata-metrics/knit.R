tictoc::tic("INFO: Fitbit DailyData Metrics markdown knit to archive folder")

synapseclient <- reticulate::import("synapseclient")
syn_temp <- synapseclient$Synapse()
syn_temp$login(authToken = Sys.getenv("SYNAPSE_AUTH_TOKEN"))
synapser::synLogin()

### Please install v1.28 of reticulate, as this is the needed version for synapser
### devtools::install_version("reticulate", version = "1.28", repos = "http://cran.us.r-project.org")
DASHBOARD_FOLDER_SYNID <- "syn61348473"
FOLDER_NAME <- "Fitbit DailyData Metrics"

knit2synapse:::createAndKnitToFolderEntityClient(
  file = "dashboards/fitbit-dailydata-metrics/fitbit-dailydata-metrics.Rmd",
  parentId = DASHBOARD_FOLDER_SYNID,
  folderName = FOLDER_NAME)

tictoc::toc()

tictoc::tic("INFO: Fitbit DailyData Metrics markdown knit to project wiki")

knit2synapse::knitfile2synapse(
  file = "dashboards/fitbit-dailydata-metrics/fitbit-dailydata-metrics.Rmd",
  owner = "syn51105296",
  parentWikiId = '627748',
  wikiName = FOLDER_NAME,
  overwrite = TRUE)

tictoc::toc()
