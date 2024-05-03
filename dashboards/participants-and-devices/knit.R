tictoc::tic("INFO: Participants and Devices markdown markdown knit to archive folder")

synapseclient <- reticulate::import("synapseclient")
syn_temp <- synapseclient$Synapse()
syn_temp$login(authToken = Sys.getenv("SYNAPSE_AUTH_TOKEN"))
synapser::synLogin()

### Please install v1.28 of reticulate, as this is the needed version for synapser
### devtools::install_version("reticulate", version = "1.28", repos = "http://cran.us.r-project.org")
DASHBOARD_FOLDER_SYNID <- "syn58643693"
FOLDER_NAME <- "Participants and Devices"

knit2synapse:::createAndKnitToFolderEntityClient(
  file = "dashboards/participants-and-devices/participants-and-devices.Rmd",
  parentId = DASHBOARD_FOLDER_SYNID,
  folderName = FOLDER_NAME)

tictoc::toc()

tictoc::tic("INFO: Participants and Devices markdown knit to project wiki")

knit2synapse::knitfile2synapse(
  file = "dashboards/participants-and-devices/participants-and-devices.Rmd",
  owner = "syn51105296",
  parentWikiId = '627716',
  wikiName = FOLDER_NAME,
  overwrite = TRUE)

tictoc::toc()
