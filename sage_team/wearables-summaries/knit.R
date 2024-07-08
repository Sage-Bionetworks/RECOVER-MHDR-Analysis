tictoc::tic("INFO: Wearables summary stats markdown knit to synapse folder")

synapseclient <- reticulate::import("synapseclient")
syn_temp <- synapseclient$Synapse()
syn_temp$login(authToken = Sys.getenv("SYNAPSE_AUTH_TOKEN"))
synapser::synLogin()

### Please install v1.28 of reticulate, as this is the needed version for synapser
### devtools::install_version("reticulate", version = "1.28", repos = "http://cran.us.r-project.org")
DASHBOARD_FOLDER_SYNID <- "syn61348473"
FOLDER_NAME <- "Wearables Summary Stats"

knit2synapse:::createAndKnitToFolderEntityClient(
  file = "~/RECOVER-MHDR-Analysis/sage_team/wearables-summaries/wearables_summary_stats.Rmd",
  parentId = DASHBOARD_FOLDER_SYNID,
  folderName = FOLDER_NAME)

tictoc::toc()
