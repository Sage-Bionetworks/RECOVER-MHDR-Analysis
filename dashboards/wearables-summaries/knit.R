tictoc::tic("INFO: Wearables summary stats markdown knit to synapse folder")

synapseclient <- reticulate::import("synapseclient")
syn_temp <- synapseclient$Synapse()
syn_temp$login(authToken = Sys.getenv("SYNAPSE_AUTH_TOKEN"))
synapser::synLogin()

DASHBOARD_FOLDER_SYNID <- "syn61348473"
FOLDER_NAME <- "Wearables Summary Stats"

knit2synapse:::createAndKnitToFolderEntityClient(
  file = "~/RECOVER-MHDR-Analysis/sage_team/wearables-summaries/wearables_summary_stats.Rmd",
  parentId = DASHBOARD_FOLDER_SYNID,
  folderName = FOLDER_NAME)

tictoc::toc()

