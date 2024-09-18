tictoc::tic("INFO: Wearables summary stats markdown knit to synapse folder")

synapseclient <- reticulate::import("synapseclient")
syn_temp <- synapseclient$Synapse()
syn_temp$login(authToken = Sys.getenv("SYNAPSE_AUTH_TOKEN"))
synapser::synLogin()

DASHBOARD_PARENT_FOLDER_SYNID <- "syn63181262"
FOLDER_NAME <- "Wearables Summary Stats"

knit2synapse:::createAndKnitToFolderEntityClient(
  file = "~/RECOVER-MHDR-Analysis/dashboards/wearables-summaries/wearables_summary_stats.Rmd",
  parentId = DASHBOARD_PARENT_FOLDER_SYNID,
  folderName = FOLDER_NAME)

tictoc::toc()

dashboard_parent_folder_children <- 
  synapser::synGetChildren(DASHBOARD_PARENT_FOLDER_SYNID) %>% 
  synapser::as.list()

dashboard_folder_synid <- 
  dashboard_parent_folder_children[
    sapply(dashboard_parent_folder_children, function(x) x$name==FOLDER_NAME)
  ][[1]]$id

synapser::File("dashboards/wearables-summaries/data_measures_df.csv", 
               description = "Data measures used for summary stats", 
               parent = dashboard_folder_synid) %>% 
  synapser::synStore()

synapser::File("dashboards/wearables-summaries/distinct_dates_df.csv", 
               description = "Count of unique dates of wearables data", 
               parent = dashboard_folder_synid) %>% 
  synapser::synStore()

synapser::File("dashboards/wearables-summaries/distinct_dates_peds_df.csv", 
               description = "Count of unique dates of wearables data", 
               parent = dashboard_folder_synid) %>% 
  synapser::synStore()

synapser::File("dashboards/wearables-summaries/distinct_date_ranges_table.csv", 
               description = "Range of unique dates of wearables data", 
               parent = dashboard_folder_synid) %>% 
  synapser::synStore()
