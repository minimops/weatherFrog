dataClara <- readRDS("Data/cli_data_05_avgDay_wide.rds")


dataSclaed <- scale(copy(dataClara[, 2:321]))


  
  
PAMhelper(cbind(date = dataClara$date, as.data.table(dataSclaed)), weights = NULL, 
            metric = "manhattan", dist = FALSE, fname = "originalPAM05")
