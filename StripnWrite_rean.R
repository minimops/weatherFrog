## this file prepares the weather dataset and saves the results

library(data.table)

#import and convert to data.table
cli_data <- as.data.table(readRDS("Data/data_reanalysis_20201109.rds"))

#splitting Date and Time (takes a while)
cli_data[, date := as.Date(cli_data$time, tz = "CET")]
cli_data[, time := as.numeric(format(cli_data$time,"%H"))]

#saving dataset with split Date and time
saveRDS(cli_data, "Data/cli_data.rds")




#subset 2000-2010
cli_data_2k <- copy(cli_data)[format(as.Date(date),"%Y") %in% seq(2000, 2010), ]

#unify time stamps
#table(format(cli_data_2k$date, "%m"), cli_data_2k$time)
#subtract 1 from all summer-time timestamps
#TODO neaten this up
cli_data_2k$time[which(cli_data_2k$time %in% c(1, 7, 13, 19))] <- 
  cli_data_2k$time[which(cli_data_2k$time %in% c(1, 7, 13, 19))] - 1
                 
#save subset 2k dataset
saveRDS(cli_data_2k, "Data/cli_data_2k.rds")


#create an average over the day
cli_data_2k_avgDay <- copy(cli_data_2k)[, .(avg_mslp = mean(mslp),
                                               avg_geopot = mean(geopotential)),
                                           by = .(date, longitude, latitude)]

#save avg 2k dataset
saveRDS(cli_data_2k_avgDay, "Data/cli_data_2k_avgDay.rds")
