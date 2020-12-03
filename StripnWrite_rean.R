## this file prepares the weather dataset and saves the results

library(data.table)

#import and convert to data.table
cli_data <- as.data.table(readRDS("Data/data_reanalysis_20201109.rds"))

#splitting Date and Time (takes a while)
cli_data[, date := as.Date(cli_data$time, tz = "CET")]
cli_data[, time := as.numeric(format(cli_data$time,"%H"))]

#saving dataset with split Date and time
saveRDS(cli_data, "Data/cli_data.rds")
head(cli_data)
tail(cli_data)


#subset 2000-2010
cli_data_2k <- copy(cli_data)[format(as.Date(date),"%Y") %in% seq(2000, 2010), ]
# subset 2006-2010
cli_data_05 <- copy(cli_data)[format(as.Date(date),"%Y") %in% seq(2006, 2010), ]
#unify time stamps
#table(format(cli_data_2k$date, "%m"), cli_data_2k$time)
#subtract 1 from all summer-time timestamps
#TODO neaten this up
cli_data_2k$time[which(cli_data_2k$time %in% c(1, 7, 13, 19))] <- 
  cli_data_2k$time[which(cli_data_2k$time %in% c(1, 7, 13, 19))] - 1

cli_data_05$time[which(cli_data_05$time %in% c(1, 7, 13, 19))] <- 
  cli_data_05$time[which(cli_data_05$time %in% c(1, 7, 13, 19))] - 1                 
#save subset 2k dataset
saveRDS(cli_data_2k, "Data/cli_data_2k.rds")
saveRDS(cli_data_05, "Data/cli_data_05.rds")

#create an average over the day
cli_data_2k_avgDay <- copy(cli_data_2k)[, .(avg_mslp = mean(mslp),
                                               avg_geopot = mean(geopotential)),
                                           by = .(date, longitude, latitude)]


cli_data_05_avgDay <- copy(cli_data_05)[, .(avg_mslp = mean(mslp),
                                            avg_geopot = mean(geopotential)),
                                        by = .(date, longitude, latitude)]
#save avg 2k dataset
saveRDS(cli_data_2k_avgDay, "Data/cli_data_2k_avgDay.rds")
saveRDS(cli_data_05_avgDay, "Data/cli_data_05_avgDay.rds")


#replace longitude and latitude with indeciies
cli_data_05_avgDay_index <- copy(cli_data_05_avgDay)
setorder(cli_data_05_avgDay_index, date, longitude, latitude)

cli_data_05_avgDay_index[, geoIndex := 1:.N, by = date][, ":=" (longitude = NULL,
                                                                latitude = NULL)]
saveRDS(cli_data_05_avgDay_index, "Data/cli_data_05_avgDay_index.rds")

#create wide format dataset
cli_data_05_avg_wide <- dcast(copy(cli_data_05_avgDay_index),
                              date ~ geoIndex,
                              value.var = c("avg_mslp", "avg_geopot"))

saveRDS(cli_data_05_avg_wide, "Data/cli_data_05_avgDay_wide.rds")

