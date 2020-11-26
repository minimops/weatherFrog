#this is a first stab at the datasets
library(data.table)

#param dataset
cli_data <- readRDS("Data/data_reanalysis_20201109.rds")

#splitting Date and Time
cli_data$date <- as.Date(cli_data$time, tz = "CET")
cli_data$time <- format(cli_data$time,"%H:%M:%S")

#subsetting data 2000-2010
cli_data_2k <- subset(cli_data, format(as.Date(date),"%Y") %in% 
              seq(2000, 2010))


#convert to data.table for further analysis
cli_data_2k_dt <- as.data.table(cli_data_2k)

#complare with gwl in that timeframe
gwl_2000s <- readRDS("Data/gwl_2000s_split.rds")
#check if all 29 gwl are present in this subset
nrow(unique(gwl_2000s[, .(gwl)]))
#this timeframe does includes 31 gwl
table(gwl_2000s[, .(gwl)])
#and every single one is represented at least 13 times

#there are always 4 measurements per day:
table(cli_data_2k_dt[, .N, by = .(longitude, latitude, date)][, N])
#however the times are not the same:
table(cli_data_2k_dt[, time])
#but the difference is always an hour, so maybe
#this is simply the difference between winter and summer
#aka 00:00 is 01:00 in the other season
#this is supportet by the fact, that the counts are always the same

#so this timeframe (2000-2010) seems like a good first pick


#TODO
#an idea to look at how to treat this, would be to 
#take a look at the differences in measurements per day


