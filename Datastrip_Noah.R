#this is a first stab at the datasets
library(data.table)
library(stringr)

#gwl data
gwl_data <- as.data.table(read.table("Data/GWL_1900-2010.csv", header = TRUE, sep = ";",
                       na.strings = " "))
#long to wide:
gwl_data <- melt(gwl_data, id.vars = c("JAHRMONAT"), measure.vars = patterns("^X"),
     variable.name = "day", value.name = "gwl")
#split year and month
gwl_data[, ":=" (year = substr(JAHRMONAT, 1, 4), 
                 month = substr(JAHRMONAT, 5, 6))][, JAHRMONAT := NULL]
#rename and pad day
gwl_data[, day := str_pad(sub("X", "", day), 2, "left", "0")]
#fuse to date
gwl_data[, date := as.Date(paste(year, month, day, sep = "-"))]
#delete columns
gwl_data[, ":=" (year = NULL, month = NULL, day = NULL)]
#reorder cols
setcolorder(gwl_data, c("date", "gwl"))



#param dataset
cli_data <- readRDS("Data/data_reanalysis_20201109.rds")

#splitting Date and Time
cli_data$date <- as.Date(cli_data$time)
cli_data$time <- format(cli_data$time,"%H:%M:%S")

#subsetting data 2000-2010
cli_data_2k <- subset(cli_data, format(as.Date(date),"%Y") %in% 
              seq(2000, 2010))

#shows that the timestamps are not always the same 6
length(unique(cli_data_2k$time))

#convert to data.table for further analysis
cli_data_2k_dt <- as.data.table(cli_data_2k)

#shows that there are not always 4 per day 
table(cli_data_2k_dt[, .N, by = .(longitude, latitude, date)][, N])
#but, all of the instances of only 3 are on the 31.12 - 
#so there isnt any missing data, it just probably moved 
#to the next year


#but there isnt a year 2011
#so use a different subset instead to fulfill 4 per day
cli_data_1999to2009 <- as.data.table(subset(cli_data, format(as.Date(date),"%Y") %in% 
                                seq(1999, 2009)))
#check if all 29 gwl are present in this subset
nrow(unique(gwl_data[format(as.Date(date),"%Y") %in% seq(1999, 2009), .(gwl)]))
#this timeframe does include all 29+1 gwl
table(gwl_data[format(as.Date(date),"%Y") %in% seq(1999, 2009), .(gwl)])
#and there isnt one that is only represented just a few amount of times

#here, there are always 4 per day:
table(cli_data_1999to2009[, .N, by = .(longitude, latitude, date)][, N])
#however the times are not the same either:
table(cli_data_1999to2009[, time])
#but the difference is always an hour, so maybe
#this is simply the difference between winter and summer
#aka 00:00 is 01:00 in the other season
#this is supportet by the fact, that the counts are always the same

#so this timeframe (1999-2009) seems like a good first pick


#TODO
#an idea to look at how to treat this, would be to 
#take a look at the differences in measurements per day


