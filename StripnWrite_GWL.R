### This file prepares the dataset of gwl data and saves the results###

library(data.table)
library(stringr)

#gwl data
gwl_data <- as.data.table(read.table("Data/GWL_1900-2010.csv", header = TRUE, sep = ";",
                                     na.strings = " "))
#wide to long:
gwl_data <- melt(gwl_data, id.vars = c("JAHRMONAT"), measure.vars = patterns("^X"),
                 variable.name = "day", value.name = "gwl")
#split year and month
gwl_data[, ":=" (year = substr(JAHRMONAT, 1, 4), 
                 month = substr(JAHRMONAT, 5, 6))][, JAHRMONAT := NULL]
#rename and pad day
gwl_data_split <- copy(gwl_data[, day := str_pad(sub("X", "", day), 2, "left", "0")])
#fuse to date
gwl_data[, date := as.Date(paste(year, month, day, sep = "-"))]
#delete columns
gwl_data[, ":=" (year = NULL, month = NULL, day = NULL)]
#reorder cols
setcolorder(gwl_data, c("date", "gwl"))

#subset timeframe
gwl_2000s <- gwl_data_split[year %in% seq(2000, 2010), ]


#saving datasets to Data folder

saveRDS(gwl_data_split, "Data/gwl_split.rds")
saveRDS(gwl_data, "Data/gwl.rds")
saveRDS(gwl_2000s, "Data/gwl_2000s_split.rds")


