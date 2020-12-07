library(data.table)
library(ggplot2)
library(data.table)
library(stringr)
library(tidyverse)






###########################################
#Datenvorbereitung

#GWL data
gwl_data <- as.data.table(read.table("Data/GWL_1900-2010.csv", header = TRUE, sep = ";",
                                     na.strings = " "))
#wide to long:

gwl_data1 <- as.data.frame(t(gwl_data))
colnames(gwl_data1) <- gwl_data$JAHRMONAT
gwl_data1 <- gwl_data1[-1,]
day <- seq(from = 1, to = 31, by = 1)
gwl_data1 <- cbind(day, gwl_data1)
gwl_data1 <- as.data.table(gather(data = gwl_data1,key = JAHRMONAT, value = gwl, -day))

#Remove empty rows

gwl_data1 <- gwl_data1[!(apply(gwl_data1, 1, function(y) any(y == ""))),]



#split year and month
gwl_data1[, ":=" (year = substr(JAHRMONAT, 1, 4), 
                  month = substr(JAHRMONAT, 5, 6))][, JAHRMONAT := NULL]
#rename and pad day
gwl_data_split <- copy(gwl_data1[, day := str_pad(sub("X", "", day), 2, "left", "0")])
#fuse to date
gwl_data1[, date := as.Date(paste(year, month, day, sep = "-"))]

#reorder cols
setcolorder(gwl_data1, c("date", "day","month","year","gwl"))

saveRDS(gwl_data1,file = "Data\\gwl_data1.rds")

#subset timeframe

gwl1971 <- gwl_data1[year %in% seq(1971, 2010), ]
saveRDS(gwl1971, file ="Data\\gwl1971.rds")





#subset 1971-2010
cli_data_1971 <- copy(cli_data)[format(as.Date(date),"%Y") %in% seq(1971, 2010), ]
#unify time stamps
#table(format(cli_data_2k$date, "%m"), cli_data_2k$time)
#subtract 1 from all summer-time timestamps
#TODO neaten this up
cli_data_1971$time[which(cli_data_1971$time %in% c(1, 7, 13, 19))] <- 
  cli_data_1971$time[which(cli_data_1971$time %in% c(1, 7, 13, 19))] - 1

#create an average over the day
cli_data_1971_avgDay <- copy(cli_data_1971)[, .(avg_mslp = mean(mslp),
                                            avg_geopot = mean(geopotential)),
                                        by = .(date, longitude, latitude)]


#replace longitude and latitude with indeciies
cli_data_1971_avgDay_index <- copy(cli_data_1971_avgDay)
setorder(cli_data_1971_avgDay_index, date, longitude, latitude)

cli_data_1971_avgDay_index[, geoIndex := 1:.N, by = date][, ":=" (longitude = NULL,
                                                                latitude = NULL)]


#create wide format dataset
cli_data_1971_avg_wide <- dcast(copy(cli_data_1971_avgDay_index),
                              date ~ geoIndex,
                              value.var = c("avg_mslp", "avg_geopot"))


cli_data_1971_avg_wide[, ":=" (year = substr(date, 1, 4), 
                               month = substr(date, 6, 7),
                                 day = substr(date,9,10))][, date := NULL]

#Datum in einer Zeile
cli_data_1971_avg_wide[, date := as.Date(paste(year, month, day, sep = "-"))]

saveRDS(cli_data_1971_avg_wide, "Data/cli_data_1971_avgDay_wide.rds")




#####################################

gwl1971 <- readRDS("Data/gwl1971.rds")
cli_data_1971_avg_wide <- readRDS("Data/cli_data_1971_avgDay_wide.rds")

#Beide Datensaetze zusammenfuehren

cli_gwl_1971 <- merge(gwl1971,cli_data_1971_avg_wide, by = c("date","year","month","day"))
cli_gwl_1971$id <- seq(from = 1, to = nrow(cli_gwl_1971),by = 1)
setcolorder(cli_gwl_1971, c("id"))
saveRDS(cli_gwl_1971,"Data\\cli_gwl_1971.rds")


# Hntereinadner folgende GWLs auflisten



b <- with(rle(cli_gwl_1971$gwl), lengths[values == "TRM"])
 table(b)  

a <- rle(cli_gwl_1971$gwl)
b <-a[["lengths"]]
c <- a[["values"]]

# gibt aus, wie viele Tage eine einzelne GWL andauert
lengthGWL <-as.data.frame(cbind(c,b))
colnames(lengthGWL) <- c("gwl","length")

sum(as.numeric(lengthGWL$length))

# gibt aus, wie oft eine GWL im Zeitraum 1971 - 2010 vorkommt
GWLAnzahl <- as.data.frame(table(lengthGWL$gwl))
GWLAnzahl <-GWLAnzahl[order(GWLAnzahl$Freq),]


# Anzahl der GWLs je Laenge
table(lengthGWL$length)
AnzahlLaenge <- as.data.frame(table(lengthGWL$length))
colnames(AnzahlLaenge) <- c("duration", "frequency")
#### nicht numeric auf duration anwenden!!!! Verschiebt den data frame








