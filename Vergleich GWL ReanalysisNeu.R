library(data.table)
library(ggplot2)
library(data.table)
library(stringr)
library(tidyverse)
library(dplyr)






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


##############reanalysis data 
cli_data <- as.data.table(readRDS("Data/data_reanalysis_20201109.rds"))

#splitting Date and Time (takes a while)
cli_data[, date := as.Date(cli_data$time, tz = "CET")]
cli_data[, time := as.numeric(format(cli_data$time,"%H"))]



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
cli_gwl_1971 <- readRDS("Data\\cli_gwl_1971.rds")
cli_gw_1971 <- readRDS("Data\\cli_gwl_1971.rds")

index_length_gwl <-  rleid(cli_gwl_1971$gwl)

cli_gwl_1971 <- cbind(index_length_gwl,cli_gwl_1971)


a <- rle(cli_gwl_1971$gwl)

# gibt aus, wie viele Tage eine einzelne GWL andauert
lengthGWL <-as.data.frame(cbind(a[["values"]],a[["lengths"]]))
colnames(lengthGWL) <- c("gwl","length")
crosstable <- table(lengthGWL$gwl,lengthGWL$length)
# Die meisten GWL dauern 3 bis 7 Tage an 



# gibt aus, wie oft eine GWL im Zeitraum 1971 - 2010 vorkommt
GWLAnzahl <- as.data.frame(table(lengthGWL$gwl))
GWLAnzahl <-GWLAnzahl[order(GWLAnzahl$Freq),]
# GWL WZ kommt am haufigsten vor 


# Anzahl der GWLs je Laenge
table(as.numeric(lengthGWL$length))

################################
# Gibt es saisonale Unterschiede im Aufkommen der Wetterlagen?

#Meterologische Jahreszeiten: 
#Winter: 1. 12. - 28./29. 2.
#Frühling: 1. 3. bis 31. 5. 
#Sommer: 1. 6. bis 31. 8.
#Herbst: 1.9 bis 20.11

#Spalte Jahreszeit cli_gwl_1971 hinzufügen

cli_gwl_1971 <- cli_gwl_1971 %>%
  mutate(Jahreszeit = case_when(month %in% c("12", "01", "02") ~ "Winter",
                             month %in% c("03", "04", "05") ~ "Fruehling",
                             month %in% c("06", "07", "08") ~ "Sommer",
                             month %in% c("09", "10", "11") ~ "Herbst"))
setcolorder(cli_gwl_1971,c("index_length_gwl","id","Jahreszeit"))

#Laenge der GWLs nach Jahreszeit gruppiert berechnen

cli_gwl_1971 <- as.data.table(cli_gwl_1971)
gwlNachJahreszeit <- cli_gwl_1971[,(rle(gwl)), by = Jahreszeit]


# Anzahl der GWLs gruppiert nach Jahreszeit

table(gwlNachJahreszeit$values,gwlNachJahreszeit$Jahreszeit)


#####################################
#Unterscheidet sich der erste und letzte Tag einer GWL?

# Alle GWLs, die weniger als 4 Tage andauern, löschen
clii <- cli_gwl_1971[,.(.N), by = index_length_gwl]
cli_gwl_1971 <- merge(clii, cli_gwl_1971,by = "index_length_gwl")

 cli_gwl_groesser3 <- cli_gwl_1971 %>%
  group_by(index_length_gwl) %>%
  filter(N >3)
cli_gwl_groesser3 <- as.data.table(cli_gwl_groesser3)


 # Mittelwert über alle Messpunkte der verschiedenen Standorte
 cli_gwl_mean <- cli_gwl_groesser3 %>%
   select(index_length_gwl,id, Jahreszeit,date, year, month,day,gwl) %>%
       mutate(mslp_mean = rowMeans(cli_gwl_groesser3[,9 : 168]),
             geo_mean = rowMeans(cli_gwl_groesser3[,169 : 328])) 
 # sinnvoll, das so runterzukuerzen? Geht ja schon viel info verloren?


 
cli_gwl_mean1 <- cli_gwl_mean %>%
  select(index_length_gwl,mslp_mean)


for( gwl_number in 1 : 20){
 plot(cli_gwl_mean1$mslp_mean[cli_gwl_mean1$index_length_gwl == gwl_number])
}
######Viel zu viele Plots, nicht praktikabel


# Differenzen bestimmen über gemittelte Standorte
 cli_gwl_mean <- cli_gwl_mean %>%
   group_by(index_length_gwl) %>%
   mutate(mslp_diff = mslp_mean - lag(mslp_mean),
          geo_diff = geo_mean - lag(geo_mean))
 
 
 #Differenzen bestimmen über alle Standorte und dann einen Filtern setzen, wenn Differenz 
 # bestimmten wert übersteigt
 

 

 
 

 
 


