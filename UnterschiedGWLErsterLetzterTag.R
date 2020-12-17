library(data.table)
library(ggplot2)
library(data.table)
library(stringr)
library(tidyverse)
library(dplyr)
library(plyr)






###########################################
#Datenvorbereitung

#GWL data
gwl_data <- as.data.table(read.table("Data/GWL_1900-2010.csv", header = TRUE, sep = ";",
                                     na.strings = " "))

#wide to long mit kalendarischer Abfolge des Datums

gwl_data_long <- as.data.frame(t(gwl_data))
colnames(gwl_data_long) <- gwl_data$JAHRMONAT
gwl_data_long <- gwl_data_long[-1,]
day <- seq(from = 1, to = 31, by = 1)
gwl_data_long <- cbind(day, gwl_data_long)
gwl_data_long <- as.data.table(gather(data = gwl_data_long,key = JAHRMONAT, value = gwl, -day))

#Remove empty rows

gwl_data_long <- gwl_data_long[!(apply(gwl_data_long, 1, function(y) any(y == ""))),]



#split year and month
gwl_data_long[, ":=" (year = substr(JAHRMONAT, 1, 4), 
                  month = substr(JAHRMONAT, 5, 6))][, JAHRMONAT := NULL]
#rename and pad day
gwl_data_split <- copy(gwl_data_long[, day := str_pad(sub("X", "", day), 2, "left", "0")])
#fuse to date
gwl_data_long[, date := as.Date(paste(year, month, day, sep = "-"))]

#reorder cols
setcolorder(gwl_data_long, c("date", "day","month","year","gwl"))

saveRDS(gwl_data_long,file = "Data\\gwl_data1.rds")

#subset timeframe

gwl1971 <- gwl_data1[year %in% seq(1971, 2010), ]
saveRDS(gwl1971, file ="Data\\gwl1971.rds")


##############reanalysis data 


cli_data <- readRDS("Data/cli_data.rds")
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
# GWL WZ kommt am haufigsten vor: 345 mal


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
                             month %in% c("09", "10", "11") ~ "Herbst") )
setcolorder(cli_gwl_1971,c("index_length_gwl","id","Jahreszeit")) 

#Laenge der GWLs nach Jahreszeit gruppiert berechnen

cli_gwl_1971 <- as.data.table(cli_gwl_1971)
gwlNachJahreszeit <- cli_gwl_1971[,(rle(gwl)), by = Jahreszeit]
table(gwlNachJahreszeit$values,gwlNachJahreszeit$Jahreszeit)
colSums(table(gwlNachJahreszeit$values,gwlNachJahreszeit$Jahreszeit))

# Anzahl der GWLs gruppiert nach Jahreszeit

GWLJahreszeiten <- as.data.table(table(gwlNachJahreszeit$values,gwlNachJahreszeit$Jahreszeit))
barplot(t(table(gwlNachJahreszeit$values,gwlNachJahreszeit$Jahreszeit)), col = terrain.colors(4))
mosaicplot(t(table(gwlNachJahreszeit$values,gwlNachJahreszeit$Jahreszeit)), col = terrain.colors(31), las = 1)
mosaicplot(table(gwlNachJahreszeit$values,gwlNachJahreszeit$Jahreszeit), col = terrain.colors(4), las = 1)

barplot(colSums(table(gwlNachJahreszeit$values,gwlNachJahreszeit$Jahreszeit)), col = rainbow(4))

# Im Frühling gibt am meisten GWL, abnehmende Anzahl der GWLs: Herbst, Sommer, Winter
# Zusammensetzung der GWLs in den Jahreszeiten unterschiedlich

#####################################
#Unterscheidet sich der erste und letzte Tag einer GWL?

dbinom(2, size = 8, prob = 1/8)

# Alle GWLs, die weniger als 4 Tage andauern, löschen
clii <- cli_gwl_1971[,.(.N), by = index_length_gwl]
cli_gwl_1971 <- merge(clii, cli_gwl_1971,by = "index_length_gwl")

 cli_gwl_groesser3 <- cli_gwl_1971 %>%
  group_by(index_length_gwl) %>%
  filter(N >3)
cli_gwl_groesser3 <- as.data.table(cli_gwl_groesser3)


 # Mittelwert über alle Messpunkte der verschiedenen Standorte
 cli_gwl_mean <- cli_gwl_groesser3 %>%
   select(index_length_gwl,N,id, Jahreszeit,date, year, month,day,gwl) %>%
       mutate(mslp_mean = rowMeans(cli_gwl_groesser3[,10 : 169]),
             geo_mean = rowMeans(cli_gwl_groesser3[,170 : 329])) 
 # sinnvoll, das so runterzukuerzen? Geht ja schon viel info verloren?
 
 # Differenzen berechnen
 cli_gwl_mean <- cli_gwl_mean %>%
   group_by(index_length_gwl) %>%
   mutate(mslp_diff = mslp_mean - lag(mslp_mean),
          geo_diff = geo_mean - lag(geo_mean))
 


 
cli_gwl_mean1 <- cli_gwl_mean %>%
  select(index_length_gwl,mslp_mean)


for( gwl_number in 1 : 20){
 plot(cli_gwl_mean1$mslp_mean[cli_gwl_mean1$index_length_gwl == gwl_number])
}
######Viel zu viele Plots, nicht praktikabel




 
 
 #Differenzen bestimmen über alle Standorte und dann einen Filtern setzen, wenn Differenz 
 # bestimmten wert übersteigt und mit dann diese GWLs ausgeben lassen 

cli_gwl_diff <- cli_gwl_groesser3[, lapply(.SD, function(x) x - lag(x)), by = index_length_gwl, .SDcols = 10:329] 

# Mittelwerte berechnen außer erstem und letztem Tag innerhalb eines GWLs

cli_gwl_first_last <- ddply(cli_gwl_groesser3, .(index_length_gwl), function(x) x[c(1, nrow(x)), ])
cli_gwl_first <- ddply(cli_gwl_groesser3, .(index_length_gwl), function(x) x[1, ]) 
cli_gwl_last <- ddply(cli_gwl_groesser3, .(index_length_gwl), function(x) x[nrow(x), ]) 
cli_gwl_inner <- ddply(cli_gwl_groesser3, .(index_length_gwl),function(x) x[c(2 : (nrow(x) -1)), ])
cli_gwl_inner <- as.data.table(cli_gwl_inner)
cli_gwl_mean <- cli_gwl_inner[, lapply(.SD, mean), .SDcols = 10 : 329, by = index_length_gwl]
cli_gwl_SD <- cli_gwl_inner[, lapply(.SD, sd), .SDcols = 10 : 329, by = index_length_gwl]

cli_gwl_first1 <- cli_gwl_first[,-c(2:9)]
cli_gwl_last1 <- cli_gwl_last[ ,-c(2:9)]

#Funktion schreiben, die ersten und letzten Tag mit Mittelwert + Standardabweichung
# einer GWL ohne ersten und letzten Tag vergleicht.



# Ist 1. Tag im intervall [mean - sd, mean + sd] enthalten?


unterschied_first <- matrix(ncol = ncol(cli_gwl_first1), nrow = nrow(cli_gwl_first1))

for (i in seq_len(ncol(cli_gwl_mean) -1)){
  for ( j in seq_len(nrow(cli_gwl_first1))){
    if(cli_gwl_first1 [j, (1 + i)] < (cli_gwl_mean[j,(1 + i)]) - (cli_gwl_SD[j,(1 + i)])){
      unterschied_first [j,(1 + i)] <- cli_gwl_first1[j, (1 + i)]
    }
    else if(cli_gwl_first1 [j, (1 + i)] > (cli_gwl_mean[j,(1 + i)]) + (cli_gwl_SD[j,(1 + i)])){
      unterschied_first [j,(1 + i)] <- cli_gwl_first1[j, (1 + i)]
    }  
  }
  print("Spalte durchlaufen")
}
unterschied_first <- as.data.table(unterschied_first)
unterschied_first <- cbind(cli_gwl_first[,1:9], unterschied_first)

# Ist 1. Tag im intervall [mean - sd, mean + sd] enthalten?


unterschied_last <- matrix(ncol = ncol(cli_gwl_last1), nrow = nrow(cli_gwl_last1))

for (i in seq_len(ncol(cli_gwl_mean) -1)){
  for ( j in seq_len(nrow(cli_gwl_last1))){
    if(cli_gwl_last1 [j, (1 + i)] < (cli_gwl_mean[j,(1 + i)]) - (cli_gwl_SD[j,(1 + i)])){
      unterschied_last [j,(1 + i)] <- cli_gwl_last1[j, (1 + i)]
    }
    else if (cli_gwl_last1 [j, (1 + i)] < (cli_gwl_mean[j,(1 + i)]) - (cli_gwl_SD[j,(1 + i)])){
      unterschied_last [j,(1 + i)] <- cli_gwl_last1[j, (1 + i)]
    }
  }
  print("Spalte durchlaufen")
}
unterschied_last <- as.data.table(unterschied_last)
unterschied_last <- cbind(cli_gwl_last[,1:9], unterschied_last)

#mean + sd lieber als intervall, also [mean -SD, mean + SD] und schauen, ob
# 1. und letzter Tag in diesem Intervall ist

#Eventuell statt 1 mal Standardabweicung 2 Standardabweichungen hinzunehmen?
#Differenzen vergleichen: also Differenz 1. Tag zu Mitte und letzter Tag zur Mitte 
#größer als Differenzen innterhalb der Mitte?


# #########Range pro Tag ueber alle Standorte berechnen 

rangeProTag <- as.data.table(t(apply(cli_gwl_groesser3[,10:169], 1, range)))
colnames(rangeProTag) <- c("minMslp", "maxMslp")
rangeProTag <- cbind(cli_gwl_groesser3[,1:9],rangeProTag)

rangeProTagGeo <- as.data.table(t(apply(cli_gwl_groesser3[,170 : 329], 1, range)))
colnames(rangeProTagGeo) <- c("minGeo", "maxGeo")

rangeProTag <- cbind(rangeProTag,rangeProTagGeo)


######Range pro GWL üeber alle Standorte pro GWL 
#Datenvorbereitung
rangeProTagMslp <- as.data.table(t(apply(cli_gwl_groesser3[,10:169], 1, range)))
colnames(rangeProTagMslp) <- c("minMslp", "maxMslp")
rangeProTagMslp <- cbind(cli_gwl_groesser3[,1:9],rangeProTagMslp)
rangeProTagMslpLong <- melt(rangeProTagMslp, id.vars = c("index_length_gwl","id","Jahreszeit",
                                                         "date","year","month","day","gwl"), measure.vars = c("minMslp","maxMslp"))
rangeProTagMslppLong <- rangeProTagMslpLong[order(rangeProTagMslpLong$date),]

colnames(rangeProTagMslpLong)[colnames(rangeProTagMslpLong) == "variable"] <- "MinMaxMslp"
colnames(rangeProTagMslpLong)[colnames(rangeProTagMslpLong) == "value"] <- "Mslp"



rangeProTagGeo <- cbind(cli_gwl_groesser3[,1:9], rangeProTagGeo)
rangeProTagGeoLong <- melt(rangeProTagGeo, id.vars = c("index_length_gwl","id","Jahreszeit","date",
                                                         "year","month","day","gwl"), measure.vars = c("minGeo","maxGeo"))

rangeProTagGeoLong <- rangeProTagGeoLong[order(rangeProTagGeoLong$date),]
colnames(rangeProTagGeoLong)[colnames(rangeProTagGeoLong) == "variable"] <- "MinMaxGeo"
colnames(rangeProTagGeoLong)[colnames(rangeProTagGeoLong) == "value"] <- "Geo"

ranges <- cbind(rangeProTagMslpLong, rangeProTagGeoLong[,9 : 10])

#Berechnung

rangesProGwl <- ranges[, lapply(.SD, function(x) range(x)), by = index_length_gwl, .SDcols = c(10,12)] 
rangesDiff <- rangesProGwl[,lapply(.SD, function(x) x - lag(x)),by = index_length_gwl, .SDcols = c(2,3)]
rangesDiff <- na.omit(rangesDiff)

#######Berechung der ranges über alle Standorte pro GWL ohne den ersten und letzten Tag


#Datenvorbereitung
rangeProTagMslpinner <- as.data.table(t(apply(cli_gwl_inner[,10:169], 1, range)))
colnames(rangeProTagMslpinner) <- c("minMslp", "maxMslp")
rangeProTagMslpinner <- cbind(cli_gwl_inner[,1:9],rangeProTagMslpinner)
rangeProTagMslpLonginner <- melt(rangeProTagMslpinner, id.vars = c("index_length_gwl","id","Jahreszeit",
                                                         "date","year","month","day","gwl"), measure.vars = c("minMslp","maxMslp"))
rangeProTagMslppLonginnerinner <- rangeProTagMslpLonginner[order(rangeProTagMslpLonginner$date),]

colnames(rangeProTagMslpLonginner)[colnames(rangeProTagMslpLonginner) == "variable"] <- "MinMaxMslp"
colnames(rangeProTagMslpLonginner)[colnames(rangeProTagMslpLonginner) == "value"] <- "Mslp"

rangeProTagGeoinner <- as.data.table(t(apply(cli_gwl_inner[,170 : 329], 1, range)))
colnames(rangeProTagGeoinner) <- c("minGeo", "maxGeo")

rangeProTagGeoinner <- cbind(cli_gwl_inner[,1:9], rangeProTagGeoinner)
rangeProTagGeoLonginner <- melt(rangeProTagGeoinner, id.vars = c("index_length_gwl","id","Jahreszeit","date",
                                                       "year","month","day","gwl"), measure.vars = c("minGeo","maxGeo"))


rangeProTagGeoLonginner <- rangeProTagGeoLonginner[order(rangeProTagGeoLonginner$date),]
colnames(rangeProTagGeoLonginner)[colnames(rangeProTagGeoLonginner) == "variable"] <- "MinMaxGeo"
colnames(rangeProTagGeoLonginner)[colnames(rangeProTagGeoLonginner) == "value"] <- "Geo"

rangesinner <- cbind(rangeProTagMslpLonginner, rangeProTagGeoLonginner[,9 : 10])

#Berechnung

rangesProGwlinner <- rangesinner[, lapply(.SD, function(x) range(x)), by = index_length_gwl, .SDcols = c(10,12)] 
rangesDiffinner <- rangesProGwlinner[,lapply(.SD, function(x) x - lag(x)),by = index_length_gwl, .SDcols = c(2,3)]
rangesDiffinner <- na.omit(rangesDiffinner)
colnames(rangesDiffinner) <- c("index_length_gwl","MslpInner","GeoInner")
rangesGWL <- merge(rangesDiff, rangesDiffinner, by = "index_length_gwl")

##### Vergleich von ranges und ranges ohne ersten und letzten Tag


boxplot(rangesGWL[,c(2,4,3,5)])
# range des luftdrucks mit und ohne ersten und letzten Tag eines GWL scheinen
#gleich zu sein, im Geopotential gibt es Unterschiede

###########Durchführen eines Signifikanztest, ob Unterschiede signifikannt sind

# Prüfen auf Testvoraussetzungen
#Normalverteilung?

ks.test(rangesGWL$Geo,"pnorm", mean = mean(rangesGWL$Geo), sd = sd(rangesGWL$Geo))

# keine Normalverteilung

#Varianzen gleich?
var.test(rangesGWL$Geo,rangesGWL$GeoInner)
# keine Varianzgleichheit

# Wech Test
t.test(rangesGWL$Geo, rangesGWL$GeoInner, alternative = "two.sided" , 
       paired = T, var.equal = F)
# Die Mittelwerte der ranges mit und ohne die ersten und letzten Tage bei 
# Geopotential sind signifikannt unterschiedlich

ks.test(rangesGWL$Mslp,"pnorm", mean = mean(rangesGWL$Mslp), sd = sd(rangesGWL$Mslp))
var.test(rangesGWL$Mslp,rangesGWL$MslpInner)
t.test(rangesGWL$Mslp, rangesGWL$MslpInner, alternatlive = "two.sided" , 
       paired = T, var.equal = T)

# Mittelwerte mit und ohne ersten und letzten Tag bei Luftdruck sind signifiaknnt 
#unterschiedlich

#Gibt es bestimmte Tage, wo die range besonders groß ist? Alle Werte herausfiltern,
# die nicht im IQR liegen

IQR(rangesGWL$Mslp)
IQR(rangesGWL$MslpInner)

IQR(rangesGWL$Geo)
IQR(rangesGWL$GeoInner)
quantile(rangesGWL$Mslp, 0.25)

# IQR der ranges sind sehr gleich
# schauen, wie viele ranges der GWL im IQR derranges der GWL ohne ersten und letzten Tag drin ist   
a <- rangesGWL$Mslp[rangesGWL$Mslp  %inrange% c(quantile(rangesGWL$MslpInner, 0.25), quantile(rangesGWL$MslpInner, 0.75))]
a <- rangesGWL$Geo[rangesGWL$Geo  %inrange% c(quantile(rangesGWL$GeoInner, 0.25), quantile(rangesGWL$GeoInner, 0.75))]

