library(matrixStats)




dataLong <- readRDS(file = "C:/Users/asus/Documents/StatistikBio/5.Semester/StatistischesPraktikum/Daten/dataLong") 

# nur 2006 bis 2010
dataLong2006 <- dataLong[dataLong$date>= "2006-01-01" & dataLong$date <= "2010-12-31",]

# Tagesdifferenzen

diffMslp0600 <- dataLong2006$mslp.06 - dataLong2006$mslp.00
diffMslp1206 <- dataLong2006$mslp.12 - dataLong2006$mslp.06
diffMslp1812 <- dataLong2006$mslp.18 - dataLong2006$mslp.12
diffMsl1800 <- dataLong2006$mslp.18 - dataLong2006$mslp.00


diffMslp <- as.data.frame(cbind(dataLong2006$longitude,dataLong2006$latitude,
                                diffmslp0600,diffMslp1206, diffMslp1812,diffMsl1800))
diffMslp <- cbind(dataLong2006$date,diffMslp)
names(diffMslp) <- c("date","longitude", "latitude","diffMslp0600", "diffMslp1206", "diffMslp1812","diffMslp1800") 


diffGeo0600 <- dataLong2006$geopotential.06 - dataLong2006$geopotential.00
diffGeo1206 <- dataLong2006$geopotential.12 - dataLong2006$geopotential.06
diffGeo1812 <- dataLong2006$geopotential.18 - dataLong2006$geopotential.12
diffGeo1800 <- dataLong2006$geopotential.18 - dataLong2006$geopotential.00

diffGeo <- as.data.frame(cbind(dataLong2006$longitude,dataLong2006$latitude,
                                diffGeo0600,diffGeo1206, diffGeo1812,diffGeo1800))
diffGeo <- cbind(dataLong2006$date,diffGeo)
names(diffGeo) <- c("date","longitude", "latitude","diffGeo0600", "diffGeo1206", "diffGeo1812","diffGeo1800") 



#Tagesminimum und Tagesmaximum, mean usw.

MslpMin <- rowMins(as.matrix(dataLong2006[,c(4,5,6,7)]))
MslpMax <- rowMaxs(as.matrix(dataLong2006[,c(4,5,6,7)]))
MslpMean <- rowMeans(as.matrix(dataLong2006[,c(4,5,6,7)]))
MslpSD <- rowSds(as.matrix(dataLong2006[,c(4,5,6,7)]))
MslpQuantile <- rowQuantiles(as.matrix(dataLong2006[,c(4,5,6,7)]))

statisticsMslp <- as.data.frame(cbind(dataLong2006$date,dataLong2006$longitude,dataLong2006$latitude,MslpMean,MslpSD,MslpMin,
                                      MslpMax,MslpQuantile[,c(2,3,4)]))
statisticsMslp <- cbind(dataLong2006$date,statisticsMslp)
names(statisticsMslp) <- c("date","longitude", "latitude","MslpMean", "MslpSD","MslpMin", "MslpMax", "Mslp25%", "Mslp50%","Mslp75%")



GeoMin <- rowMins(as.matrix(dataLong2006[,c(8,9,10,11)]))
GeoMax <- rowMaxs(as.matrix(dataLong2006[,c(8,9,10,11)]))
GeoMean <- rowMeans(as.matrix(dataLong2006[,c(8,9,10,11)]))
GeoSD <- rowSds(as.matrix(dataLong2006[,c(8,9,10,11)]))
GeoQuantile <- rowQuantiles(as.matrix(dataLong2006[,c(8,9,10,11)]))


statisticsGeo <- as.data.frame(cbind(dataLong2006$date,dataLong2006$longitude,dataLong2006$latitude,GeoMean,GeoSD,GeoMin,
                                      GeoMax,GeoQuantile[,c(2,3,4)]))
statisticsGeo <- cbind(dataLong2006$date,statisticsGeo)
names(statisticsGeo) <- c("date","longitude", "latitude","GeoMean", "GeoSD","GeoMin", "GeoMax", "Geo25%", "Geo50%","Geo75%")

#Visualisierungen






