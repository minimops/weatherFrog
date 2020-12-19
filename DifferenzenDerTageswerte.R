library(matrixStats)
library(ggplot2)
library(dplyr)

library(dplyr)

data <- readRDS("Data\\data_reanalysis_20201109.rds")
data$time
wetterlagen <- read.csv("Data\\GWL_1900-2010.csv",sep =";", na.strings = " ", header = T)

# Jahre 2000 bis 2010 audwählen

wetterlagen <- wetterlagen[wetterlagen$JAHRMONAT >= 200001,]

## Datensatz in Date und Time aufteilen
data$date <- as.Date(data$time, tz = "CET")
data$time <- format(data$time, "%H:%M:%S")


# Zeitverschiebung anpassen
library(dplyr)
data00 <- data %>% filter(time == "00:00:00" | time == "01:00:00" | time == "02:00:00")
data06 <- data %>% filter(time == "06:00:00" | time == "07:00:00" | time == "08:00:00") 
data12 <- data %>% filter(time == "12:00:00" | time == "13:00:00" | time == "14:00:00")
data18 <- data %>% filter(time == "18:00:00" | time == "19:00:00" | time == "20:00:00")

data00$time <- "00:00:00"
data06$time <- "06:00:00"
data12$time <- "12:00:00"
data18$time <- "18:00:00"


data <- rbind(data00, data06, data12, data18)

dataSubset <- data[data$date >= "2006-01-01" & data$date <= "2010-12-31", ]

# long to wide format pro Tag
dataLong <- reshape(dataSubset,
                    idvar = c("longitude", "latitude", "date"),
                    timevar = "time",
                    direction = "wide")

dataLong <- dataLong[,c(3,1,2,4,6,8,10,5,7,9,11)]
names(dataLong) <- c("date", "longitude", "latitude","mslp.00","mslp.06","mslp.12", "mslp.18","geopotential.00", "geopotential.06","geopotential.12", "geopotential.18")


# nur 2006 bis 2010
dataLong2006 <- dataLong[dataLong$date>= "2006-01-01" & dataLong$date <= "2010-12-31",]
dataSubset <- dataSubset[dataSubset$date>= "2006-01-01" & dataSubset$date <= "2010-12-31",]


# Tagesdifferenzen

diffMslp0600 <- dataLong2006$mslp.06 - dataLong2006$mslp.00
diffMslp1206 <- dataLong2006$mslp.12 - dataLong2006$mslp.06
diffMslp1812 <- dataLong2006$mslp.18 - dataLong2006$mslp.12
diffMsl1800 <- dataLong2006$mslp.18 - dataLong2006$mslp.00


diffMslp <- as.data.frame(cbind(dataLong2006$longitude,dataLong2006$latitude,
                                diffMslp0600,diffMslp1206, diffMslp1812,diffMsl1800))
diffMslp <- cbind(dataLong2006$date,diffMslp)
names(diffMslp) <- c("date","longitude", "latitude","diffMslp0600", "diffMslp1206", "diffMslp1812","diffMslp1800") 

x <- seq(from = 1, to = 160, by = 1)
292160 / 160
y <- rep(x, times = 1826)
diffMslp$Group <- as.character(y)


diffGeo0600 <- dataLong2006$geopotential.06 - dataLong2006$geopotential.00
diffGeo1206 <- dataLong2006$geopotential.12 - dataLong2006$geopotential.06
diffGeo1812 <- dataLong2006$geopotential.18 - dataLong2006$geopotential.12
diffGeo1800 <- dataLong2006$geopotential.18 - dataLong2006$geopotential.00

diffGeo <- as.data.frame(cbind(dataLong2006$longitude,dataLong2006$latitude,
                                diffGeo0600,diffGeo1206, diffGeo1812,diffGeo1800))
diffGeo <- cbind(dataLong2006$date,diffGeo)
names(diffGeo) <- c("date","longitude", "latitude","diffGeo0600", "diffGeo1206", "diffGeo1812","diffGeo1800") 
diffGeo$Group <- as.character(y)


#Tagesminimum und Tagesmaximum, mean usw.

MslpMin <- rowMins(as.matrix(dataLong2006[,c(4,5,6,7)]))
MslpMax <- rowMaxs(as.matrix(dataLong2006[,c(4,5,6,7)]))
MslpMean <- rowMeans(as.matrix(dataLong2006[,c(4,5,6,7)]))
MslpSD <- rowSds(as.matrix(dataLong2006[,c(4,5,6,7)]))
MslpQuantile <- rowQuantiles(as.matrix(dataLong2006[,c(4,5,6,7)]))

statisticsMslp <- as.data.frame(cbind(dataLong2006$longitude,dataLong2006$latitude,MslpMean,MslpSD,MslpMin,
                                      MslpMax,MslpQuantile[,c(2,3,4)]))
statisticsMslp <- cbind(dataLong2006$date,statisticsMslp)
names(statisticsMslp) <- c("date","longitude", "latitude","MslpMean", "MslpSD","MslpMin", "MslpMax", "Mslp25%", "Mslp50%","Mslp75%")
statisticsMslp$range <- statisticsMslp[,7] - statisticsMslp[,6]
statisticsMslp$Group <- as.character(y)

GeoMin <- rowMins(as.matrix(dataLong2006[,c(8,9,10,11)]))
GeoMax <- rowMaxs(as.matrix(dataLong2006[,c(8,9,10,11)]))
GeoMean <- rowMeans(as.matrix(dataLong2006[,c(8,9,10,11)]))
GeoSD <- rowSds(as.matrix(dataLong2006[,c(8,9,10,11)]))
GeoQuantile <- rowQuantiles(as.matrix(dataLong2006[,c(8,9,10,11)]))


statisticsGeo <- as.data.frame(cbind(dataLong2006$longitude,dataLong2006$latitude,GeoMean,GeoSD,GeoMin,
                                      GeoMax,GeoQuantile[,c(2,3,4)]))
statisticsGeo <- cbind(dataLong2006$date,statisticsGeo)
names(statisticsGeo) <- c("date","longitude", "latitude","GeoMean", "GeoSD","GeoMin", "GeoMax", "Geo25%", "Geo50%","Geo75%")
statisticsGeo$range <- statisticsGeo[,7] - statisticsGeo[,6]
statisticsGeo$Group <- as.character(y)

#Visualisierung von Tagesunterschieden

dataLong112006 <- dataLong2006[dataLong2006$date == "2006-01-01",]
dataSubset112006 <- dataSubset[dataSubset$date == "2006-01-01",]


# Boxplots von Geopotential und Luftdruck pro Tag unterteilt in die 
# 4 Tagesmesswerte

dailyBoxplot <- function(date){
dataSubset112006 <- dataSubset[dataSubset$date == date,]

par(mfrow=c(1,2))
boxplot(mslp ~ time, data = dataSubset112006, ylim = c(0,104127)) 
boxplot(geopotential ~ time, data = dataSubset112006, ylim = c(0,57127)) 

boxplot(mslp ~ time, data = dataSubset112006) 
boxplot(geopotential ~ time, data = dataSubset112006) 

}
dailyBoxplot("2006-02-02")



# 1 Jahr ein Standort range der Tagesmessungen
statisticsGeo1 <- statisticsGeo %>%
  filter(date >= "2010-01-01",
        Group == "1" )

p <- ggplot(data = statisticsGeo1, aes(x = date, y = range))
p + geom_line(col = "blue")

# 1 Jahr ein Standort min und max der Tagesmessungen
ggplot() +
  geom_line(data = statisticsGeo1, aes ( x = date, y = GeoMin), col ="green") +
 geom_line(data = statisticsGeo1, aes(x = date, y = GeoMax), col = "red")

# einen Messpunkt über 5 Jahre plotten mit Differenz 6 Uhr - 0 Uhr 
# der Variable Luftdruck

diffMslp1 <- diffMslp[diffMslp$Group == "1",]

plot(diffMslp1$date,diffMslp1$diffMslp0600)
diffMslp$Group <- as.character(y)
  
p <- ggplot(data = diffMslp1, aes(x = date, y = diffMslp0600))
  p + geom_line(col = "blue")
