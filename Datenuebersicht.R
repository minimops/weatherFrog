setwd("C:/Users/asus/Documents/StatistikBio/5.Semester/StatistischesPraktikum/Daten")

library(dplyr)

data <- readRDS(file = "data_reanalysis_20201109.rds")
data$time
wetterlagen <- read.csv("GWL_1900-2010.csv",sep =";", na.strings = " ", header = T)

# Jahre 2000 bis 2010 audwählen

wetterlagen <- wetterlagen[wetterlagen$JAHRMONAT >= 200001,]

## Datensatz in Date und Time aufteilen
data$date <- as.Date(data$time, tz = "CET")

data$time <- format(data$time, "%H:%M:%S")
 as.data.frame(table(data$time))
# wegen Zeitverschiebung auch 1.00, 2.00....
 
 dataSubset <- data[data$date >= "2000-01-01" & data$date <= "2010-12-31", ]
 as.data.frame(table(dataSubset$time))

# Zusammenfassen von = Uhr, 1 Uhr und 2 Uhr...?
 
#library(dplyr)
#data06 <- data %>% filter(time == "06:00:00" | time == "07:00:00") 
#dim(data06)

#dataSubset <- data06[data06$date >= "2000-01-01" & data06$date <= "2010-12-31", ]

saveRDS(dataSubset, "C:/Users/asus/Documents/StatistikBio/5.Semester/StatistischesPraktikum/Daten/reanalysis.rds")

#Verteilung

hist(dataSubset$mslp)
summary(dataSubset$mslp)

ks.test(dataSubset$mslp,"dnorm",mean = mean(dataSubset$mslp), sd = sd(dataSubset$mslp))
#keine Normalverteilung

hist(dataSubset$geopotential)
summary(dataSubset$geopotential)
ks.test(dataSubset$geopotential,"dnorm", mean = (dataSubset$geopotential), sd = sd(dataSubset$geopotential))
#keine Normalverteilung

# Häufigkeit der Messungen und Anzahl der Messpunkte

a <- as.data.frame(table(dataSubset$longitude,dataSubset$latitude))
# 160 Messpunkte 
b <- as.data.frame(table(data$longitude))
c <- as.data.frame(table(data$latitude))

data20000 <- dataSubset[dataSubset$date == "2000-01-01",]
#Pro Tag 4 Messungen für jeden Standort, also 640 Messungen an einem Tag 


#Visualisierung der Messpunkte in einer Europakarte 


#Prüfen auf Unabhängigkeit der Variablen geopotential und mslp

cov(data20000$mslp,data20000$geopotential)
cor(data20000$mslp,data20000$geopotential)

cov(dataSubset$mslp,dataSubset$geopotential)
cor(dataSubset$mslp,dataSubset$geopotential)
plot(data20000$mslp,data20000$geopotential)
d <- as.data.frame(cbind(data20000$mslp,data20000$geopotential))
cov(d)
#Korrleaion ungleich 1
#Covarianzen in KOvarianzmatrix ungleich 0
#Im Scatterplt linearer Trend zwischen Geopotential und mslp ersichtlich
### Keine Unabhängigkeit zwischen beiden Variablen 