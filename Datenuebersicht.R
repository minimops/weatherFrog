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

 dataSubset <- data[data$date >= "2000-01-01" & data$date <= "2010-12-31", ]

  # long to wide format pro Tag
 dataLong <- reshape(dataSubset,
                     idvar = c("longitude", "latitude", "date"),
                     timevar = "time",
                     direction = "wide")
 
 dataLong <- dataLong[,c(3,1,2,4,6,8,10,5,7,9,11)]
 names(dataLong) <- c("date", "longitude", "latitude","mslp.00","mslp.06","mslp.12", "mslp.18","geopotential.00", "geopotential.06","geopotential.12", "geopotential.18")

 
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