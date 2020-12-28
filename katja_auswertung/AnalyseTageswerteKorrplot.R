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

dataSubset <- data[data$date >= "1971-01-01" & data$date <= "2010-12-31", ]

#Verteilung

hist(dataSubset$mslp)
summary(dataSubset$mslp)

ks.test(dataSubset$mslp,"dnorm",mean = mean(dataSubset$mslp), sd = sd(dataSubset$mslp))
#keine Normalverteilung

hist(dataSubset$geopotential)
summary(dataSubset$geopotential)
ks.test(dataSubset$geopotential,"dnorm", mean = (dataSubset$geopotential), sd = sd(dataSubset$geopotential))
#keine Normalverteilung

#Prüfen auf Unabhängigkeit der Variablen geopotential und mslp

cov(data20000$mslp,data20000$geopotential)
cor(data20000$mslp,data20000$geopotential)

cov(dataSubset$mslp,dataSubset$geopotential)
cor(dataSubset$mslp,dataSubset$geopotential)
jpeg(width = 1500,height =1000, pointsize = 29,quality = 100,"plots/corPlotMslpGeo.jpeg")
plot(dataSubset$mslp,dataSubset$geopotential, main = "Korrelation zwischen Luftdruck und Geopotential (2006 bis 2010)",
     xlab = "Luftdruck in Pa", ylab = "Geopotential in m²/s²",cex.axis = 1.5, cex.main = 1.5, cex.axis = 1.5)
dev.off()

#Korrelationsmatrix
d <- as.data.frame(cbind(dataSubset$mslp,dataSubset$geopotential))

###Tageswertevergleich
jpeg(width = 2000,height =1000, pointsize = 29,quality = 100,"plots/VergleichTageswerteMslp.jpeg")
boxplot(dataSubset$mslp ~ dataSubset$time, main = "Tageswertvergleiche der Jahre 2006 bis 2010",
        xlab = "Messzeitpunkt", ylab = "Luftdruck in Pa", cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5)
dev.off()

jpeg(width = 2000,height =1000, pointsize = 29,quality = 100,"plots/VergleichTageswerteGeo.jpeg")
boxplot(dataSubset$geopotential ~ dataSubset$time, main = "Tageswertvergleiche der Jahre 2006 bis 2010",
        xlab = "Messzeitpunkt", ylab = "Geopotential im m²/s²", cex.lab = 1.5,cex.main = 1.5, cex.axis = 1.5)
dev.off()
cov(d)