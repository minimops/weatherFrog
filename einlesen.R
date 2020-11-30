################# Statistisches Praktikum ##################
#### WHEATHER FROG

## Daten einlesen 

data <- readRDS("Data\\data_reanalysis_20201109.rds")

# Wenn man head(data) ausgibt, wird die Uhrzeit nicht mit angezeigt... find ich bisschen komisch..
head(data, 20)
str(data)

wetterlagen <- read.csv("C:\\Users\\HP\\Documents\\weatherFrog\\StatistischesPraktikum\\GWL_1900-2010.csv", header = TRUE,
                   sep = ";", na.strings = " ")

head(wetterlagen)

# Hier wähle ich die Daten aus den Jahren 2000 - 2010 aus
wetterSubset <- tail(wetterlagen, 11*12)
# Start am 01.01.00
head(wetterSubset)
# Ende am 31.12.10
tail(wetterSubset)

# 
unique(unlist(apply(wetterSubset[2:32], 1, unique)))


# Datensatz in Date und Time aufteilen
data$date <- as.Date(data$time, tz = "CET")

data$time <- format(data$time, "%H:%M:%S")
tail(data)
head(data)
table(data$time)


str(data)
library(dplyr)
data06 <- data %>% filter(time == "06:00:00" | time == "07:00:00") 
dim(data06)

dataSubset <- data06[data06$date >= "2000-01-01" & data06$date <= "2010-12-31", ]
head(dataSubset)
dim(dataSubset)
# so viele zeilen sollten es sein oder?
11*365*160 + 3*160
#  3* 160 wegen Schaltjahr 
# -> passt! wird wohl an Zeitverscheibung liegen
library(data.table)

DTdataSubset <- as.data.table(dataSubset)
dim(DTdataSubset)


# save data
saveRDS(DTdataSubset,"Data\\dataSubset2000-2010.rds")

hist(DTdataSubset$mslp)
# kann man evtl als normalverteilt annehmen? Wie weiß man sowas?
hist(DTdataSubset$geopotential)
# eher eine linksschiefe Verteilung


# more effective way:
plot(density(DTdataSubset$mslp))
# -> auch ein wenig linksschief
plot(density(DTdataSubset$geopotential))
# -> puh, schwierig

mean(DTdataSubset$mslp)
# Mittelwert: 101356.9
mean(DTdataSubset$geopotential)
# Mittelwert 54075.83



# plot(DTdataSubset$mslp, DTdataSubset$geopotential)

head(DTdataSubset)
