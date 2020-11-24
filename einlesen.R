################# Statistisches Praktikum ##################
#### WHEATHER FROG

## Daten einlesen 

data <- readRDS("C:\\Users\\HP\\Documents\\weatherFrog\\StatistischesPraktikum\\data_reanalysis_20201109.rds")

# Wenn man head(data) ausgibt, wird die Uhrzeit nicht mit angezeigt... find ich bisschen komisch..
head(data)
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

unique(unlist(apply(wetterSubset[2:32], 1, unique)))
head(data)

# Datensatz in Date und Time aufteilen
data$date <- as.Date(data$time, tz = "CET")

data$time <- format(data$time,"%H:%M:%S")
tail(data)
head(data)
table(data$time)
