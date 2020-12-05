library(data.table)


gwl_5_years <- readRDS("Data\\gwl_5_years_split.rds")
cli_data_05_avg_wide <- readRDS("Data\\cli_data_05_avgDay_wide.rds")

# Jahre, Monat und Tag auf separate Spalten
#cli_data_05_avg_wide <- as.data.table(data_5_years)
cli_data_05_avg_wide[, ":=" (year = substr(date, 1, 4), 
                     month = substr(date, 6, 7),
                     day = substr(date,9,10))][, date := NULL]
dim(gwl_5_years)
dim(cli_data_05_avg_wide)

table(gwl_5_years$year)
table(gwl_5_years$month,gwl_5_years$year)
#warum hat jeder Monat 31 Tage?

# GWL: Remove empty rows

gwl_5_years1 <- gwl_5_years[!(apply(gwl_5_years, 1, function(y) any(y == ""))),]
saveRDS(gwl_5_years1, file = "Data\\gwl_5_years1.rds")

gwl_5_years <- gwl_5_years[-c()]

#Merge data frames

gwl_data <- cbind(cli_data_05_avg_wide,gwl_5_years1)
gwl_data <- gwl_data[,-c(327,326,324)]

subdata <- gwl_data[,c(323,322,321,324)]

gwl_data <- gwl_data[,-c(323,322,321,324)]
gwl_data <- cbind(subdata,gwl_data)

# Herausfiltern aller Wetterlagen, die 2 mal oder mehrmals an darauffolgenden Tagen vorkommen

#gwl_data$BM <- gwl_data$gwl == "BM"
#gwl_data$BM[gwl_data$BM == "FALSE"] <- " "
#gwl_data$BM
#which(gwl_data$BM == "TRUE")


groesser2gwl <- as.data.frame(matrix(ncol = 4))

for ( i in 1: 1825) {
  if (gwl_data[i,4] == gwl_data[(i + 1), 4]) {
  groesser2gwl[i,] <-  gwl_data[i,c(1:4)]
  }
}
groesser2gwl <- na.omit(groesser2gwl)  
# Eine GWL im erzeugten Data frame: GWL kam an 2 hintereinandefolgenden Tagen vor
# 2 GWL im erzeugten Data frame: GWL kam an 3 hintereinanderfolgenden Tagen vor


# Herausfiltern aller GWL, die 3 mal oderöfter an darauffolgenden Tagen vorkamen
groesser3gwl <- as.data.frame(matrix(ncol = 4))

for ( i in 1: 106) {
  if (groesser2gwl[i,4] == groesser2gwl[(i + 1), 4]) {
    groesser3gwl[i,] <-  gwl_data[i,c(1:4)]
  }
}
groesser3gwl <- na.omit(groesser3gwl)

#ToDo: 
#Laufindex noch schön schrieben und nicht als Zahl
#Variablennamen
