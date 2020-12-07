library(data.table)
library(ggplot2)


#####################################
# nur die letzten 5 Jahre betrachten
gwl_5_years <- readRDS("Data\\gwl_5_years_split.rds")
cli_data_05_avg_wide <- readRDS("Data\\cli_data_05_avgDay_wide.rds")

# Jahre, Monat und Tag auf separate Spalten

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



#Merge data frames

gwl_data <- cbind(cli_data_05_avg_wide,gwl_5_years1)
gwl_data <- gwl_data[,-c(327,326,324)]

subdata <- gwl_data[,c(323,322,321,324)]

gwl_data <- gwl_data[,-c(323,322,321,324)]
gwl_data <- cbind(subdata,gwl_data)





# Eine GWL im erzeugten Data frame: GWL kam an 2 hintereinandefolgenden Tagen vor
# 2 GWL im erzeugten Data frame: GWL kam an 3 hintereinanderfolgenden Tagen vor

groesser2gwl <- as.data.frame(matrix(ncol = 324))

for ( i in seq_len(nrow(gwl_data)-2)) {
  if (gwl_data[i,4] == gwl_data[(i + 1),4]) {
    if (gwl_data[(i + 1) ,4] == gwl_data[(i + 2),4]){
        groesser2gwl[(i + 1),] <-  gwl_data[i,] 
        groesser2gwl[(i + 2),] <-  gwl_data[(i + 1),] 
        groesser2gwl[(i + 3),] <-  gwl_data[(i + 2),] 
    }
  }
}
groesser2gwl <- na.omit(groesser2gwl)  
colnames(groesser2gwl) <- colnames(gwl_data)



# je einzelnen Dataframe für Mslp und Geopotential und in passende Form 
# fürs Visualisieren bringen

groesser2gwlMslp <- groesser2gwl[,1:164]
groesser2gwlMslp <-as.data.frame(t(groesser2gwlMslp))
standort <- seq(from = 1, to = nrow(groesser2gwlMslp))
groesser2gwlMslp <- cbind(standort,groesser2gwlMslp)
colnames(groesser2gwlMslp) <- c("standort", "WZ1Tag1","WZ1Tag2","WZ1Tag3", "WZ2Tag1","WZ2Tag2","WZ2Tag3",
                                "WZ3Tag1","WZ3Tag2","WZ3Tag3","WZ4Tag1","WZ4Tag2","WZ4Tag3","WZ5Tag1","WZ5Tag2","WZ5Tag3",
                                "SWZ6Tag1","SWZ6Tag2","SWZ6Tag3","SWZ7Tag1","SWZ7Tag2","SWZ7Tag3",
                                "SWZ8Tag1","SWZ8Tag2","SWZ8Tag3","WZ9Tag1","WZ9Tag2","WZ9Tag3")
groesser2gwlMslp <- groesser2gwlMslp[5:164,]


groesser2gwlGeo <- groesser2gwl[,-(5:164)]
groesser2gwlGeo <-as.data.frame(t(groesser2gwlGeo))
standort <- seq(from = 1, to = nrow(groesser2gwlGeo))
groesser2gwlGeo <- cbind(standort,groesser2gwlGeo)
colnames(groesser2gwlGeo) <- c("standort", "WZ1Tag1","WZ1Tag2","WZ1Tag3", "WZ2Tag1","WZ2Tag2","WZ2Tag3",
                               "WZ3Tag1","WZ3Tag2","WZ3Tag3","WZ4Tag1","WZ4Tag2","WZ4Tag3","WZ5Tag1","WZ5Tag2","WZ5Tag3",
                               "SWZ6Tag1","SWZ6Tag2","SWZ6Tag3","SWZ7Tag1","SWZ7Tag2","SWZ7Tag3",
                               "SWZ8Tag1","SWZ8Tag2","SWZ8Tag3","WZ9Tag1","WZ9Tag2","WZ9Tag3")
groesser2gwlGeo <- groesser2gwlGeo[5:164,]



#Visualisierung einer GWL, die an darauffolgenden Tagen auftritt
timeplot <- function (Standort,Tag1, Tag2, Tag3, yVariable){
plot(x= Standort,y= Tag1 ,type ="l",col="red",lwd=1,xlim = c(0,160),
     xaxp = c(0,160,32),
     xlab="Messstandorte",ylab= yVariable)
lines(x= Standort, y= Tag2,type="l",lwd = 1,col="blue")
lines(x= Standort, y= Tag3,type="l",lwd = 1,col="red")

legend("bottomright",title=NA,
       legend=c("Tag 1","Tag 2","Tag 3"),lty=c(1,1,1),lwd=2,col = c("red","blue", "red"),
       bty="n",ncol=3)
}

timeplot(groesser2gwlMslp$standort,groesser2gwlMslp$WZ1Tag1,groesser2gwlMslp$WZ1Tag2,
         groesser2gwlMslp$WZ1Tag3,"Luftdruck")
timeplot(groesser2gwlMslp$standort,groesser2gwlMslp$WZ2Tag1,groesser2gwlMslp$WZ2Tag2,
         groesser2gwlMslp$WZ2Tag3,"Luftdruck")
timeplot(groesser2gwlMslp$standort,groesser2gwlMslp$WZ3Tag1,groesser2gwlMslp$WZ3Tag2,
         groesser2gwlMslp$WZ3Tag3,"Luftdruck")
timeplot(groesser2gwlMslp$standort,groesser2gwlMslp$WZ4Tag1,groesser2gwlMslp$WZ4Tag2,
         groesser2gwlMslp$WZ4Tag3,"Luftdruck")
timeplot(groesser2gwlMslp$standort,groesser2gwlMslp$WZ5Tag1,groesser2gwlMslp$WZ5Tag2,
         groesser2gwlMslp$WZ5Tag3,"Luftdruck")
timeplot(groesser2gwlMslp$standort,groesser2gwlMslp$SWZ6Tag1,groesser2gwlMslp$SWZ6Tag2,
         groesser2gwlMslp$SWZ6Tag3,"Luftdruck")
timeplot(groesser2gwlMslp$standort,groesser2gwlMslp$SWZ7Tag1,groesser2gwlMslp$SWZ7Tag2,
         groesser2gwlMslp$SWZ7Tag3,"Luftdruck")
timeplot(groesser2gwlMslp$standort,groesser2gwlMslp$SWZ8Tag1,groesser2gwlMslp$SWZ8Tag2,
         groesser2gwlMslp$SWZ8Tag3,"Luftdruck")
timeplot(groesser2gwlMslp$standort,groesser2gwlMslp$WZ9Tag1,groesser2gwlMslp$WZ9Tag2,
         groesser2gwlMslp$WZ9Tag3,"Luftdruck")

timeplot(groesser2gwlGeo$standort,groesser2gwlGeo$WZ1Tag1,groesser2gwlGeo$WZ1Tag2,
         groesser2gwlGeo$WZ1Tag3,"Geopotential")
timeplot(groesser2gwlGeo$standort,groesser2gwlGeo$WZ2Tag1,groesser2gwlGeo$WZ2Tag2,
         groesser2gwlGeo$WZ2Tag3,"Geopotential")
timeplot(groesser2gwlGeo$standort,groesser2gwlGeo$WZ3Tag1,groesser2gwlGeo$WZ3Tag2,
         groesser2gwlGeo$WZ3Tag3,"Geopotential")
timeplot(groesser2gwlGeo$standort,groesser2gwlGeo$WZ4Tag1,groesser2gwlGeo$WZ4Tag2,
         groesser2gwlGeo$WZ4Tag3,"Geopotential")
timeplot(groesser2gwlGeo$standort,groesser2gwlGeo$WZ5Tag1,groesser2gwlGeo$WZ5Tag2,
         groesser2gwlGeo$WZ5Tag3,"Geopotential")
timeplot(groesser2gwlGeo$standort,groesser2gwlGeo$SWZ6Tag1,groesser2gwlGeo$SWZ6Tag2,
         groesser2gwlGeo$SWZ6Tag3,"Geopotential")
timeplot(groesser2gwlGeo$standort,groesser2gwlGeo$SWZ7Tag1,groesser2gwlGeo$SWZ7Tag2,
         groesser2gwlGeo$SWZ7Tag3,"Geopotential")
timeplot(groesser2gwlGeo$standort,groesser2gwlGeo$SWZ8Tag1,groesser2gwlGeo$SWZ8Tag2,
         groesser2gwlGeo$SWZ8Tag3,"Geopotential")
timeplot(groesser2gwlGeo$standort,groesser2gwlGeo$WZ9Tag1,groesser2gwlGeo$WZ9Tag2,
         groesser2gwlGeo$WZ9Tag3,"Geopotential")


# In den Jahren 2006 bis 2010 dauern die meisten GWLs nur einen Tag an.
# 9 mal in den 5 Jahren gibt es GWL, die 3 Tage hintereinander andauern. 
# GWL, die mehr als 3 Tage hintereinander vorkommen, gibt es nicht.

# TODO:
# 1971 bis 2010 anschauen oder noch weiter zurückgehen

###################################################
# ab Jahre 1971 anschauen

#GWL Datensatz ab 1971 herausfiltern

library(data.table)
library(stringr)

#gwl data
gwl_data <- as.data.table(read.table("Data/GWL_1900-2010.csv", header = TRUE, sep = ";",
                                     na.strings = " "))
#wide to long:
gwl_data <- melt(gwl_data, id.vars = c("JAHRMONAT"), measure.vars = patterns("^X"),
                 variable.name = "day", value.name = "gwl")
#split year and month
gwl_data[, ":=" (year = substr(JAHRMONAT, 1, 4), 
                 month = substr(JAHRMONAT, 5, 6))][, JAHRMONAT := NULL]
#rename and pad day
gwl_data_split <- copy(gwl_data[, day := str_pad(sub("X", "", day), 2, "left", "0")])
#fuse to date
gwl_data[, date := as.Date(paste(year, month, day, sep = "-"))]
#delete columns
gwl_data[, ":=" (year = NULL, month = NULL, day = NULL)]
#reorder cols
setcolorder(gwl_data, c("date", "gwl"))

#subset timeframe
gwl_1971 <- gwl_data_split[year %in% seq(1971, 2010), ]

#leere Zeilen entfernen

gwl_1971 <- gwl_1971[!(apply(gwl_1971, 1, function(y) any(y == ""))),]


###### reanalysis data ab 1971 herausfiltern
## this file prepares the weather dataset and saves the results

library(data.table)

#import and convert to data.table
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

# Jahre, Monat und Tag auf separate Spalten

cli_data_1971_avg_wide[, ":=" (year = substr(date, 1, 4), 
                             month = substr(date, 6, 7),
                             day = substr(date,9,10))][, date := NULL]

#####Merge data frames

gwl_data <- cbind(cli_data_1971_avg_wide,gwl_1971)
gwl_data <- gwl_data[,-c(327,326,324)]

subdata <- gwl_data[,c(323,322,321,324)]

gwl_data <- gwl_data[,-c(323,322,321,324)]
gwl_data <- cbind(subdata,gwl_data)



# Eine GWL im erzeugten Data frame: GWL kam an 2 hintereinandefolgenden Tagen vor
# 2 GWL im erzeugten Data frame: GWL kam an 3 hintereinanderfolgenden Tagen vor

groesser2gwl <- as.data.frame(matrix(ncol = 324))

for ( i in seq_len(nrow(gwl_data)-2)) {
  if (gwl_data[i,4] == gwl_data[(i + 1),4]) {
    if (gwl_data[(i + 1) ,4] == gwl_data[(i + 2),4]){
      groesser2gwl[(i + 1),] <-  gwl_data[i,] 
      groesser2gwl[(i + 2),] <-  gwl_data[(i + 1),] 
      groesser2gwl[(i + 3),] <-  gwl_data[(i + 2),] 
    }
  }
}
groesser2gwl <- na.omit(groesser2gwl)  
colnames(groesser2gwl) <- colnames(gwl_data)


groesser2gwlMslp <- groesser2gwl[,1:164]
groesser2gwlMslp <-as.data.frame(t(groesser2gwlMslp))
standort <- seq(from = 1, to = nrow(groesser2gwlMslp)-4)
standort <- c(0,0,0,0,standort)
groesser2gwlMslp <- cbind(standort,groesser2gwlMslp)

groesser2gwlGeo <- groesser2gwl[,1:164]
groesser2gwlGeo <-as.data.frame(t(groesser2gwlGeo))
standort <- seq(from = 1, to = nrow(groesser2gwlGeo)-4)
standort <- c(0,0,0,0,standort)
groesser2gwlGeo <- cbind(standort,groesser2gwlGeo)



#Visualisierung einer GWL, die an darauffolgenden Tagen auftritt
timeplot <- function (Standort,Tag1, Tag2, Tag3, yVariable){
  plot(x= Standort,y= Tag1 ,type ="l",col="red",lwd=1,xlim = c(0,160),
       xaxp = c(0,160,32),
       xlab="Messstandorte",ylab= yVariable)
  lines(x= Standort, y= Tag2,type="l",lwd = 1,col="blue")
  lines(x= Standort, y= Tag3,type="l",lwd = 1,col="red")
  
  legend("bottomright",title=NA,
         legend=c("Tag 1","Tag 2","Tag 3"),lty=c(1,1,1),lwd=2,col = c("red","blue", "red"),
         bty="n",ncol=3)
}

timeplot(groesser2gwlMslp[5:246,1],groesser2gwlMslp[5:246,2],groesser2gwlMslp[5:246,3],groesser2gwlMslp[5:246,4],"Luftdruck")



######## Herausfiltern aller GWL, die mindestens 4 mal hintereinander vorkommen

groesser3gwl <- as.data.frame(matrix(ncol = 324))

for ( i in seq_len(nrow(gwl_data)-3)) {
  if (gwl_data[i,4] == gwl_data[(i + 1),4]) {
    if (gwl_data[(i + 1) ,4] == gwl_data[(i + 2),4]){
      if(gwl_data[(i + 2),4] == gwl_data[(i + 3),4]){
        
      groesser3gwl[(i + 1),] <-  gwl_data[i,] 
      groesser3gwl[(i + 2),] <-  gwl_data[(i + 1),] 
      groesser3gwl[(i + 3),] <-  gwl_data[(i + 2),] 
      groesser3gwl[(i + 4),] <-  gwl_data[(i + 3),]
      }
    }
  }
}
groesser3gwl <- na.omit(groesser3gwl)  
colnames(groesser3gwl) <- colnames(gwl_data)

# je einzelnen Dataframe für Mslp und Geopotential und in passende Form 
# fürs Visualisieren bringen

groesser3gwlMslp <- groesser3gwl[,1:164]
groesser3gwlMslp <-as.data.frame(t(groesser3gwlMslp))
standort <- seq(from = 1, to = nrow(groesser3gwlMslp))
groesser3gwlMslp <- cbind(standort,groesser3gwlMslp)
colnames(groesser3gwlMslp) <- c("standort","SWZ1Tag1","SWZ1Tag2","SWZ1Tag3","SWZ1Tag4",
                                "SWZ2Tag1","SWZ2Tag2","SWZ2Tag3","SWZ3Tag4")
groesser3gwlMslp <- groesser3gwlMslp[5:164,]


groesser3gwlGeo <- groesser3gwl[,-(5:164)]
groesser3gwlGeo <-as.data.frame(t(groesser3gwlGeo))
standort <- seq(from = 1, to = nrow(groesser3gwlGeo))
groesser3gwlGeo <- cbind(standort,groesser3gwlGeo)
colnames(groesser3gwlGeo) <- c("standort","SWZ1Tag1","SWZ1Tag2","SWZ1Tag3","SWZ1Tag4",
                                "SWZ2Tag1","SWZ2Tag2","SWZ2Tag3","SWZ3Tag4")

groesser3gwlGeo <- groesser3gwlGeo[5:164,]


#Visualisierung einer GWL, die an 4 darauffolgenden Tagen auftritt
timeplot1 <- function (Standort,Tag1, Tag2, Tag3, Tag4, yVariable){
  plot(x= Standort,y= Tag1 ,type ="l",col="red",lwd=1,xlim = c(0,160),
       xaxp = c(0,160,32),
       xlab="Messstandorte",ylab= yVariable)
  lines(x= Standort, y= Tag2,type="l",lwd = 1,col="blue")
  lines(x= Standort, y= Tag3,type="l",lwd = 1,col="blue")
  lines(x = Standort,y = Tag4,type = "l", lwd = 1, col = "red")
  
  
  legend("bottomright",title=NA,
         legend=c("Tag 1","Tag 2","Tag 3","Tag 4"),lty=c(1,1,1,1),lwd=2,col = c("red","blue", "blue", "red"),
         bty="n",ncol=4)
}

timeplot1(groesser3gwlMslp$standort, groesser3gwlMslp$SWZ1Tag1,groesser3gwlMslp$SWZ1Tag2,
         groesser3gwlMslp$SWZ1Tag3, groesser3gwlMslp$SWZ1Tag4, "Luftdruck")
timeplot1(groesser3gwlMslp$standort, groesser3gwlMslp$SWZ2Tag1,groesser3gwlMslp$SWZ2Tag2,
          groesser3gwlMslp$SWZ2Tag3, groesser3gwlMslp$SWZ2Tag4, "Luftdruck")

timeplot1(groesser3gwlGeo$standort, groesser3gwlGeo$SWZ1Tag1,groesser3gwlGeo$SWZ1Tag2,
          groesser3gwlGeo$SWZ1Tag3, groesser3gwlGeo$SWZ1Tag4, "Geopotential")
timeplot1(groesser3gwlGeo$standort, groesser3gwlGeo$SWZ2Tag1,groesser3gwlGeo$SWZ2Tag2,
          groesser3gwlGeo$SWZ2Tag3, groesser3gwlGeo$SWZ2Tag4, "Geopotential")


