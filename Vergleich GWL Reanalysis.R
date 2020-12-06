library(data.table)
library(ggplot2)

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


groesser2gwl <- as.data.frame(matrix(ncol = 324))


#for ( i in seq_len(nrow(gwl_data)-1)) {
#  if (gwl_data[i,4] == gwl_data[(i + 1),4]) {
#  groesser2gwl[i,] <-  gwl_data[i,]
#  }
#}
#groesser2gwl <- na.omit(groesser2gwl)  
#colnames(groesser2gwl) <- colnames(gwl_data)
# Eine GWL im erzeugten Data frame: GWL kam an 2 hintereinandefolgenden Tagen vor
# 2 GWL im erzeugten Data frame: GWL kam an 3 hintereinanderfolgenden Tagen vor


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
# 2000 bis 2010 anschauen oder noch weiter zurückgehen

