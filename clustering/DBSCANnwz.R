
nwz <- cli_gwl_1971[cli_gwl_1971$gwl == "NWZ",]

kNNdistplot(oneDay, k = 5)
dbscan(oneDay, eps = .8, minPts = 10)

library(KneeArrower)



kNNdist()

oneDay <- readRDS("Data/cli_data_05_avgDay.rds")[date %in% as.Date("2010-05-24"), ]
resultDT <- copy(oneDay)
oneDay[, ":=" (longitude = 1.2* scale(longitude), latitude = 1.2* scale(latitude),
               avg_mslp = scale(avg_mslp), avg_geopot = scale(avg_geopot))]

oneDay <- as.matrix(oneDay[, date := NULL])
kNNdistplot(oneDay, k = 5)

# eelenbogenpunkt berechenen
 y <- kNNdist(oneDay, k = 5)
 x <- seq(from = 1, to = length(a), by = 1)
 
KneeArrower::findCutoff(x,y)
 
result <- dbscan(oneDay, eps = .85, minPts = 10)
result




