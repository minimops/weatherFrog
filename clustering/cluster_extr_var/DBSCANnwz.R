
nwz <- cli_gwl_1971[cli_gwl_1971$gwl == "NWZ",]

kNNdistplot(oneDay, k = 5)
dbscan(oneDay, eps = .8, minPts = 10)

library(KneeArrower)



kNNdist()

oneDay <- readRDS("Data/cli_data_05_avgDay.rds")[date %in% as.Date("2010-01-20"), ]
resultDT <- copy(oneDay)
oneDay[, ":=" (longitude = 1.2* scale(longitude), latitude = 1.2* scale(latitude),
               avg_mslp = scale(avg_mslp), avg_geopot = scale(avg_geopot))]

oneDay <- as.matrix(oneDay[, date := NULL])
kNNdistplot(oneDay, k = 5)
abline(h=0.66, col = "red", lty =  2)

# eelenbogenpunkt berechenen
 y <- kNNdist(oneDay, k = 5)
 x <- seq(from = 1, to = 160, by = 1)
 
KneeArrower::findCutoff(x,y, method = "curvature")
KneeArrower::findCutoff(x,y, method = "first", frac.of.steepest.slope = 0.5)
 
result <- dbscan(oneDay, eps = .85, minPts = 10)
result

?findCutoffCurvature
?findCutoff
