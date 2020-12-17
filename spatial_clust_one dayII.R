#trying dbscan to cluster a single day

library(dbscan)
library(data.table)
library(ggplot2)

#sample one day
oneDay <- readRDS("Data/cli_data_05_avgDay.rds")[date %in% as.Date("2006-03-12"), ]
oneDay[, ":=" (longitude = 1.2* scale(longitude), latitude = 1.2* scale(latitude),
              avg_mslp = scale(avg_mslp), avg_geopot = scale(avg_geopot))]

oneDay <- as.matrix(oneDay[, date := NULL])


kNNdistplot(oneDay, k = 5)
abline(h=1.1, col = "red", lty =  2)

#TODO gewichtung?
result <- dbscan(oneDay, eps = 1, minPts = 10)
result

plotRes <- data.frame(cluster = as.factor(result$cluster), oneDay)
ggplot(data = plotRes, aes(x = longitude, y = latitude)) +
  geom_point(aes(color = as.factor(result$cluster)))
#reference to actual values:
ggplot(data = as.data.frame(oneDay), aes(x = longitude, y = latitude, color = avg_geopot)) +
  geom_point()

ggplot(data = as.data.frame(oneDay), aes(x = longitude, y = latitude, color = avg_mslp)) +
  geom_point()

#gwl on that day
gwlOnDay <- readRDS("Data/gwl.rds")[date %in% as.Date("2006-03-12"), ]
gwlOnDay


###more sophisticated approach

# 1. choose variables wisely
#eps: how close should the points be in a cluster
  # - use a coordinate based design for now (maybe weigh the location higher?)

#minPoints: min amount of points to build a cluster
 # - lets start with 4

# 2. create list of matrizes of each day
