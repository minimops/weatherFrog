#trying dbscan to cluster a single day

library(dbscan)
library(data.table)
library(ggplot2)

#sample one day
oneDay <- readRDS("Data/cli_data_2k_avgDay.rds")[date %in% datestoCheck[[10]], ]
resultDT <- copy(oneDay)
oneDay[, ":=" (longitude = 1* scale(longitude), latitude = 1* scale(latitude),
               avg_mslp = scale(avg_mslp), avg_geopot = scale(avg_geopot))]

oneDay <- as.matrix(oneDay[, date := NULL])

kNNdistplot(oneDay, k = 5)
abline(h=0.9, col = "red", lty =  2)

#TODO gewichtung?
result <- dbscan(oneDay, eps = .9, minPts = 10)
result

#attach clusterinfo to result dt
plotRes <- data.frame(resultDT, cluster = as.factor(result$cluster))

world_map_local +
  geom_rect(data = plotRes, mapping=aes(xmin=longitude - diff_lon, xmax=longitude + diff_lon,
                                        ymin=latitude - diff_lat, ymax=latitude + diff_lat, 
                                        fill = cluster), alpha = 0.7) +
  labs(title = datestoCheck[[10]], x = "", y = "") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
