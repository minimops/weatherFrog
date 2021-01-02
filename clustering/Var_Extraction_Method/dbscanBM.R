gwl2 <- gwl[date > "2000-01-01"]
gwl2 <- gwl2[gwl == "BM"]

set.seed(123)

samples <- sample(gwl2[, date], 8)
samples
samples <- append(samples, "2001-02-14")
samples <- append(samples, "2001-02-15")

samples


#sample one day
oneDay <- readRDS("Data/cli_data_2k_avgDay.rds")[date %in% as.Date("2001-02-15"), ]
resultDT <- copy(oneDay)
oneDay[, ":=" (longitude = 1.2* scale(longitude), latitude = 1.2* scale(latitude),
               avg_mslp = scale(avg_mslp), avg_geopot = scale(avg_geopot))]

oneDay <- as.matrix(oneDay[, date := NULL])


kNNdistplot(oneDay, k = 5)
abline(h=0.71, col = "red", lty =  2)

#TODO gewichtung?
result <- dbscan(oneDay, eps = 0.75, minPts = 10)
result

#attach clusterinfo to result dt
plotRes <- data.frame(resultDT, cluster = as.factor(result$cluster))

world_map_local +
  geom_rect(data = plotRes, mapping=aes(xmin=longitude - diff_lon, xmax=longitude + diff_lon,
                                        ymin=latitude - diff_lat, ymax=latitude + diff_lat, 
                                        fill = cluster), alpha = 0.7) +
  labs(title = "dbscan am 2001-02-15", x = "", y = "") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
#reference to actual values:
world_map_local +
  geom_rect(data = plotRes, mapping=aes(xmin=longitude - diff_lon, xmax=longitude + diff_lon,
                                        ymin=latitude - diff_lat, ymax=latitude + diff_lat, 
                                        fill = avg_mslp), alpha = 0.7) +
  scale_fill_gradient(low = "blue", high = "red", guide = FALSE) + 
  labs(title = "mslp am 2006-12-12", x = "", y = "") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

world_map_local +
  geom_rect(data = plotRes, mapping=aes(xmin=longitude - diff_lon, xmax=longitude + diff_lon,
                                        ymin=latitude - diff_lat, ymax=latitude + diff_lat, 
                                        fill = avg_geopot), alpha = 0.7) +
  scale_fill_gradient(low = "blue", high = "red", guide = FALSE) + 
  labs(title = "geopot am 2006-12-12", x = "", y = "") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

#gwl on that day
gwlOnDay <- readRDS("Data/gwl.rds")[date %in% as.Date("2006-12-12"), ]
gwlOnDay
