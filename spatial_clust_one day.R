#trying dbscan to cluster a single day

library(dbscan)
library(data.table)
library(ggplot2)

#sample one day
oneDay <- readRDS("Data/cli_data_05_avgDay.rds")[date %in% as.Date("2006-01-01"), ]

oneDay <- as.matrix(oneDay[, date := NULL])

kNNdistplot(oneDay, k = 5)
abline(h=600, col = "red", lty =  2)

#TODO gewichtung?
result <- dbscan(oneDay, eps = 600, minPts = 10)
result

plotRes <- data.frame(cluster = as.factor(result$cluster), oneDay)
ggplot(data = plotRes, aes(x = longitude, y = latitude)) +
  geom_rect(aes(xmin=longitude - 2.812519, xmax=longitude + 2.812519,
                        ymin=latitude - 2.812519, ymax=latitude + 2.812519, 
                        fill = cluster), alpha=0.5)

#reference to actual values:
ggplot() +
  geom_rect(data=as.data.frame(oneDay), 
            mapping=aes(xmin=longitude - 2.812519, xmax=longitude + 2.812519,
                        ymin=latitude - 2.812519, ymax=latitude + 2.812519, 
                        fill = avg_geopot), alpha=0.5) +
  scale_fill_gradient(name = "geopot", low = "blue", high = "red")

ggplot() +
  geom_rect(data=as.data.frame(oneDay), 
            mapping=aes(xmin=longitude - 2.812519, xmax=longitude + 2.812519,
                        ymin=latitude - 2.812519, ymax=latitude + 2.812519, 
                        fill = avg_mslp), alpha=0.5) +
  scale_fill_gradient(name = "mslp in Pa", low = "blue", high = "red")

