#### Clara clustern: 320 Dimensionen

## laden der Pakete
library(cluster)
library(ggplot2)
library(data.table)
library(factoextra)
## daten im wide format einlesen
data.wide <- readRDS("Data/cli_data_05_avgDay_wide.rds")
data.complete <- readRDS("Data/cli_data_05.rds")
data.completed <- dcast(data.complete, 
                       "date  ~ longitude + latitude + time", value.var = c("mslp", "geopotential"))
dim(data.completed)

# Anzahl Cluster bestimmen
?fviz_nbclust()
fviz_nbclust(as.data.frame(scale(data.wide[, 2:321])), FUNcluster = kmeans,
              method = "silhouette", 
             diss = dist(scale(data.wide[, 2:321]), method = "euclidean"))
## geht bei mir leider nicht, da 0.5.0 geladen ist oder so, kann das mal bitte wer anders ausführen?


?clara
dist.data.scaled <- dist(scale(data.wide[, 2:321]), method = "euclidean")
clusterclara <- clara(scale(data.wide[, 2:321]), k = 29, metric = "euclidean", 
                      stand = FALSE, samples = 100)
summary(clusterclara)
clusterclara$clustering

data.wide.cluster <- data.wide[, cluster := clusterclara$clustering]
data.wide.cluster.gwl <- gwl[data.wide.cluster, on = .(date)]
clara.plot <- autoplot(clusterclara, frame = TRUE, frame.type = "norm")                                                         
clara.plot
plot(clusterclara)



library(ggmosaic)
mosaic_cluster_clara <- ggplot(data = data.wide.cluster.gwl) + geom_mosaic(aes(x = product(gwl, cluster), fill = gwl), 
                                                                    na.rm = TRUE) +
  labs(x = " Cluster", y = "GWL")

mosaic_gwl_clara <- ggplot(data = data.wide.cluster.gwl) + geom_mosaic(aes(x = product(cluster, gwl), fill = as.factor(cluster)), 
                                                                na.rm = TRUE) 

gwl


dist.data.scaled.complete <- dist(scale(data.completed[, 2:1281]), method = "euclidean")
clusterclara.complete <- clara(scale(data.completed[, 2:1281]), k = 9, metric = "euclidean", 
                      stand = FALSE, samples = 100)
summary(clusterclara.complete)
clusterclara.complete$clustering

data.wide.cluster.complete <- data.completed[, cluster := clusterclara.complete$clustering]
data.wide.cluster.gwl.complete <- gwl[data.wide.cluster.complete, on = .(date)]
clara.plot <- autoplot(clusterclara.complete, frame = TRUE, frame.type = "norm")                                                         
clara.plot
plot(clusterclara)



library(ggmosaic)
mosaic_cluster_clara_complete <- ggplot(data = data.wide.cluster.gwl.complete) + geom_mosaic(aes(x = product(gwl, cluster), fill = gwl), 
                                                                           na.rm = TRUE) +
  labs(x = " Cluster", y = "GWL")

mosaic_gwl_clara_complete <- ggplot(data = data.wide.cluster.gwl.complete) + geom_mosaic(aes(x = product(cluster, gwl), fill = as.factor(cluster)), 
                                                                       na.rm = TRUE) 


