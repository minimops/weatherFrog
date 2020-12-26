#### Clara clustern: 320 Dimensionen#################################################################################
## Clara steht für Clustering LARge Applications
## basiert auf k-medoids Approach, also PAM
## Methodik: 1. Ziehen einer Stichprobe aus Datensatz, die in k vorgegebene Cluster eingeteilt wird mit k-medoids bzw PAM.
##              In jedem Cluster gibt es ein repräsenatives Objekt, also k Stück
##           2. Jede Beobachtung, die nicht in der Stichprobe enthalten ist, wird dem am nächsten liegenden der k repräsentativen 
##              Objekte zugeteilt.
##      Das ganze wird 5 Mal wiederholt und der Algorithmus entscheidet sich für die Lösung, bei der die geringste mittlere Distanz
##      innerhalb der k cluster gemessen wurde.
## Clara eigent sich für 100-1000 Dimensionen mit bis zu 2000 - 3000 Beobachtungen

## laden der Pakete
library(cluster)
library(ggplot2)
library(data.table)
library(factoextra)
library(ggfortify)
library(ggmosaic)
## daten im wide format einlesen
data.wide <- readRDS("Data/cli_data_05_avgDay_wide.rds")
data.complete <- readRDS("Data/cli_data_05.rds")
data.completed <- dcast(data.complete, 
                       "date  ~ longitude + latitude + time", value.var = c("mslp", "geopotential"))
dim(data.completed)

# Anzahl Cluster bestimmen
?fviz_nbclust()
fviz_nbclust(data, clara, method = "silhouette",k.max = yourMaxValue)+theme_classic()
fviz_nbclust(as.data.frame(scale(data.wide[, 2:321])), FUNcluster = clara,
              method = "silhouette")
## geht bei mir leider nicht, da 0.5.0 geladen ist oder so, kann das mal bitte wer anders ausführen?

### normierung funktioniert noch nicht so
min <- data.wide[, apply(.SD, 2, min), .SDcols = j]
max <- data.wide[, apply(.SD, 2, max), .SDcols = j]
j <- 2:321
data.normiert <- data.wide[, lapply(.SD, function(x) ((.SD - min[x]) / (max[x] - min[x]))), .SDcols = j]
min[1]

data.wide[, lapply(.SD, function(x) (.SD - min[x]) / (max[x] - min[x])), .SDcols = j]



#### clustern
set.seed(1289)
# dist.data.scaled <- dist(scale(data.wide[, 2:321]), method = "euclidean")
clusterclara <- clara(scale(data.wide[, 2:321]), k = 5, metric = "euclidean", 
                      stand = FALSE, samples = 1000)
summary(clusterclara)
?clara
clusterclara$clustering

clusterclara$silinfo
data.wide.cluster <- data.wide[, cluster := clusterclara$clustering]
data.wide.cluster.gwl <- gwl[data.wide.cluster, on = .(date)]
clara.plot <- autoplot(clusterclara, frame = TRUE, frame.type = "norm")                                                         
clara.plot
plot(clusterclara)
gwl.plot <- autoplot(clusterclara, data = data.wide.cluster.gwl, colour = "gwl")


mosaic_cluster_clara <- ggplot(data = data.wide.cluster.gwl) + geom_mosaic(aes(x = product(gwl, cluster), fill = gwl), 
                                                                    na.rm = TRUE) +
  labs(x = " Cluster", y = "GWL")

mosaic_gwl_clara <- ggplot(data = data.wide.cluster.gwl) + geom_mosaic(aes(x = product(cluster, gwl), fill = as.factor(cluster)), 
                                                                na.rm = TRUE) 
mosaicplot(table(data.wide.cluster.gwl$gwl, data.wide.cluster.gwl$cluster), color = TRUE, main = "Clustering Large Applications", 
           ylab = "Cluster", xlab = "GWL", cex.axis = 0.6, las = 2)
mosaicplot(table(data.wide.cluster.gwl$cluster, data.wide.cluster.gwl$gwl), color = TRUE)

?mosaicplot



dist.data.scaled.complete <- dist(scale(data.completed[, 2:1281]), method = "euclidean")
clusterclara.complete <- clara(scale(data.completed[, 2:1281]), k = 9, metric = "euclidean", 
                      stand = FALSE, samples = 1000)

summary(clusterclara.complete)
clusterclara.complete$clustering

data.wide.cluster.complete <- data.completed[, cluster := clusterclara.complete$clustering]
data.wide.cluster.gwl.complete <- gwl[data.wide.cluster.complete, on = .(date)]
clara.plot <- autoplot(clusterclara.complete, frame = TRUE, frame.type = "norm")                                                         
clara.plot
plot(clusterclara)



mosaic_cluster_clara_complete <- ggplot(data = data.wide.cluster.gwl.complete) + geom_mosaic(aes(x = product(gwl, cluster), fill = gwl), 
                                                                           na.rm = TRUE) +
  labs(x = " Cluster", y = "GWL")

mosaic_gwl_clara_complete <- ggplot(data = data.wide.cluster.gwl.complete) + geom_mosaic(aes(x = product(cluster, gwl), fill = as.factor(cluster)), 
                                                                       na.rm = TRUE) 


