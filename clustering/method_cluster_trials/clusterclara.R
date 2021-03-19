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
dim(data.wide)

# Anzahl Cluster bestimmen
fviz_nbclust(as.data.frame(scale(data.wide[, 2:321])), FUNcluster = clara,
              method = "silhouette")

#### clustern
set.seed(1289)
# dist.data.scaled <- dist(scale(data.wide[, 2:321]), method = "euclidean")
clusterclara <- clara(scale(data.wide[, 2:321]), k = 6, metric = "euclidean", 
                      stand = TRUE, samples = 1000, sampsize = 300)
summary(clusterclara)
cluster.vector.clara <- clusterclara$clustering

## Measurements EUC ########### 
# 1. Silhouette
sil(clusterclara, cluster.vector.clara, dist(scale(copy(data.wide)[, 2:321])), "kmeans")
### s = 0.1239335

dat.clara <- copy(data.wide)[, cluster := cluster.vector.clara]
# 2. Timeline
Cl.timeline(copy(dat.clara))
### TLS = 0.1764022

# 3. Mosaikplot
mosaic(copy(data.wide), cluster.vector.clara, title = "CLARA WITH EUC")
### HB_diff = 0.4882091

######### MANHATTAN ##################################
clusterclara.manhat <- clara(scale(data.wide[, 2:321]), k = 5, metric = "manhattan", 
                      stand = TRUE, samples = 1000, sampsize = 300)
summary(clusterclara.manhat)
cluster.vector.clara.manhat <- clusterclara.manhat$clustering

## Measurements MANHATTAN ########### 
# 1. Silhouette
sil(clusterclara.manhat, cluster.vector.clara.manhat, dist(scale(copy(data.wide)[, 2:321])), "kmeans")
### s = 0.1301552

# 2. Timeline
dat.clara.manhat <- copy(data.wide)[, cluster := cluster.vector.clara.manhat]
Cl.timeline(copy(dat.clara.manhat))
### TLS = 0.1670198

# 3. Mosaik
mosaic(copy(data.wide), cluster.vector.clara.manhat, title = "CLARA WITH EUC")
### HB_diff = 0.4919


clara.plot <- autoplot(clusterclara, frame = TRUE, frame.type = "norm")                                                         
clara.plot
plot(clusterclara)

clara.plot.manhat <- autoplot(clusterclara.manhat, frame = TRUE, frame.type = "norm") 
clara.plot.manhat
plot(clusterclara.manhat)
