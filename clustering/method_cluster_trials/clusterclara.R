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

#### clustern
set.seed(1289)
# dist.data.scaled <- dist(scale(data.wide[, 2:321]), method = "euclidean")
clusterclara <- clara(scale(data.wide[, 2:321]), k = 5, metric = "euclidean", 
                      stand = TRUE, samples = 1000, sampsize = 300)
summary(clusterclara)
?clara
cluster.vector.clara <- clusterclara$clustering
?clara
## Measurements EUC ########### 
# 1.
sil(clusterclara, cluster.vector.clara, dist(scale(copy(data.wide)[, 2:321])), "kmeans")
    
?manova
dat.clara <- copy(data.wide)[, cluster := cluster.vector.clara]
# 2.
Cl.timeline(copy(dat.clara))
# 3.
model.clara <- manova(as.matrix(dat.clara[, 2:321]) ~ dat.clara$cluster)
summary(as.matrix(dat.clara[, 2:321]) ~ dat.clara$cluster, test = "Wilks")
summary.aov(model.clara)
# 4.
mosaic(copy(data.wide), cluster.vector.clara, title = "CLARA WITH EUC")


######### MANHATTAN ##################################
clusterclara.manhat <- clara(scale(data.wide[, 2:321]), k = 5, metric = "jaccard", 
                      stand = TRUE, samples = 1000, sampsize = 300)
summary(clusterclara.manhat)
cluster.vector.clara.manhat <- clusterclara.manhat$clustering
?clara
## Measurements MANHATTAN ########### 
# 1.
sil(clusterclara.manhat, cluster.vector.clara.manhat, dist(scale(copy(data.wide)[, 2:321])), "kmeans")

?manova
dat.clara.manhat <- copy(data.wide)[, cluster := cluster.vector.clara.manhat]
# 2.
Cl.timeline(copy(dat.clara.manhat))
# 3.
model.clara.manhat <- manova(as.matrix(dat.clara.manhat[, 2:321]) ~ dat.clara.manhat$cluster)
summary(as.matrix(dat.clara.manhat[, 2:321]) ~ dat.clara.manhat$cluster, test = "Wilks")
summary.aov(model.clara.manhat)
# 4.
mosaic(copy(data.wide), cluster.vector.clara.manhat, title = "CLARA WITH EUC")





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


