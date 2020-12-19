## Cluster mit Mahalanobisdistanz mit 320 Dimensionen

## ertsmal einlesen
data.wide <- readRDS("Data/cli_data_05_avgDay_wide.rds")

### Pakete laden
library(data.table)
library(ggplot2)
library(ggmosaic)
library(factoextra)
library(ggfortify)
library(cluster)
#### einmal alles mit gescalten Variablen
data.scaled <- scale(data.wide[, 2:321], scale=TRUE, center=TRUE)
gwl <- readRDS("Data/gwl.rds")
## abchecken obs stimmt
mean <- colMeans(data.scaled)
# Lovarianzmatrix erstellen
variance <- var(data.scaled)
# Anzahl der Iterationen
n <- nrow(data.scaled)

?mahalanobis

dist_mahal_scaled <- matrix(NA, nrow=n, ncol=n)

# Mahalanobisdistanz berechnen und in Distanzmatrix abspeichern
for(i in seq_len(n)){
  dist_mahal_scaled[i,] <- mahalanobis(data.scaled, data.scaled[i,], variance)
}


rownames(dist_mahal_scaled) <- colnames(dist_mahal_scaled) <- rn <- 1:1826
# dist_mahal_scaled <- as.dist(dist_mahal)

# sollte das eigentlich von alleine machen, aber geht nicht
??mahalanobis.dist
saveRDS(dist_mahal_scaled, "Data/dist_mahal_scaled.rds")

# erster Clusterversuch, hierarchisch
complete_linkage <- hclust(as.dist(dist_mahal_scaled), method="complete")
summary(complete_linkage)
plot(complete_linkage)
fviz_dend(complete_linkage) + ggtitle("Complete Linkage - Dendrogramm")
complete_linkage$order

## zweiter Clusterversuch
clust <- hcut(as.dist(dist_mahal_scaled), k = 9, hc_func = "hclust", hc_method = "complete")
clust$height
plot(clust)


### Fuzzy analysis
# teilt nicht in ein festes cluster ein, sondern erlaubt auch mehrdeutigkeit
# hält mehr detaillierte informationen von den daten
set.seed(1289)
?fanny
clusterfanny <- fanny(as.dist(dist_mahal_scaled), k = 9, diss = TRUE, memb.exp = 1)

data.wide.cluster.fanny <- data.wide[, cluster := clusterfanny$clustering]
data.wide.cluster.gwl.fanny <- gwl[data.wide.cluster.fanny, on = .(date)]
fanny.plot <- autoplot(clusterfanny, frame = TRUE, frame.type = "norm")                                                         
clara.plot
plot(clusterfanny)
mosaicplot(table(data.wide.cluster.gwl.fanny$gwl, data.wide.cluster.gwl.fanny$cluster), color = TRUE, main = "Cluster der GWL", 
           ylab = "Cluster", xlab = "GWL", cex.axis = 0.35)
mosaicplot(table(data.wide.cluster.gwl.fanny$cluster, data.wide.cluster.gwl.fanny$gwl), color = TRUE)
# -> funktioniert nicht

### k -means clustering 
# kmeans-Clustering mit allen Kovariablen, 100 zufaellige Startpartitionen


clusterkmeans <- kmeans(as.dist(dist_mahal_scaled), centers=9, iter.max=1000)

data.wide.cluster.kmeans <- data.wide[, cluster := clusterkmeans$cluster]
data.wide.cluster.gwl.kmeans <- gwl[data.wide.cluster.kmeans, on = .(date)]
kmeans.plot <- autoplot(clusterkmeans, frame = TRUE, frame.type = "norm")                                                         
kmeans.plot


mosaic_cluster_kmeans <- ggplot(data = data.wide.cluster.gwl.kmeans) + geom_mosaic(aes(x = product(gwl, cluster), fill = gwl), 
                                                                           na.rm = TRUE) + labs(x = " Cluster", y = "GWL")
mosaic_gwl_kmeans <- ggplot(data = data.wide.cluster.gwl.kmeans) + geom_mosaic(aes(x = product(cluster, gwl), 
                                                                                   fill = as.factor(cluster)), 
                                                                       na.rm = TRUE) 
mosaicplot(table(data.wide.cluster.gwl.kmeans$gwl, data.wide.cluster.gwl.kmeans$cluster), color = TRUE, 
           main = "Cluster mit k-Means und Mahalanobis", ylab = "Cluster", xlab = "GWL", cex.axis = 0.6, las = 2)
mosaicplot(table(data.wide.cluster.gwl.kmeans$cluster, data.wide.cluster.gwl.kmeans$gwl), color = TRUE)





#### einmal alles ohne Skalierung #################################################################################


mean2 <- colMeans(data.wide[, 1:320])
variance2 <- cov(data.wide[, 2:321])

n <- nrow(data.wide[2:321])
?mahalanobis
dist_mahal2 <- matrix(NA, nrow=n, ncol=n)
data.col <- data.wide[, date := NULL]
for(i in 1:n){
  dist_mahal2[i,] <- mahalanobis(data.wide, data.wide[i, ], variance2)
}

dim(data.wide)
rownames(dist_mahal2) <- colnames(dist_mahal2) <- rownames(dist_mahal2_cov) <- colnames(dist_mahal2_cov) <- rn <- 1:1826
# dist_mahal <- as.dist(dist_mahal)

??mahalanobis.dist

complete_linkage <- hclust(dist_mahal, method="complete")
summary(complete_linkage)
plot(complete_linkage)
fviz_dend(complete_linkage) + ggtitle("Complete Linkage - Dendrogramm")
complete_linkage$order


clust <- hcut(dist_mahal, k = 9, hc_func = "hclust", hc_method = "complete")
clust$height
## mean für center Argument bei mahalanobis
mean <- sapply(data.wide[, 2:321], mean)
## kovarianzmatrix für Argument cov in mahalanobis
covariance <- cov(as.data.frame(data.wide[, 2:321]))
dim(covariance)
# cov2cor(covariance)


### Fuzzy analysis
# teilt nicht in ein festes cluster ein, sondern erlaubt acu mehrdeutigkeit
# hält mehr detaillierte informationen von den daten

?fanny
clusterfanny <- fanny(as.dist(dist_mahal_cov), k = 9, diss = TRUE, memb.exp = 1)

data.wide.cluster.fanny <- data.wide[, cluster := clusterfanny$clustering]
data.wide.cluster.gwl.fanny <- gwl[data.wide.cluster.fanny, on = .(date)]
fanny.plot <- autoplot(clusterfanny, frame = TRUE, frame.type = "norm")                                                         
clara.plot
plot(clusterfanny)


### k -means
# kmeans-Clustering mit allen Kovariablen, 100 zufaellige Startpartitionen
set.seed(356)

clusterkmeans <- kmeans(x=dist_mahal, centers=9, iter.max=100, nstart=100)

data.wide.cluster.kmeans <- data.wide[, cluster := clusterkmeans$cluster]
data.wide.cluster.gwl.kmeans <- gwl[data.wide.cluster.kmeans, on = .(date)]
kmeans.plot <- autoplot(clusterkmeans, frame = TRUE, frame.type = "norm")                                                         
kmeans.plot
plot(clusterkmeans)




mosaic_cluster_kmeans <- ggplot(data = data.wide.cluster.gwl.kmeans) + geom_mosaic(aes(x = product(gwl, cluster), fill = gwl), 
                                                                                   na.rm = TRUE) +
  labs(x = " Cluster", y = "GWL")

mosaic_gwl_kmeans <- ggplot(data = data.wide.cluster.gwl.kmeans) + geom_mosaic(aes(x = product(cluster, gwl), fill = as.factor(cluster)), 
                                                                               na.rm = TRUE) 

clusterkmeans_cov <- kmeans(x= as.dist(dist_mahal_cov), centers=9, iter.max=100, nstart=100)

data.wide.cluster.kmeans <- data.wide[, cluster := clusterkmeans$cluster]
data.wide.cluster.gwl.kmeans <- gwl[data.wide.cluster.kmeans, on = .(date)]
kmeans.plot <- autoplot(clusterkmeans, frame = TRUE, frame.type = "norm")                                                         
kmeans.plot
plot(clusterkmeans)




