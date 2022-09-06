# This file clusters the original dataset with average over day for the years 2006-2010.
# Cluster algorithms are CLARA, k-Means and PAM.

## load the packages
library(cluster)
library(ggplot2)
library(data.table)
library(factoextra)
library(ggfortify)

source("clustering/ClusterAssesmentHelper.R")

# 1. read the data which is the original dataset for and in this case it only considers 5 years
data.wide <- readRDS("Data/cli_data_05_avgDay_wide.rds")
dim(data.wide)

# 2. Get Number of clusters for clustering with CLARA
fviz_nbclust(as.data.frame(scale(data.wide[, 2:321])), FUNcluster = clara,
              method = "silhouette")

# 3. Clustering with CLARA ###########################
set.seed(1234)

# 3.1 Cluster with CLARA and euclidean distance
clusterclara <- clara(scale(data.wide[, 2:321]), k = 6, metric = "euclidean", 
                      stand = TRUE, samples = 1000, sampsize = 300)
summary(clusterclara)
cluster.vector.clara <- clusterclara$clustering

## Measurements 
## 1. Silhouette
sil(clusterclara, cluster.vector.clara, dist(scale(copy(data.wide)[, 2:321])), "kmeans")
### s = 0.1239335
dat.clara <- copy(data.wide)[, cluster := cluster.vector.clara]
## 2. Timeline
Cl.timeline(copy(dat.clara))
### TLS = 0.1764022

## 3. Mosaic
mosaic(copy(data.wide), cluster.vector.clara, title = "CLARA WITH EUC")
### HB_diff = 0.4882091




# 3.2 Clustering with CLARA and Manhattan distance
clusterclara.manhat <- clara(scale(data.wide[, 2:321]), k = 5, metric = "manhattan", 
                      stand = TRUE, samples = 1000, sampsize = 300)
summary(clusterclara.manhat)
cluster.vector.clara.manhat <- clusterclara.manhat$clustering

## Measurements 
## 1. Silhouette
sil(clusterclara.manhat, cluster.vector.clara.manhat, dist(scale(copy(data.wide)[, 2:321])), "kmeans")
### s = 0.1301552

## 2. Timeline
dat.clara.manhat <- copy(data.wide)[, cluster := cluster.vector.clara.manhat]
Cl.timeline(copy(dat.clara.manhat))
### TLS = 0.1670198

## 3. Mosaic
mosaic(copy(data.wide), cluster.vector.clara.manhat, title = "CLARA WITH EUC")
### HB_diff = 0.4919

# 4. Visualization
clara.plot <- autoplot(clusterclara, frame = TRUE, frame.type = "norm")                                                         
clara.plot
plot(clusterclara)

clara.plot.manhat <- autoplot(clusterclara.manhat, frame = TRUE, frame.type = "norm") 
clara.plot.manhat
plot(clusterclara.manhat)


# 5. Clustering with k-Means #################################
# -> get numer of clusters
km_5 <- list()
wss5 <- numeric()

for (k in 1:10){
  km_5[[k]] <- kmeans(scale(data.wide[, 2:321]), centers = k, iter.max = 1000)
  wss5[k] <- sum(km_5[[k]]$withinss)
}

par(mfrow = c(1,1))
plot(1:10, wss5, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares")

# -> Cluster
clusterkmeans <- kmeans(scale(data.wide[, 2:321]), centers = 6, iter.max = 10000)
clusterkmeans.vector <- clusterkmeans$cluster

## Measurements
## 1. Silhouette
sil(clusterkmeans, clusterkmeans.vector, dist(scale(copy(data.wide)[, 2:321])), "kmeans")
### s = 0.1286251

## 2. Timeline
dat.kmeans <- copy(data.wide)[, cluster := clusterkmeans.vector]
Cl.timeline(copy(dat.kmeans))
### TLS = 0.2433221

## 3. Mosaic
mosaic(copy(data.wide), clusterkmeans.vector, title = "CLARA WITH EUC")
### HB_diff = 0.5166874



# 6. PAM with Mahalanobis distance ################################
# 6.1 scale data
data.scaled <- scale(data.wide[, 2:321], scale=TRUE, center=TRUE)

## compute distance and save it in dist_mahal_scaled, this takes a lot of time, maybe not run the lines 116-118,
## and just have a look at the measurements
dist_mahal_scaled <- matrix(NA, nrow = n, ncol = n)

for(i in seq_len(n)){
  dist_mahal_scaled[i,] <- mahalanobis(data.scaled, data.scaled[i,], variance)
}

rownames(dist_mahal_scaled) <- colnames(dist_mahal_scaled) <- rn <- 1:1826

dist_mahal_scaled <- as.dist(dist_mahal)

saveRDS(dist_mahal_scaled, "Data/dist_mahal_scaled.rds")
dist_mahal_scaled <- readRDS("Data/dist_mahal_scaled.rds")

# compute number of clusters
sil_widthMahal <- c(NA)
for(i in 5:9){
  pam_fitMahal <- pam(as.dist(dist_mahal_scaled),
                 diss = TRUE,
                 k = i)
  
  sil_widthMahal[i-4] <- pam_fit$silinfo$avg.width
}
plot(5:9, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width",)
lines(5:9, sil_width)

# clustering
clusterpam.mahal <- pam(as.dist(dist_mahal_scaled), k = 6, diss = TRUE)
clusterpam.mahal.vector <- clusterpam.mahal$clustering


## Measurements 
## 1. Silhouette
sil(clusterpam.mahal, clusterpam.mahal$clustering, as.dist(dist_mahal_scaled), "pam")
### s = -0.0014021

## 2. Timeline
dat.pam.mahal <- copy(data.wide)[, cluster := clusterpam.mahal$clustering]
Cl.timeline(copy(dat.pam.mahal))
### TLS = -0.665745

## 3. Mosaic
mosaic(copy(data.wide), clusterpam.mahal.vector, title = "PAM WITH MAHAL")
### HB_diff = 0.4126577

