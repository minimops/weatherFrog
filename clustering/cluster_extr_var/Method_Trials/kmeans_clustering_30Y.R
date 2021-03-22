### K-Means 30 years

source("clustering/ClusterAssesmentHelper.R")
source("clustering/cluster_extr_var/f_extr_funs.R")

data <- readRDS("Data/f_data.rds")
datscale <- scaleNweight(copy(data))

set.seed(123)
# forgot to set seed when clustering for results in report.


####### KMEANS with Euclidean ###############
km.list.euc <- list()
wss.euc <- numeric()

for (k in 5:9){
  km.list.euc[[k]] <- kmeans(copy(datscale)[, 2:49], centers=k, iter.max = 5000, nstart = 5)
  wss.euc[k] <- sum(km.list.euc[[k]]$withinss)
}
plot(5:9, wss.euc[5:9], type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

kmeans.euc <- kmeans(copy(datscale)[, 2:49], iter.max = 10000, nstart = 5, centers = 6)


## Measurement 
# 1. Silhouettenkoeffizient
sil(kmeans.euc, kmeans.euc$cluster, dist(copy(datscale)[, 2:49]), "kmeans")
### s = 0.1208965

# 2. Timeline
dat.kmeans <- copy(data)[, cluster := kmeans.euc$cluster]
Cl.timeline(copy(dat.kmeans))
### TLS = 0.2777393

# 3. Mosaic
mosaic(copy(data), kmeans.euc$cluster, title = "PAM WITH MANHAT")
### HBdiff = 0.3551077

####### KMEANS with Euclidean weighted ###############
km.list.euc.weighted <- list()
wss.euc.weighted <- numeric()

for (k in 5:9){
  km.list.euc.weighted[[k]] <- kmeans(scaleNweight(copy(data), weight = TRUE, 
                                                   weights = c(rep(c(1/3, 1/6, rep(1/3, 2), rep(1/6, 4)), 2),
                                                               rep(1 / 6, 12), rep(1/9, 18), rep(1/6, 2)))[, 2:49]
                                      , centers=k, iter.max = 5000, nstart = 5)
  wss.euc.weighted[k] <- sum(km.list.euc.weighted[[k]]$withinss)
}
plot(5:9, wss.euc.weighted[5:9], type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

kmeans.euc.weighted <- kmeans(scaleNweight(copy(data), weight = TRUE,
                                           weights = c(rep(c(1/3, 1/6, rep(1/3, 2), rep(1/6, 4)), 2),
                                                       rep(1 / 6, 12), rep(1/9, 18), rep(1/6, 2)))[, 2:49], iter.max = 5000, 
                              nstart = 5, centers = 6)


## Measurement 
# 1. Silhouette
sil(kmeans.euc.weighted, kmeans.euc.weighted$cluster, dist(scaleNweight(copy(data), weight = TRUE,
                                                                        weights = c(rep(c(1/3, 1/6, rep(1/3, 2), rep(1/6, 4)), 2),
                                                                                    rep(1 / 6, 12), rep(1/9, 18), rep(1/6, 2)))[, 2:49]),
    "kmeans")
### s = 0.1211194

# 2.
dat.kmeans.weighted <- copy(datafinal)[, cluster := kmeans.euc.weighted$cluster]
Cl.timeline(copy(dat.kmeans.weighted))
### TLS = 0.3938625

# 3. Mosaik
mosaic(copy(data), kmeans.euc.weighted$cluster, title = "PAM WITH MANHAT")
### HBdiff = 0.3280676



####### KMEANS with Gower ###################
# geht nicht!

####### KMEANS with Manhattan ###############
# geht nicht!
