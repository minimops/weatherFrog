#### CLUSTERING EXTRAHIERTE VARIABLEN ###############################################################

source("clustering/cluster_extr_var/f_extr_funs.R")

library(dplyr)
library(cluster)
library(Rtsne)
library(ggplot2)

# this is a function to compute the dissimilarity matrix for pam clustering and the silhouette which
# speciefies k (clusters)

# INPUT: - data: a data.table which is already scaled
#        - weights: the weights for the variables (only if the dt hasnt already been weighted)
#        - metric: either euclidean, manhatten or gower
#        - dist: logical, indicating whether just the dissimilarity matrix has to be computed

dissimilarityPAM <- function(data, weights = c(rep(c(1/9, 1/9, 1/6, 1/6, 1/18, 1/18, 1/9, 1/9, 1/9), 2), 
                                               rep(1/6, 12), rep(1/18, 18)), 
                             metric = "euclidean", dist = TRUE) {
  assertDataTable(data)
  assertNumeric(weights, null.ok = TRUE)
  assertSubset("date", colnames(data)[1])
  assertSubset(metric, choices = c("euclidean", "gower", "manhattan"))
  assertLogical(dist)
  dissimilarity <- daisy(data[, 2:ncol(data)], metric = metric, weights = weights)
  
  if (dist) {
    return(dissimilarity)
  }
  
  sil_width <- c(NA)
  for(i in 5:9){
    pam_fit <- pam(dissimilarity,
                   diss = TRUE,
                   k = i)
    
    sil_width[i-4] <- pam_fit$silinfo$avg.width
  }
  plot(5:9, sil_width,
       xlab = "Number of clusters",
       ylab = "Silhouette Width",)
  lines(5:9, sil_width)
  print(sil_width)
  return(dissimilarity)
}

datscale <- scaleNweight(copy(datafinal))
diss.gower.pam <- dissimilarityPAM(copy(datscale), dist = FALSE)
summary(dissimilarity)

# do clustering with PAM
pam_fit <- pam(diss.gower.pam, diss = TRUE, k = 7)
cluster_vector <- pam_fit$clustering


############################################################################
############################## PAM #########################################

####### PAM with Gower ######################
diss.pam.gower <- dissimilarityPAM(copy(datscale), metric = "gower", dist = FALSE)

pam.gower <- pam(diss.pam.gower, diss = TRUE, k = 6)
cluster.gower <- pam.gower$clustering
mosaic(copy(data), cluster.gower, title = "PAM WITH GOWER")
# sil_width: 0.144

## Measurement 
# 1.
sil(pam.gower, cluster.gower, diss.pam.gower, "pam")
?manova
dat <- copy(data)
dat.gower <- copy(dat)[, cluster := cluster.gower]
# 2.
Cl.timeline(copy(dat.gower))
# 3.
model.pam.gower <- manova(as.matrix(dat.gower[, 2:49]) ~ dat.gower$cluster)
summary(as.matrix(dat.gower[, 2:49]) ~ dat.gower$cluster, test = "Wilks")
summary.aov(model.pam.gower)
# 4.
mosaic(copy(data), cluster.gower, title = "PAM WITH GOWER")

####### PAM with Euclidean ##################
diss.pam.euc <- dissimilarityPAM(copy(datscale), metric = "euclidean", dist = FALSE)

pam.euc <- pam(diss.pam.euc, diss = TRUE, k = 6)
cluster.euc <- pam.euc$clustering
mosaic(copy(data), cluster.euc, title = "PAM WITH EUCLIDEAN")
# sil_width: 0.114

## Measurement 
# 1.
sil(pam.euc, cluster.euc, diss.pam.euc, "pam")
?manova
dat.euc <- copy(dat)[, cluster := cluster.euc]
# 2.
Cl.timeline(copy(dat.euc))
# 3.
model.pam.euc <- manova(as.matrix(dat.euc[, 2:49]) ~ dat.euc$cluster)
summary(as.matrix(dat.euc[, 2:49]) ~ dat.euc$cluster, test = "Wilks")
summary.aov(model.pam.euc)
# 4.
mosaic(copy(data), cluster.euc, title = "PAM WITH EUC")


####### PAM with Manhattan ##################
diss.pam.manhat <- dissimilarityPAM(copy(datscale), metric = "manhattan", dist = FALSE)

pam.manhat <- pam(diss.pam.manhat, diss = TRUE, k = 6)
cluster.manhat <- pam.manhat$clustering
mosaic(copy(data), cluster.manhat, title = "PAM WITH MANHATTAN")
# sil_width: 0.149

## Measurement 
# 1.
sil(pam.manhat, cluster.manhat, diss.pam.manhat, "pam")
?manova
dat.manhat <- copy(dat)[, cluster := cluster.manhat]
# 2.
Cl.timeline(copy(dat.manhat))
# 3.
model.pam.manhat <- manova(as.matrix(dat.manhat[, 2:49]) ~ dat.manhat$cluster)
summary(as.matrix(dat.manhat[, 2:49]) ~ dat.manhat$cluster, test = "Wilks")
summary.aov(model.pam.manhat)
# 4.
mosaic(copy(data), cluster.manhat, title = "PAM WITH MANHAT")


####### PAM with Mahalanobis ###############
diss.pam.mahalanobis <- dist_mahal_scaled

pam.mahal <- pam(diss.pam.mahalanobis, diss = TRUE, k = 4)
cluster.mahal <- pam.mahal$clustering
mosaic(copy(data), cluster.mahal, title = "PAM WITH MAHALANOBIS")

for(i in 4:15){
  pam_fit <- pam(diss.pam.mahalanobis,
                 diss = TRUE,
                 k = i)
  
  sil_width[i-3] <- pam_fit$silinfo$avg.width
}
plot(4:15, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width",)
lines(4:15, sil_width)
# silhouette width lower than 0 -> not effective

## Measurement 
# 1.
sil(pam.mahal, cluster.mahal, diss.pam.mahalanobis, "pam")
?manova
dat.mahal <- copy(dat)[, cluster := cluster.mahal]
# 2.
Cl.timeline(copy(dat.mahal))



###########################################################################
############################# KMEANS ######################################
?kmeans

####### KMEANS with Euclidean ###############
km.list.euc <- list()
wss.euc <- numeric()

for (k in 5:9){
  km.list.euc[[k]] <- kmeans(copy(datscale)[, 2:49], centers=k, iter.max = 5000, nstart = 5)
  wss.euc[k] <- sum(km.list.euc[[k]]$withinss)
}
plot(5:9, wss.euc[5:9], type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

kmeans.euc <- kmeans(copy(datscale)[, 2:49], iter.max = 10000, nstart = 5, centers = 6)
mosaic(copy(datafinal), kmeans.euc$cluster, "KMEANS WITH EUCLIDEAN")


## Measurement 
# 1. Silhouettenkoeffizient
sil(kmeans.euc, kmeans.euc$cluster, dist(copy(datscale)[, 2:49]), "kmeans")
### s = 0.1206552

# 2. Timeline
dat.kmeans <- copy(datafinal)[, cluster := kmeans.euc$cluster]
Cl.timeline(copy(dat.kmeans))
### TLS = 0.2913967

# 4. Mosaic
mosaic(copy(datafinal), kmeans.euc$cluster, title = "PAM WITH MANHAT")
### HBdiff = 0.352828

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
### s = 0.1190865

# 2.
dat.kmeans.weighted <- copy(datafinal)[, cluster := kmeans.euc.weighted$cluster]
Cl.timeline(copy(dat.kmeans.weighted))
### TLS = 0.4454835

# 3. Mosaik
mosaic(copy(data), kmeans.euc.weighted$cluster, title = "PAM WITH MANHAT")
### HBdiff = 0.3379194
####### KMEANS with Gower ###################

# geht nicht!
  
####### KMEANS with Manhattan ###############

# geht nicht!



# dissimilarity als matrix speichern
infoCluster <- function(data, dissimilarity, cluster_vector) {
  assertDataTable(data)
  
  gower_mat <- as.matrix(dissimilarity)
  
  # output most similar pair
  similar <- data[which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]),
                        arr.ind = TRUE)[1, ], ]
  # output most dissimilar pair
  dissimilar <- data[which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]),
                           arr.ind = TRUE)[1, ], ]
  
  # für eine summary über die cluster
  pam_results <- data[, 2:ncol(data)] %>%
    mutate(cluster = cluster_vector) %>%
    group_by(cluster) %>%
    do(the_summary = summary(.))
  
  list("most similar pair:" = similar, "most dissimilar pair:" = dissimilar, 
       "Summary over cluster:" = pam_results$the_summary)
  
}


infoCluster(copy(data), dissimilarity, cluster_vector)

## visualization
#install.packages("Rtsne")

# somehow it differs every time, set.seed doesnt help either

tsne <- function(data, dissimilarity, cluster_vector) {
  assertDataTable(data)
  assertInteger(cluster_vector)
  
  tsne_obj <- Rtsne(dissimilarity, is_distance = TRUE)

  tsne_data <- tsne_obj$Y %>%
    data.frame() %>%
    setNames(c("X", "Y")) %>%
    mutate(cluster = factor(cluster_vector),
           date = data$date)

  ggplot(aes(x = X, y = Y), data = tsne_data) +
    geom_point(aes(color = cluster))
}

tsne(copy(data), dissimilarity, cluster_vector)
### okay, da muss ich nochmal genauer schauen, was da wirklich gemacht wird, hab das von dieser website
# https://www.r-bloggers.com/2016/06/clustering-mixed-data-types-in-r/
?Rtsne
