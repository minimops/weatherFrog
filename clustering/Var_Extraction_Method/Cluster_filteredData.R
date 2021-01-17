#### CLUSTERING EXTRAHIERTE VARIABLEN ###############################################################

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
  assertNumeric(weights, len = ncol(data) - 1, null.ok = TRUE)
  assertSubset("date", colnames(data)[1])
  assertSubset(metric, choices = c("euclidean", "gower", "manhattan"))
  assertLogical(dist)
  dissimilarity <- daisy(data[, 2:ncol(data)], metric = metric, weights = weights)
  
  if (dist) {
    return(dissimilarity)
  }
  
  sil_width <- c(NA)
  for(i in 4:15){
    pam_fit <- pam(dissimilarity,
                   diss = TRUE,
                   k = i)
    
    sil_width[i-3] <- pam_fit$silinfo$avg.width
  }
  plot(4:15, sil_width,
       xlab = "Number of clusters",
       ylab = "Silhouette Width",)
  lines(4:15, sil_width)
  print(sil_width)
  return(dissimilarity)
}

datscale <- scaleNweight(copy(data))
diss.gower.pam <- dissimilarityPAM(copy(datscale), dist = FALSE)
summary(dissimilarity)

# do clustering with PAM
pam_fit <- pam(diss.gower.pam, diss = TRUE, k = 7)
cluster_vector <- pam_fit$clustering


### Mosaikplot

# function that print mosaicplots
# INPUT: - data with date 
#        - clustering vector of clusters
#        - title of plots, input is the used method

mosaic <- function(data, cluster_vector, title = "PAM") {
  assertDataTable(data)
  assertInteger(cluster_vector)
  assertString(title)
  assertSubset("date", colnames(data))
  
  gwl <- readRDS("Data/gwl.rds")
  data.gwl <- gwl[data, on = .(date)]
  data.gwl.cluster <- data.gwl[, cluster := cluster_vector]
  
  mosaicplot(table(data.gwl.cluster$cluster, data.gwl.cluster$gwl), color = TRUE,
             xlab = "Cluster", ylab = "GWL", cex.axis = 0.6, las = 2,
             main = paste0(title, " Cluster - GWL"))
  mosaicplot(table(data.gwl.cluster$gwl, data.gwl.cluster$cluster), color = TRUE,
             ylab = "Cluster", xlab = "GWL", cex.axis = 0.6, las = 2,
             main = paste0(title, " Cluster - GWL"))
}

mosaic(copy(data), cluster_vector)

############################################################################
############################### PAM ########################################

####### PAM with Gower ######################
diss.pam.gower <- dissimilarityPAM(copy(datscale), metric = "gower", dist = FALSE)

pam_fit <- pam(diss.pam.gower, diss = TRUE, k = 6)
cluster_vector <- pam_fit$clustering
mosaic(copy(data), cluster_vector, title = "PAM WITH GOWER")
# sil_width: 0.145

####### PAM with Euclidean ##################
diss.pam.euc <- dissimilarityPAM(copy(datscale), metric = "euclidean", dist = FALSE)

pam_fit <- pam(diss.pam.euc, diss = TRUE, k = 6)
cluster_vector <- pam_fit$clustering
mosaic(copy(data), cluster_vector, title = "PAM WITH EUCLIDEAN")
# sil_width: 0.114

####### PAM with Manhattan ##################
diss.pam.manhat <- dissimilarityPAM(copy(datscale), metric = "manhattan", dist = FALSE)

pam_fit <- pam(diss.pam.manhat, diss = TRUE, k = 6)
cluster_vector <- pam_fit$clustering
mosaic(copy(data), cluster_vector, title = "PAM WITH MANHATTAN")
# sil_width: 0.149

###########################################################################
############################# KMEANS ######################################

?kmeans

km_list <- list()
wss <- numeric()

for (k in 1:15){
  km_list[[k]] <- kmeans(copy(datscale)[, 2:49], centers=k, iter.max = 5000, nstart = 5)
  wss[k] <- sum(km_list[[k]]$withinss)
}
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

kmeans.euc <- kmeans(copy(datscale)[, 2:49], iter.max = 10000, nstart = 5, centers = 7)
mosaic(copy(data), kmeans.euc$cluster, "KMEANS WITH EUCLIDEAN")


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
