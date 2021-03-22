#### clustering extrapolated data, 1. Try with 5Years
# cluster with PAM - Manhattan, PAM - euclidean, PAM - gower, Kmeans - euclidean

source("clustering/cluster_extr_var/f_extr_funs.R")
source("clustering/ClusterAssesmentHelper.R")

library(dplyr)
library(cluster)
library(Rtsne)
library(ggplot2)


data <- extrapolate(yearspan = seq(2006, 2010), "all")
datscale <- scaleNweight(copy(data))


############################################################################
############################## PAM #########################################

####### PAM with Gower ######################
diss.pam.gower <- dissimilarityPAM(copy(datscale), metric = "gower", dist = FALSE)

pam.gower <- pam(diss.pam.gower, diss = TRUE, k = 6)
cluster.gower <- pam.gower$clustering


## Measurement 
# 1.
sil(pam.gower, cluster.gower, diss.pam.gower, "pam")


dat.gower <- copy(data)[, cluster := cluster.gower]
# 2.
Cl.timeline(copy(dat.gower))

# 3.
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

dat.euc <- copy(data)[, cluster := cluster.euc]
# 2.
Cl.timeline(copy(dat.euc))
# 3.
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

dat.manhat <- copy(data)[, cluster := cluster.manhat]
# 2.
Cl.timeline(copy(dat.manhat))
# 3.
mosaic(copy(data), cluster.manhat, title = "PAM WITH MANHAT")





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

