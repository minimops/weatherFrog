
library(cluster)
?daisy
dissimilarity <- daisy(as.data.frame(discrete[, .(scale(min.mslp), scale(intensitaet.tief.mslp), quadrant.min.mslp,
                                                  quadrant.max.mslp, scale(max.mslp), scale(intensitaet.hoch.mslp),  
                                                  scale(mean.mslp), scale(median.mslp), scale(range.mslp),
                                                  scale(mean.geopot), scale(median.geopot), scale(range.geopot),
                                                  scale(min.geopot), quadrant.min.geopot, scale(intensitaet.tief.geopot),
                                                  quadrant.max.geopot, scale(intensitaet.hoch.geopot), scale(max.geopot), 
                                                  scale(euclidean.mslp), scale(euclidean.geopot))]), 
                                                  weights = c(1 / 3, 1 / 3, 1 / 3, 
                                                              1 / 3, 1 / 3, 1 / 3, 
                                                              1 / 3, 1 / 3, 1 / 3,
                                                              1 / 3, 1 / 3, 1 / 3,
                                                              1 / 3, 1 / 3, 1 / 3, 
                                                              1 / 3, 1 / 3, 1 / 3,
                                                              0.5, 0.5),
                                                  metric = "gower")
summary(dissimilarity)

?daisy
# dissimilarity als matrix speichern
gower_mat <- as.matrix(dissimilarity)

# output most similar pair
discrete[
  which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]),
        arr.ind = TRUE)[1, ], ]
# output most dissimilar pair
discrete[
  which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]),
        arr.ind = TRUE)[1, ], ]

# Calculate silhouette width for many k using PAM

sil_width <- c(NA)

for(i in 2:15){
  
  pam_fit <- pam(dissimilarity,
                 diss = TRUE,
                 k = i)
  
  sil_width[i] <- pam_fit$silinfo$avg.width
  
}

# Plot sihouette width (higher is better)

plot(1:15, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:15, sil_width)
# 8 Cluster scheinen hier am besten zu sein, je höher, desto besser

# hier wird das clustering angewandt
?pam
pam_fit <- pam(dissimilarity, diss = TRUE, k = 11)

cluster_vector <- pam_fit$clustering

pam_fit$medoids
library(dplyr)
# für eine summary über die cluster
pam_results <- discrete[, .(minimum, intensitaet.tief, quadrant.min, 
                            maximum, intensitaet.hoch, quadrant.max)] %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))

pam_results$the_summary

discrete[pam_fit$medoids, ]

## visualization
#install.packages("Rtsne")
library(Rtsne)
tsne_obj <- Rtsne(dissimilarity, is_distance = TRUE)

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering),
         date = discrete$date)

library(ggplot2)
ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))
### okay, da muss ich nochmal genauer schauen, was da wirklich gemacht wird, hab das von dieser website
# https://www.r-bloggers.com/2016/06/clustering-mixed-data-types-in-r/

### Mosaikplot

discrete_gwl <- gwl[discrete, on = .(date)]
discrete_gwl_cluster <- discrete_gwl[, cluster := cluster_vector]


mosaicplot(table(discrete_gwl_cluster$gwl, discrete_gwl_cluster$cluster), color = TRUE)
mosaicplot(table(discrete_gwl_cluster$cluster, discrete_gwl_cluster$gwl), color = TRUE)
?mosaicplot

library(factoextra)
clust_gower <- agnes(gower_mat, method = "complete")
summary(clust_gower)
clust_gower$method
summary(clust_gower)$ac
plot(clust_gower)
fviz_dist(dissimilarity)
a <- 1
a
