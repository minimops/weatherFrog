
library(cluster)
?daisy
dissimilarity <- daisy(as.data.frame(discrete[, .(minimum, intensitaet.tief, quadrant.min, 
                                                  maximum, intensitaet.hoch, quadrant.max)]), metric = "gower")
summary(dissimilarity)

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

for(i in 2:10){
  
  pam_fit <- pam(dissimilarity,
                 diss = TRUE,
                 k = i)
  
  sil_width[i] <- pam_fit$silinfo$avg.width
  
}

# Plot sihouette width (higher is better)

plot(1:10, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:10, sil_width)
# 9 Cluster scheinen hier am besten zu sein, je höher, desto besser

# hier wird das clustering angewandt
?pam
pam_fit <- pam(dissimilarity, diss = TRUE, k = 9)
cluster_vector <- pam_fit$clustering

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

(clust_table_pam <- round(
  prop.table(table(discrete_gwl_cluster$gwl, discrete_gwl_cluster$cluster), margin = 1), 2))
plot(clust_table_pam)

(clust_table_pam2 <- round(
  prop.table(table(discrete_gwl_cluster$cluster, discrete_gwl_cluster$gwl), margin = 1), 2))
plot(clust_table_pam2)

library(ggmosaic)
mosaic_cluster <- ggplot(data = discrete_gwl_cluster) + geom_mosaic(aes(x = product(gwl, cluster), fill = gwl), 
                                                                    na.rm = TRUE) +
  labs(x = " Cluster", y = "GWL")

mosaic_gwl <- ggplot(data = discrete_gwl_cluster) + geom_mosaic(aes(x = product(cluster, gwl), fill = as.factor(cluster)), 
                                                                na.rm = TRUE) 


mosaicplot(table(discrete_gwl_cluster$gwl, discrete_gwl_cluster$cluster), color = TRUE)
mosaicplot(table(discrete_gwl_cluster$cluster, discrete_gwl_cluster$gwl), color = TRUE)
?mosaicplot

