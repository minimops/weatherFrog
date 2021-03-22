#pca plot pres

source("clustering/ClusterAssesmentHelper.R")

library(ggfortify)

dat_wide <- readRDS("Data/cli_data_30_avgDay_wide.rds")

pca30 <- prcomp(as.data.frame(dat_wide)[, 2:321], scale. = TRUE)

pcaPlot <- autoplot(pca30, alpha = 0.1, colour = "darkblue") +
  theme_bw() + 
  ggtitle("Visualisierung der Daten mit PCA")

ggsave("documentation/plots/fplots/pcaPlot.png", pcaPlot, device = "png",
       width = 5, height = 3)


#pca analysis for report
plot(pca30, main = "Screeplot PCA")

cumvar <- cumsum(pca30$sdev^2 / sum(pca30$sdev^2))
(pc.index<-min(which(cumvar>0.85)))

pca30_cluster <- pca30$x[, 1:12]

library(factoextra)

fviz_nbclust(pca30_cluster, kmeans, method = "silhouette") +
  geom_vline(xintercept = 6, linetype = 2)


k2 <- kmeans(pca30_cluster, centers = 6, nstart = 25, iter.max = 30)



sil(k2, k2$cluster, dist(pca30_cluster), "kmeans")
Cl.timeline(cbind(dat_wide, cluster = k2$cluster), multiplied = TRUE)

mosaic(dat_wide, k2$cluster, "PCA")
