library(ggplot2)
library(ggfortify)
library(cluster)
library(data.table)

saveRDS(gwl_data_split, "Daten//gwl_data_split.RDS")
gwl1 <- readRDS("Daten/gwl_data_split.RDS")
head(gwl1)
str(gwl1)
summary(gwl1)
dim(gwl1)


gwl1 <- as.data.frame(gwl1)
table(gwl1$gwl)
?clara

#table

table(gwl1$gwl, gwl1$day)
gwl1$day <- as.data.frame((gwl1$day))
#continuing with 10 for now
g_cluster <- gwl1$dayx[, 1:10000]
class(g_cluster)
library(factoextra)
#optimal number of clusters
fviz_nbclust(gwl1$day, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)
#clustering
g<- kmeans(gwl1$day, centers = 4, nstart = 25)
plot(g, gwl1$gwl)

#pca
str(gwl1$day)
gwl1$day <- as.numeric(gwl1$day)
gwl1_pca_2 <- prcomp(as.data.frame(gwl1$day))

