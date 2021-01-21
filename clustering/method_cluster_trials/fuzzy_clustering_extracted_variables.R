# Fuzzy Clustering with extracted variables 

# generate extracted dataset
source("clustering/Var_Extraction_Method/f_extr_funs.R")

# choose 30 years
extract_data_30 <- extrapolate(seq(2005, 2010))

# preparation for fuzzy c-means clustering


library(ppclust)
library(factoextra)
library(cluster)
library(fclust)

# fuzzy c-means clustering with single start and euclidean metric
x <- as.data.frame(extract_data_30)

res.fm <- fcm(x[,-1], centers = 3)

# cluster plot with fviz_cluster
res.fcm2 <- ppclust2(res.fm,"kmeans")
factoextra::fviz_cluster(res.fcm2,data = x[,-1],
                          ellipse.type ="convex",
                         palette ="jco",
                         repel = TRUE)

# cluster plot with clustplot

res.fcm3 <- ppclust2(res.fm, "fanny")
cluster::clusplot(scale(x[,-1]), res.fcm3$cluster,
                  main = "cluster Versuch",
                  color = T, labels = 2, lines =2, cex = 2)

# Validation of cluster result: mesure variables extra for fuzzy clustering

res.fcm4 <- ppclust2(res.fm,"fclust")
#Fuzzy silhouetten index
SIL.F(res.fcm4$Xca,res.fcm4$U,alpha = 1)

#Partition entropy
PE(res.fcm4$U)

#Partition coefficient
PC(res.fcm4$U)

#Modifiet partiton index
MPC(res.fm$u)




#MANOVA
cluster <- res.fm$cluster
manovaFUN(x[,-1],cluster)
