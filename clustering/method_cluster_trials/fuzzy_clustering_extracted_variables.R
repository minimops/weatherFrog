# Fuzzy Clustering with extracted variables 

library(ppclust)
library(factoextra)
library(cluster)
library(fclust)
library(e1071) #for cmeans


# generate extracted dataset
source("clustering/Var_Extraction_Method/f_extr_funs.R")

# choose 30 years
extract_data_30 <- extrapolate(seq(1971, 2000))

# ellbow plot for choosing optimal numer of cluster 
set.seed(123)
fviz_nbclust(extract_data_30[,-1],kmeans, method ="silhouette")
# sagt, dass das beste 2 cluster wären....


# fuzzy c-means clustering with single start and euclidean metric
# fcm

res.fcm6 <- fcm(extract_data_30[,-1], centers = 6)
## dauert zu lange. 30 Minuten gewartet ohne ergebnis bei 30 Jahren






# cluster plot with fviz_cluster
res.fcm2 <- ppclust2(res.fcm6,"kmeans")
factoextra::fviz_cluster(res.fcm2,data = x[,-1],
                          ellipse.type ="convex",
                         palette ="jco",
                         repel = TRUE)

# cluster plot with clustplot nür für inputs von package ppclust

res.fcm3 <- ppclust2(res.fcm6, "fanny")
cluster::clusplot(scale(x[,-1]), res.fcm3$cluster,
                  main = "cluster Versuch",
                  color = T, labels = 2, lines =2, cex = 2)

# Validation of cluster result: mesure variables extra for fuzzy clustering
 
res.fcm4 <- ppclust2(res.fcm6,"fclust")
#Fuzzy silhouetten index
SIL.F(res.fcm4$Xca,res.fcm4$U,alpha = 1)

#Partition entropy
PE(res.fcm4$U)

#Partition coefficient
PC(res.fcm4$U)


#Modifiet partiton index
MPC(res.fm$u)




# fuzzy clustering with cmeans und euclidean metric
res.fcm6 <- cmeans(extract_data_30[,-1],centers = 6,iter.max = 500)
### juhu, geht sehr schnell zur Abwechslung
#Problem PC etc. lässt sich nicht berechnen, äre aber wichtig,
#um die optimale clusterzahl herauszubekommen

#Visualisierung
fviz_cluster(list(data = extract_data_30[,-1],cluster = res.fcm6$cluster),
             ellipse.type = "norm",
             ellipse.level = 0.68,
             palette("jco"),
             ggtheme = theme_minimal())



# Jackpot: Fuzzy k means mit Gustafon-Kessel-Extention
# benutzt statt eukldischer Distanz die Mahalanobis Distanz 
# euklidische Dustanz führt zu spherischen Clusterlösungen, dadurch können
#eventuell die Cluster nicht richtig erkannt werden 

# kann folgend implementiert werden: 
#FKM.gk im package fclust
#gk im package ggclust

FKM1 <- FKM.gk(extract_data_30[,-1], k = 5:10, index = "SIL.F",
               RS = 10, seed = 123)

FKM1 <- FKM.gkb(extract_data_30[,-1], k = 5:10, index = "SIL.F",
               RS = 10, seed = 123)



