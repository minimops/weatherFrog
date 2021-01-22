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
#gk im package ppclust

FKM1 <- FKM.gk(extract_data_30[,-1], k = 5:10, index = "SIL.F",
               RS = 10, seed = 123)

FKM1 <- FKM.gkb(extract_data_30[,-1], k = 5:10, index = "SIL.F",
               RS = 10, seed = 123)

FKM9 <- gk(extract_data_30[,-1], centers = 9)
# hier bekomme ich sogar mal nach relativ kurzer zeit ein ergebnis. 
# Nachteil: optimale Clusterzahl muss selbst bestimmt werden 
# Da die Methode aber aus dem package ppklust ist, kann man die
#Analyse mit verschiedenen Gruppenzahlen durchlaufen lassen und dann zur Bewertung
# PC oder silouehtte oder anderes hernehmen


# Validation of cluster result: mesure variables extra for fuzzy clustering
validation_variables <- function(fuzzy_output){
  measure_vec <- vector()
  res.FKM.9 <- ppclust2(fuzzy_output,"fclust")
  #Fuzzy silhouetten index maximum
  fuzzy.sil <- SIL.F(res.FKM.9$Xca,res.FKM.9$U,alpha = 1)
  print(paste("Fuzzy Silhouette Index:",fuzzy.sil))
  
  #Partition entropy minimum
  part_ent <- PE(res.FKM.9$U)
  print(paste("Partition Entropy: ", part_ent))
  
  #Partition coefficient maximum
  part_coef <- PC(res.FKM.9$U)
  print(paste("Partition Coefficient: ", part_coef))
  
  #Modifiet partiton index maximum
  mod_part_ind <- MPC(res.FKM.9$U)
  print(paste("Modified Partition Coefficient: ", mod_part_ind))
  
  measure_vec <- c(fuzzy.sil,part_ent,part_coef,mod_part_ind)
  measure_vec
  
  
} 


validation_variables(FKM9)

best_cluster <- function(begin, end){
  vec <- seq(from = begin, to = end, step = 1)
  measue_mat <- matrix(ncol = 4)
  measure_mat <- as.data.frame(matrix(ncol = 4))
  
  for(i in vec){
    paste("FKM",i) <- gk(extract_data_30[,-1], centers = i)
    paste("validation",i) <- validation_variables(paste("FKM",i))
    measure_mat[i,] <-  paste("validation",i)
  }
  
    measure_mat <- as.data.frame(measure_mat)
    k <- seq(from = begin, to = end, by = 1)
    measure_mat <- cbind(k,measure_mat)
    colnames(measure_mat) <- c("cluster","fuzzy_silhouette","partition_entropy","partition_coef","modified_part_coef")
    
    # minimum und maxima of the validation variables for optimal cluster numer
    which.max(measure_mat[,2])
    which.min(measure_mat[,3])
    which.max(measure_mat[,4])
    which.max(measure_mat[,5])
    
    # visualisation of validation variables in a plot
    plot(measure_mat[,1], measure_mat[,2], col = "red", type = "l")
         lines(measure_mat[,1], measure_mat[,3], col ="green", type = "l")
         lines(measure_mat[,1], measure_mat[,4], col ="blue", type = "l")
         lines(measure_mat[,1], measure_mat[,5], col ="black", type = "l")
  
}

bestCluster(2,3)
