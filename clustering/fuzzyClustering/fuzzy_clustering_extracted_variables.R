

# generate extracted dataset
source("clustering/Var_Extraction_Method/f_extr_funs.R")

# choose 30 years
extract_data_30 <- extrapolate(seq(1971, 2000))


# Jackpot: Fuzzy k means mit Gustafon-Kessel-Extention
# benutzt statt eukldischer Distanz die Mahalanobis Distanz 
# euklidische Dustanz führt zu spherischen Clusterlösungen, dadurch können
#eventuell die Cluster nicht richtig erkannt werden 



source("clustering/fuzzyClustering/function_for_fuzzy_clustering.R")

# lieber nicht durchlaufen lassen, dauert ewig, habe aber den output in github hochgeladen

#unscaled_gk_5_26 <- best_cluster_number(5,26,extract_data_30)
#scaled_gk_5_26 <- best_cluster_number(5,26,extract_data_30,scale = TRUE)

#save(unscaled_gk_5_26,file = "clustering/fuzzyClustering/unscaled_gk_6_26_30.RData")
#save(scaled_gk_5_26,file = "clustering/fuzzyClustering/scaled_gk_6_26_30.RData")



# Validation 

source("clustering/ClusterAssesmentHelper.R")

#attach gwl with data

extract_data_30_gwl <- attachGwl(extract_data_30)

# gk unscaled

# optimale Clusterzahl: 5
gk5 <- unscaled_gk_5_26[[1]]
gk8 <- unscaled_gk_5_26[[4]]
gk11 <- unscaled_gk_5_26[[7]]
gk15 <- unscaled_gk_5_26[[11]]


data <- cbind(gk11$cluster,extract_data_30_gwl)
Cl.timeline(data, "V1", titleAdd = "Clustering gk with cluster number 8",seperated = T)
sil_fun(gk11,extract_data_30[,-1])
mosaic(extract_data_30_gwl,gk15$cluster,title = "fuzzy gk unscaled with 8 ")
#noiseAllocation(gk5$cluster,gk5$u)
manovaFUN(extract_data_30[,-1],gk5$cluster)





# gk scaled
#optimale Clusterzahl:6
gk6_scaled <- scaled_gk_5_26[[2]]
gk11_scaled <- scaled_gk_5_26[[7]]

data <- cbind(gk11_scaled$cluster,extract_data_30_gwl)
Cl.timeline(data, "V1", titleAdd = "Clustering gk with cluster number 6 scaled",seperated = T)
sil_fun(gk11_scaled,extract_data_30[,-1])
mosaic(extract_data_30_gwl,gk11_scaled$cluster,title = "fuzzy gk scaled with 6 ")
#noiseAllocation(gk5$cluster,gk5$u)
manovaFUN(extract_data_30[,-1],gk5$cluster)


# Fuzzy clustering with eucledeam distance

# Achtung: nicht durchlaufen lassen, sondern entsprechende R Objekte laden

#fcm_unscaled_6 <- fcm(extract_data_30[,-1],6, iter.max = 500) 
#fcm_unscaled_8 <- fcm(extract_data_30[,-1],8, iter.max = 500)
#fcm_unscaled_9 <- fcm(extract_data_30[,-1],9, iter.max = 500) 
#fcm_unscaled_10 <- fcm(extract_data_30[,-1],10, iter.max = 500)
#fcm_unscaled_11 <- fcm(extract_data_30[,-1],11, iter.max = 500)
#fcm_unscaled_15 <- fcm(extract_data_30[,-1],15, iter.max = 500)
#fcm_unscaled_list <- list(fcm_unscaled_6,fcm_unscaled_8,fcm_unscaled_9,fcm_unscaled_10,fcm_unscaled_11, fcm_unscaled_15)
#name <- c("fcm6","fcm8","fcm9","fcm10","fcm11","fcm15")
#names(fcm_unscaled_list) <- name

#save(fcm_unscaled_list,file = "clustering/fuzzyClustering/unscaled_fcm_6_26_30.RData")



#validation variables for best cluster number

fcm_unscaled_6 <- fcm_unscaled_list[[1]]
fcm_unscaled_8 <- fcm_unscaled_list[[2]]
fcm_unscaled_9 <- fcm_unscaled_list[[3]]
fcm_unscaled_10 <- fcm_unscaled_list[[4]]
fcm_unscaled_11 <- fcm_unscaled_list[[5]]
fcm_unscaled_15 <- fcm_unscaled_list[[6]]


valid_fcm_unscaled_6 <- validation_variables(fcm_unscaled_6)
valid_fcm_unscaled_8 <- validation_variables(fcm_unscaled_8)
valid_fcm_unscaled_9 <- validation_variables(fcm_unscaled_9)
valid_fcm_unscaled_10 <- validation_variables(fcm_unscaled_10)
valid_fcm_unscaled_11 <- validation_variables(fcm_unscaled_11)
valid_fcm_unscaled_15 <- validation_variables(fcm_unscaled_15)
cluster_number <- c(6,8,9,10,11,15)
valid_fcm <- as.data.frame(rbind(valid_fcm_unscaled_6,valid_fcm_unscaled_8,valid_fcm_unscaled_9,valid_fcm_unscaled_10,valid_fcm_unscaled_11,valid_fcm_unscaled_15))
valid_fcm <- cbind(cluster_number,valid_fcm)
colnames(valid_fcm) <- c("cluster_number","fuzzy_silhouette","partition_entropy","partition_coef","modified_part_coef")



plot(valid_fcm$cluster_number,valid_fcm$fuzzy_silhouette,  type = "b", main = "fuzzy silhouette")
plot(valid_fcm$cluster_number,valid_fcm$partition_entropy,  type = "b", main = "partition_entropy")
plot(valid_fcm$cluster_number,valid_fcm$partition_coef, type = "b", main = "partition coefficient")
plot(valid_fcm$cluster_number,valid_fcm$modified_part_coef,  type = "b",main = "modified partition coefficient")


# validation of cluster

# 9 cluster
data <- cbind(fcm_unscaled_9$cluster,extract_data_30_gwl)
Cl.timeline(data, "V1", titleAdd = "Clustering fuzzy with cluster number 9 scaled",seperated = T)
sil_fun(fcm_unscaled_9,extract_data_30[,-1])
mosaic(extract_data_30_gwl,fcm_unscaled_9$cluster,title = "fuzzy  scaled with 9 ")
#noiseAllocation(gk5$cluster,gk5$u)
manovaFUN(extract_data_30[,-1],fcm_unscaled_9$cluster)

# 15 cluster
data <- cbind(fcm_unscaled_15$cluster,extract_data_30_gwl)
Cl.timeline(data, "V1", titleAdd = "Clustering fuzzy with cluster number 9 scaled",seperated = T)
sil_fun(fcm_unscaled_15,extract_data_30[,-1])
mosaic(extract_data_30_gwl,fcm_unscaled_15$cluster,title = "fuzzy  scaled with 9 ")
#noiseAllocation(gk5$cluster,gk5$u)
manovaFUN(extract_data_30[,-1],fcm_unscaled_15$cluster)


# Cluster Plots ( werden eventuell später noch benötigt)

# cluster plot with fviz_cluster
#res.fcm2 <- ppclust2(res.fcm6,"kmeans")
#factoextra::fviz_cluster(res.fcm2,data = x[,-1],
#                          ellipse.type ="convex",
#                         palette ="jco",
#                         repel = TRUE)



# cluster plot with clustplot nür für inputs von package ppclust

#res.fcm3 <- ppclust2(res.fcm6, "fanny")
#cluster::clusplot(scale(x[,-1]), res.fcm3$cluster,
#                 main = "cluster Versuch",
#                color = T, labels = 2, lines =2, cex = 2)



