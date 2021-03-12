# Attention: don´t run this file unless you do not need your computer one day


source("clustering/Var_Extraction_Method/f_extr_funs.R")
source("clustering/fuzzyClustering/function_for_fuzzy_clustering.R")
source("clustering/ClusterAssesmentHelper.R")
source("clustering/manova_function.R")




# choose 30 years
extract_data_30 <- extrapolate(seq(1971, 2000))
extract_data_30_scaled <- scale(extract_data_30[,-1])
summary(extract_data_30_scaled)


extract_data_30_scaled <- cbind(extract_data_30[,1],extract_data_30_scaled)
extract_data_30_weighted <- scaleNweight(extract_data_30,weight = FALSE)
extract_data_30_scaled_weighted <- scaleNweight(extract_data_30,weight = TRUE)

extract_data_30_gwl <- attachGwl(extract_data_30)

###################### 
#Gustavon_Kessel: Mahalnaobis


unscaled_gk_5_26 <- best_cluster_number(5,26,extract_data_30)
scaled_gk_5_26 <- best_cluster_number(5,26,extract_data_30,scale = TRUE)

save(unscaled_gk_5_26,file = "clustering/fuzzyClustering/unscaled_gk_6_26_30.RData")
save(scaled_gk_5_26,file = "clustering/fuzzyClustering/scaled_gk_6_26_30.RData")



# Validation of gk

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



# gk scaled
#optimale Clusterzahl:6
gk6_scaled <- scaled_gk_5_26[[2]]
gk11_scaled <- scaled_gk_5_26[[7]]

data <- cbind(gk11_scaled$cluster,extract_data_30_gwl)
Cl.timeline(data, "V1", titleAdd = "Clustering gk with cluster number 6 scaled",seperated = T)
sil_fun(gk11_scaled,extract_data_30[,-1])
mosaic(extract_data_30_gwl,gk11_scaled$cluster,title = "fuzzy gk scaled with 6 ")





############################################
# Fuzzy clustering with sqared eucledeam distance fcm


#squared_ unscaled

fcm_squared_unscaled_6 <- fcm(extract_data_30[,-1],6) 
fcm_squared_unscaled_8 <- fcm(extract_data_30[,-1],8,iter.max = 500)
fcm_squared_unscaled_9 <- fcm(extract_data_30[,-1],9,iter.max = 500) 
fcm_squared_unscaled_10 <- fcm(extract_data_30[,-1],10,iter.max = 500)
fcm_squared_unscaled_11 <- fcm(extract_data_30[,-1],11,iter.max = 500)
fcm_squared_unscaled_15 <- fcm(extract_data_30[,-1],15,iter.max = 500)
fcm_squared_unscaled_list <- list(fcm_squared_unscaled_6,fcm_squared_unscaled_8,fcm_squared_unscaled_9,fcm_squared_unscaled_10,fcm_squared_unscaled_11, fcm_squared_unscaled_15)
name <- c("fcm6","fcm8","fcm9","fcm10","fcm11","fcm15")
names(fcm__squared_unscaled_list) <- name

save(fcm_squared_unscaled_list,file = "clustering/fuzzyClustering/squared_unscaled_fcm_6_26_30.RData")

fcm_unscaled_squared_list <- fcm_unscaled_list

#validation for optimal cluster number
fcm_squared_unscaled <- finding_optimal_cluster_number(fcm_unscaled_squared_list,c(6,8,9,10,11,15))


# validation of cluster

# 6 cluster (best cluster)
fcm_squared_unscaled_6 <- fcm_squared_unscaled_6 <- fcm(extract_data_30[,-1],6)
save(fcm_squared_unscaled_6,file = "clustering/fuzzyClustering/fcm_squared_unscaled_6.RData" )

data <- cbind(fcm_squared_unscaled_6$cluster,extract_data_30_gwl)
Cl.timeline(data, "V1",seperated = T)
sil_fun(fcm_squared_unscaled_6,extract_data_30[,-1])
mosaic(extract_data_30_gwl,fcm_squared_unscaled_6$cluster,title = "mosaic")



# 7 cluster 
fcm_squared_unscaled_7  <- fcm(extract_data_30[,-1],7)
save(fcm_squared_unscaled_7,file = "clustering/fuzzyClustering/fcm_squared_unscaled_7.RData" )

data <- cbind(fcm_squared_unscaled_7$cluster,extract_data_30_gwl)
Cl.timeline(data, "V1",seperated = T)

sil_fun(fcm_squared_unscaled_7,extract_data_30[,-1])
mosaic(extract_data_30_gwl,fcm_squared_unscaled_7$cluster,title = "mosaic")


# 8 cluster 
fcm_squared_unscaled_8  <- fcm(extract_data_30[,-1],8)
save(fcm_squared_unscaled_8,file = "clustering/fuzzyClustering/fcm_squared_unscaled_8.RData" )

data <- cbind(fcm_squared_unscaled_8$cluster,extract_data_30_gwl)
Cl.timeline(data, "V1",seperated = T)
sil_fun(fcm_squared_unscaled_8,extract_data_30[,-1])
mosaic(extract_data_30_gwl,fcm_squared_unscaled_8$cluster,title = "mosaic")


# 9 cluster 
fcm_squared_unscaled_9  <- fcm(extract_data_30[,-1],9)
save(fcm_squared_unscaled_9,file = "clustering/fuzzyClustering/fcm_squared_unscaled_9.RData" )

data <- cbind(fcm_squared_unscaled_9$cluster,extract_data_30_gwl)
Cl.timeline(data, "V1",seperated = T)
sil_fun(fcm_squared_unscaled_9,extract_data_30[,-1])
mosaic(extract_data_30_gwl,fcm_squared_unscaled_9$cluster,title = "mosaic")




#squared scaled 

fcm_squared_scaled_6 <- fcm(extract_data_30_scaled[,-1],6) 
fcm_squared_scaled_7 <- fcm(extract_data_30_scaled[,-1],7)
fcm_squared_scaled_8 <- fcm(extract_data_30_scaled[,-1],8)
fcm_squared_scaled_9 <- fcm(extract_data_30_scaled[,-1],9) 
fcm_squared_scaled_10 <- fcm(extract_data_30_scaled[,-1],10)
fcm_squared_scaled_11 <- fcm(extract_data_30_scaled[,-1],11)
fcm_squared_scaled_12 <- fcm(extract_data_30_scaled[,-1],12)
fcm_squared_scaled_13 <- fcm(extract_data_30_scaled[,-1],13)
fcm_squared_scaled_14 <- fcm(extract_data_30_scaled[,-1],14)
fcm_squared_scaled_15 <- fcm(extract_data_30_scaled[,-1],15)
fcm_squared_scaled_list <- list(fcm_squared_scaled_6,fcm_squared_scaled_7,fcm_squared_scaled_8,fcm_squared_scaled_9,fcm_squared_scaled_10,fcm_squared_scaled_11,fcm_squared_scaled_12,fcm_squared_scaled_13,fcm_squared_scaled_14, fcm_squared_scaled_15)
name <- c("fcm6","fcm7","fcm8","fcm9","fcm10","fcm11","fcm12","fcm13","fcm14","fcm15")
names(fcm_squared_scaled_list) <- name

save(fcm_squared_scaled_list,file = "clustering/fuzzyClustering/squared_scaled_fcm_list.RData")

fcm_squared_scaled <- finding_optimal_cluster_number(fcm_squared_scaled_list,c(6,7,8,9,10,11,12,13,14,15))



test <- manova.fun(extract_data_30,fcm_scaled_6$cluster)
# eventl doch noch mit 6?

barplot(table(fcm_squared_scaled_list[[1]]$cluster), main = "number of observations in each cluster")
data <- cbind(fcm_squared_scaled_list[[1]]$cluster,extract_data_30_gwl)
Cl.timeline(data, "V1",seperated = F)
silhoette_fun(fcm_squared_scaled_list[[1]])
sil_fun(fcm_squared_scaled_list[[1]],extract_data_30[,-1])
mosaic(extract_data_30_gwl,fcm_squared_scaled_list[[1]]$cluster,title = "mosaic")



# 7 cluster

fcm_squared_scaled_7 <- fcm_squared_scaled_list[[2]]

barplot(table(fcm_squared_scaled_7$cluster), main = "number of observations in each cluster")
data <- cbind(fcm_squared_scaled_7$cluster,extract_data_30_gwl)
Cl.timeline(data, "V1",seperated = F)
silhoette_fun(fcm_squared_scaled_7)
sil_fun(fcm_squared_scaled_7,extract_data_30_scaled[,-1])
mosaic(extract_data_30_gwl,fcm_squared_scaled_7$cluster,title = "mosaic")
manova_squared_scaled_7[c(which(manova_squared_scaled_7$significance == "no")),1]
save(manova_squared_scaled_7,file = "clustering/fuzzyClustering/manova_squared_scaled_7.RData")

# 8 cluster
# eigentlich nur 5 cluster

barplot(table(fcm_squared_scaled_list[[3]]$cluster), main = "number of observations in each cluster")
data <- cbind(fcm_squared_scaled_list[[3]]$cluster,extract_data_30_gwl)
Cl.timeline(data, "V1",seperated = F)
silhoette_fun(fcm_squared_scaled_list[[3]])
sil_fun(fcm_squared_scaled_list[[3]],extract_data_30_scaled[,-1])
mosaic(extract_data_30_gwl,fcm_squared_scaled_list[[3]]$cluster,title = "mosaic")
manova_squared_scaled_8 <- manova.fun( extract_data_30_scaled,fcm_squared_scaled_list[[3]]$cluster)
manova_squared_scaled_8[c(which(manova_squared_scaled_8$significance == "no")),1]
save(manova_squared_scaled_8,file = "clustering/fuzzyClustering/manova_squared_scaled_8.RData")


# 9 cluster

barplot(table(fcm_squared_scaled_list[[4]]$cluster), main = "number of observations in each cluster")
data <- cbind(fcm_squared_scaled_list[[4]]$cluster,extract_data_30_gwl)
Cl.timeline(data, "V1",seperated = F)
silhoette_fun(fcm_squared_scaled_list[[4]])
sil_fun(fcm_squared_scaled_list[[4]],extract_data_30_scaled[,-1])
mosaic(extract_data_30_gwl,fcm_squared_scaled_list[[4]]$cluster,title = "mosaic")
manova_squared_scaled_9 <- manova.fun( extract_data_30_scaled,fcm_squared_scaled_list[[4]]$cluster)
manova_squared_scaled_9[c(which(manova_squared_scaled_9$significance == "no")),1]
save(manova_squared_scaled_9,file = "clustering/fuzzyClustering/manova_squared_scaled_9.RData")



#squared scaled weighted

fcm_squared_scaled_weighted_6 <- fcm(extract_data_30_scaled[,-1],6) 
fcm_squared_scaled_weighted_7 <- fcm(extract_data_30_scaled[,-1],7)
fcm_squared_scaled_weighted_8 <- fcm(extract_data_30_scaled[,-1],8)
fcm_squared_scaled_weighted_9 <- fcm(extract_data_30_scaled[,-1],9) 
fcm_squared_scaled_weighted_10 <- fcm(extract_data_30_scaled[,-1],10)
fcm_squared_scaled_weighted_11 <- fcm(extract_data_30_scaled[,-1],11)
fcm_squared_scaled_weighted_12 <- fcm(extract_data_30_scaled[,-1],12)
fcm_squared_scaled_weighted_13 <- fcm(extract_data_30_scaled[,-1],13)
fcm_squared_scaled_weighted_14 <- fcm(extract_data_30_scaled[,-1],14)
fcm_squared_scaled_weighted_15 <- fcm(extract_data_30_scaled[,-1],15)
fcm_squared_scaled_weighted_list <- list(fcm_squared_scaled_weighted_6,fcm_squared_scaled_weighted_7,fcm_squared_scaled_weighted_8,fcm_squared_scaled_weighted_9,fcm_squared_scaled_weighted_10,fcm_squared_scaled_weighted_11,fcm_squared_scaled_weighted_12,fcm_squared_scaled_weighted_13,fcm_squared_scaled_weighted_14 ,fcm_squared_scaled_weighted_15)
name <- c("fcm6","fcm7","fcm8","fcm9","fcm10","fcm11","fcm12","fcm13","fcm14","fcm15")
names(fcm_squared_scaled_weighted_list) <- name

save(fcm_squared_scaled_weighted_list,file = "clustering/fuzzyClustering/squared_scaled_weighted_list.RData")

fcm_squared_scaled_weighted <- finding_optimal_cluster_number(fcm_squared_scaled_weighted_list,c(6,7,8,9,10,11,12,13,14,15))



# 6 cluster 
barplot(table(fcm_squared_scaled_weighted_list[[1]]$cluster), main = "number of observations in each cluster")
data <- cbind(fcm_squared_scaled_weighted_list[[1]]$cluster,extract_data_30_gwl)
Cl.timeline(data, "V1",seperated = F)
silhoette_fun(fcm_squared_scaled_weighted_list[[1]])
sil_fun(fcm_squared_scaled_weighted_list[[1]],extract_data_30_scaled_weighted[,-1])
mosaic(extract_data_30_gwl,fcm_squared_scaled_weighted_list[[1]]$cluster,title = "mosaic")
manova_squared_scaled_weighted_6 <- manova.fun( extract_data_30_scaled,fcm_squared_scaled_weighted_list[[1]]$cluster)
manova_squared_scaled_weighted_6[c(which(manova_squared_scaled_weighted_6$significance == "no")),1]
save(manova_squared_scaled_weighted_6,file = "clustering/fuzzyClustering/manova_squared_scaled_weighted_6.RData")

# 7 cluster 
barplot(table(fcm_squared_scaled_weighted_list[[2]]$cluster), main = "number of observations in each cluster")
data <- cbind(fcm_squared_scaled_weighted_list[[2]]$cluster,extract_data_30_gwl)
Cl.timeline(data, "V1",seperated = F)
silhoette_fun(fcm_squared_scaled_weighted_list[[2]])
sil_fun(fcm_squared_scaled_weighted_list[[2]],extract_data_30_scaled_weighted[,-1])
mosaic(extract_data_30_gwl,fcm_squared_scaled_weighted_list[[2]]$cluster,title = "mosaic")
manova_squared_scaled_weighted_7 <- manova.fun( extract_data_30_scaled,fcm_squared_scaled_weighted_list[[2]]$cluster)
manova_squared_scaled_weighted_7[c(which(manova_squared_scaled_weighted_7$significance == "no")),1]
save(manova_squared_scaled_weighted_7,file = "clustering/fuzzyClustering/manova_squared_scaled_weighted_7.RData")

# cluster 9

barplot(table(fcm_squared_scaled_weighted_list[[4]]$cluster), main = "number of observations in each cluster")
data <- cbind(fcm_squared_scaled_weighted_list[[4]]$cluster,extract_data_30_gwl)
Cl.timeline(data, "V1",seperated = F)
silhoette_fun(fcm_squared_scaled_weighted_list[[4]])
sil_fun(fcm_squared_scaled_weighted_list[[4]],extract_data_30_scaled_weighted[,-1])
mosaic(extract_data_30_gwl,fcm_squared_scaled_weighted_list[[4]]$cluster,title = "mosaic")
manova_squared_scaled_weighted_9 <- manova.fun( extract_data_30_scaled,fcm_squared_scaled_weighted_list[[4]]$cluster)
manova_squared_scaled_weighted_9[c(which(manova_squared_scaled_weighted_9$significance == "no")),1]
save(manova_squared_scaled_weighted_9,file = "clustering/fuzzyClustering/manova_squared_scaled_weighted_9.RData")



# cluster 10

# 10 
barplot(table(fcm_squared_scaled_weighted_list[[5]]$cluster), main = "number of observations in each cluster")
data <- cbind(fcm_squared_scaled_weighted_list[[5]]$cluster,extract_data_30_gwl)
Cl.timeline(data, "V1",seperated = F)
silhoette_fun(fcm_squared_scaled_weighted_list[[5]])
sil_fun(fcm_squared_scaled_weighted_list[[5]],extract_data_30_scaled_weighted[,-1])
mosaic(extract_data_30_gwl,fcm_squared_scaled_weighted_list[[5]]$cluster,title = "mosaic")
manova_squared_scaled_weighted_10 <- manova.fun( extract_data_30_scaled,fcm_squared_scaled_weighted_list[[5]]$cluster)
manova_squared_scaled_weighted_10[c(which(manova_squared_scaled_weighted_10$significance == "no")),1]
save(manova_squared_scaled_weighted_10,file = "clustering/fuzzyClustering/manova_squared_scaled_weighted_10.RData")



################
#fuzzy eukidean unscaled

fcm_unscaled_6 <- fcm(extract_data_30[,-1],6, iter.max = 500,dmetric = "euclidean") 
fcm_unscaled_8 <- fcm(extract_data_30[,-1],8, iter.max = 500, dmetric = "euclidean")
fcm_unscaled_9 <- fcm(extract_data_30[,-1],9, iter.max = 500, dmetric = "euclidean") 
fcm_unscaled_10 <- fcm(extract_data_30[,-1],10, iter.max = 500, dmetric = "euclidean")
fcm_unscaled_11 <- fcm(extract_data_30[,-1],11, iter.max = 500, dmetric = "euclidean")
fcm_unscaled_15 <- fcm(extract_data_30[,-1],15, iter.max = 500, dmetric = "euclidean")
fcm_unscaled_list <- list(fcm_unscaled_6,fcm_unscaled_8,fcm_unscaled_9,fcm_unscaled_10,fcm_unscaled_11, fcm_unscaled_15)
name <- c("fcm6","fcm8","fcm9","fcm10","fcm11","fcm15")
names(fcm_unscaled_list) <- name

save(fcm_unscaled_list,file = "clustering/fuzzyClustering/unscaled_fcm_6_26_30.RData")

fcm__unscaled <- finding_optimal_cluster_number(fcm_unscaled_list,c(6,8,9,10,11,15))

# 6 cluster

fcm_unscaled_6 <- fcm(extract_data_30[,-1],6,dmetric = "euclidean") 
save(fcm_unscaled_6,file = "clustering/fuzzyClustering/fcm_unscaled_6.RData")


data <- cbind(fcm_unscaled_6$cluster,extract_data_30_gwl)
Cl.timeline(data, "V1", titleAdd = "Clustering fuzzy with cluster number 9 scaled",seperated = T)
sil_fun(fcm_unscaled_6,extract_data_30[,-1])
mosaic(extract_data_30_gwl,fcm_unscaled_6$cluster,title = "fuzzy  scaled with 9 ")


#######################
# fuzzy fcm  euklidean scaled

fcm_scaled_6 <- fcm(extract_data_30_scaled[,-1],6,dmetric = "euclidean") 
fcm_scaled_7 <- fcm(extract_data_30_scaled[,-1],7,dmetric = "euclidean") 
fcm_scaled_8 <- fcm(extract_data_30_scaled[,-1],8, dmetric = "euclidean")
fcm_scaled_9 <- fcm(extract_data_30_scaled[,-1],9, dmetric = "euclidean") 
fcm_scaled_10 <- fcm(extract_data_30_scaled[,-1],10, dmetric = "euclidean")
fcm_scaled_11 <- fcm(extract_data_30_scaled[,-1],11, dmetric = "euclidean")
fcm_scaled_12 <- fcm(extract_data_30_scaled[,-1],12,dmetric = "euclidean") 
fcm_scaled_13 <- fcm(extract_data_30_scaled[,-1],13,dmetric = "euclidean") 
fcm_scaled_14 <- fcm(extract_data_30_scaled[,-1],14,dmetric = "euclidean") 
fcm_scaled_15 <- fcm(extract_data_30_scaled[,-1],15, dmetric = "euclidean")
fcm_scaled_list <- list(fcm_scaled_6,fcm_scaled_7,fcm_scaled_8,fcm_scaled_9,fcm_scaled_10,fcm_scaled_11,fcm_scaled_12,fcm_scaled_13,fcm_scaled_14, fcm_scaled_15)
name <- c("fcm6","fcm7","fcm8","fcm9","fcm10","fcm11","fcm12","fcm13","fcm14","fcm15")
names(fcm_scaled_list) <- name
save(fcm_scaled_list,file = "clustering/fuzzyClustering/scaled_fcm_list.RData")

fcm_scaled <- finding_optimal_cluster_number(fcm_scaled_list,c(6,7,8,9,10,11,12,13,14,15))


# nur 2 oder 3 cluster, der rest ist fuzzyness

# fuzzy euklidean weighted and unscaled

fcm_unscaled_weighted_6 <- fcm(extract_data_30_weighted[,-1],6, iter.max = 500,dmetric = "euclidean") 
fcm_unscaled_weighted8 <- fcm(extract_data_30_weighted[,-1],8, iter.max = 500, dmetric = "euclidean")
fcm_unscaled_weighted9 <- fcm(extract_data_30_weighted[,-1],9, iter.max = 500, dmetric = "euclidean") 
fcm_unscaled_weighted10 <- fcm(extract_data_30_weighted[,-1],10, iter.max = 500, dmetric = "euclidean")
fcm_unscaled_weighted11 <- fcm(extract_data_30_weighted[,-1],11, iter.max = 500, dmetric = "euclidean")
fcm_unscaled_weighted15 <- fcm(extract_data_30_weighted[,-1],15, iter.max = 500, dmetric = "euclidean")
fcm_unscaled_weighted_list <- list(fcm_unscaled_weighted_6,fcm_unscaled_weighted8,fcm_unscaled_weighted9,fcm_unscaled_weighted10,fcm_unscaled_weighted11, fcm_unscaled_weighted15)
name <- c("fcm6","fcm8","fcm9","fcm10","fcm11","fcm15")
names(fcm_unscaled_weighted_list) <- name
save(fcm_unscaled_weighted_list,file = "clustering/fuzzyClustering/unscaled_weighted_fcm_6_26_30.RData")

fcm_unscaled_weighted <- finding_optimal_cluster_number(fcm_unscaled_weighted_list,c(6,8,9,10,11,15))

# Cluster 11 ( eventuell noch cluster 12,13,14 anschauen)
#Cluster 11 nur 3 cluster, rest ist noise
#Cluster 6 nur 2 cluster, rest ist noise
fcm_euclidean_scaled_weighted_8 <- fcm(extract_data_30_scaled_weighted[,-1],8,dmetric = "euclidean")
save(fcm_euclidean_scaled_weighted_8,file = "clustering/fuzzyClustering/fcm_euclidean_scaled_weighted_8.RData" )


data <- cbind(fcm_euclidean_scaled_weighted_8$cluster,extract_data_30_gwl)
Cl.timeline(data, "V1",seperated = T)
sil_fun(fcm_euclidean_scaled_weighted_8,extract_data_30_scaled_weighted[,-1])
mosaic(extract_data_30_gwl,fcm_euclidean_scaled_weighted_8$cluster,title = "mosaic")
noiseAllocation(fcm_euclidean_scaled_weighted_8$cluster,fcm_euclidean_scaled_weighted_8$u)
#manovaFUN(extract_data_30[,-1],fcm_unscaled69$cluster)



# fuzzy euklidean scaled and weighted

fcm_scaled_weighted_6 <- fcm(extract_data_30_scaled_weighted[,-1],6, iter.max = 500,dmetric = "euclidean") 
fcm_scaled_weighted8 <- fcm(extract_data_30_scaled_weighted[,-1],8, iter.max = 500, dmetric = "euclidean")
fcm_scaled_weighted9 <- fcm(extract_data_30_scaled_weighted[,-1],9, iter.max = 500, dmetric = "euclidean") 
fcm_scaled_weighted10 <- fcm(extract_data_30_scaled_weighted[,-1],10, iter.max = 500, dmetric = "euclidean")
fcm_scaled_weighted11 <- fcm(extract_data_30_scaled_weighted[,-1],11, iter.max = 500, dmetric = "euclidean")
fcm_scaled_weighted15 <- fcm(extract_data_30_scaled_weighted[,-1],15, iter.max = 500, dmetric = "euclidean")
fcm_scaled_weighted_list <- list(fcm_scaled_weighted_6,fcm_scaled_weighted8,fcm_scaled_weighted9,fcm_scaled_weighted10,fcm_scaled_weighted11, fcm_scaled_weighted15)
name <- c("fcm6","fcm8","fcm9","fcm10","fcm11","fcm15")
names(fcm_scaled_weighted_list) <- name
save(fcm_scaled_weighted_list,file = "clustering/fuzzyClustering/scaled_weighted_fcm_6_26_30.RData")

fcm_scaled_weighted <- finding_optimal_cluster_number(fcm_scaled_weighted_list,c(6,8,9,10,11,15))

# cluster 11 (eventuell 12,13,14 anschauen)

# unscaled correlation

fcm_unscaled_cor_6 <- fcm(extract_data_30[,-1],6, iter.max = 500,dmetric = "correlation") 
fcm_unscaled_8 <- fcm(extract_data_30[,-1],8, iter.max = 500, dmetric = "correlation")
fcm_unscaled_9 <- fcm(extract_data_30[,-1],9, iter.max = 500, dmetric = "correlation") 
fcm_unscaled_10 <- fcm(extract_data_30[,-1],10, iter.max = 500, dmetric = "correlation")
fcm_unscaled_11 <- fcm(extract_data_30[,-1],11, iter.max = 500, dmetric = "correlation")
fcm_unscaled_15 <- fcm(extract_data_30[,-1],15, iter.max = 500, dmetric = "correlation")
fcm_unscaled_cor_list <- list(fcm_unscaled_cor_6,fcm_unscaled_8,fcm_unscaled_9,fcm_unscaled_10,fcm_unscaled_11)
name <- c("fcm6","fcm8","fcm9","fcm10","fcm11")
names(fcm_unscaled_cor_list) <- name

save(fcm_unscaled_cor_list,file = "clustering/fuzzyClustering/unscaled_cor_fcm_6_26_30.RData")

fcm_unscaled_cor <- finding_optimal_cluster_number(fcm_unscaled_cor_list,c(6,8,9,10,11))

# cluster 6


# scaled with corrleation

fcm_scaled_cor6 <- fcm(extract_data_30_scaled[,-1],6,dmetric = "correlation") 
fcm_scaled_cor7 <- fcm(extract_data_30_scaled[,-1],7,dmetric = "correlation")
fcm_scaled_cor8 <- fcm(extract_data_30_scaled[,-1],8, dmetric = "correlation")
fcm_scaled_cor9 <- fcm(extract_data_30_scaled[,-1],9,  dmetric = "correlation") 
fcm_scaled_cor10 <- fcm(extract_data_30_scaled[,-1],10, dmetric = "correlation")
fcm_scaled_cor11 <- fcm(extract_data_30_scaled[,-1],11,  dmetric = "correlation")
fcm_scaled_cor12 <- fcm(extract_data_30_scaled[,-1],12,dmetric = "correlation")
fcm_scaled_cor13 <- fcm(extract_data_30_scaled[,-1],13,dmetric = "correlation")
fcm_scaled_cor14 <- fcm(extract_data_30_scaled[,-1],14,dmetric = "correlation")
fcm_scaled_cor15 <- fcm(extract_data_30_scaled[,-1],15,  dmetric = "correlation")
fcm_scaled_cor_list <- list(fcm_scaled_cor6,fcm_scaled_cor7,fcm_scaled_cor8,fcm_scaled_cor9,fcm_scaled_cor10,fcm_scaled_cor11,fcm_scaled_cor12,fcm_scaled_cor13,fcm_scaled_cor14, fcm_scaled_cor15)
name <- c("fcm6","fcm7","fcm8","fcm9","fcm10","fcm11","fcm12","fcm13","fcm14","fcm15")
names(fcm_scaled_cor_list) <- name

get.dmetrics(dmt = "all")

save(fcm_scaled_cor_list,file = "clustering/fuzzyClustering/scaled_cor_list.RData")

fcm_scaled_cor <- finding_optimal_cluster_number(fcm_scaled_cor_list,c(6,7,8,9,10,11,12,13,14,15))

# cluster 8 

barplot(table(fcm_scaled_cor_list[[3]]$cluster), main = "number of observations in each cluster")
data <- cbind(fcm_scaled_cor_list[[3]]$cluster,extract_data_30_gwl)
Cl.timeline(data, "V1",seperated = F)
silhoette_fun(fcm_scaled_cor_list[[3]])
sil_fun(fcm_scaled_cor_list[[3]],extract_data_30_scaled[,-1])
mosaic(extract_data_30_gwl,fcm_scaled_cor_list[[3]]$cluster,title = "mosaic")
manova_scaled_cor_8 <- manova.fun( extract_data_30_scaled,fcm_scaled_cor_list[[3]]$cluster)
manova_scaled_cor_8[c(which(manova_scaled_cor_8$significance == "no")),1]
save(manova_scaled_cor_8,file = "clustering/fuzzyClustering/manova_scaled_cor_8.RData")

# cluster 10 

barplot(table(fcm_scaled_cor_list[[5]]$cluster), main = "number of observations in each cluster")
data <- cbind(fcm_scaled_cor_list[[5]]$cluster,extract_data_30_gwl)
Cl.timeline(data, "V1",seperated = F)
silhoette_fun(fcm_scaled_cor_list[[5]])
sil_fun(fcm_scaled_cor_list[[5]],extract_data_30_scaled[,-1])
mosaic(extract_data_30_gwl,fcm_scaled_cor_list[[5]]$cluster,title = "mosaic")
manova_scaled_cor_10 <- manova.fun( extract_data_30_scaled,fcm_scaled_cor_list[[5]]$cluster)
manova_scaled_cor_10[c(which(manova_scaled_cor_10$significance == "no")),1]
save(manova_scaled_cor_10,file = "clustering/fuzzyClustering/manova_scaled_cor_10.RData")



# scaled with corrleation and weighted

fcm_scaled_weighted_cor6 <- fcm(extract_data_30_scaled_weighted[,-1],6,dmetric = "correlation") 
fcm_scaled_weighted_cor7 <- fcm(extract_data_30_scaled_weighted[,-1],7,dmetric = "correlation") 
fcm_scaled_weighted_cor8 <- fcm(extract_data_30_scaled_weighted[,-1],8, dmetric = "correlation")
fcm_scaled_weighted_cor9 <- fcm(extract_data_30_scaled_weighted[,-1],9, dmetric = "correlation") 
fcm_scaled_weighted_cor10 <- fcm(extract_data_30_scaled_weighted[,-1],10, dmetric = "correlation")
fcm_scaled_weighted_cor11 <- fcm(extract_data_30_scaled_weighted[,-1],11, dmetric = "correlation")
fcm_scaled_weighted_cor12 <- fcm(extract_data_30_scaled_weighted[,-1],12,dmetric = "correlation") 
fcm_scaled_weighted_cor13 <- fcm(extract_data_30_scaled_weighted[,-1],13,dmetric = "correlation") 
fcm_scaled_weighted_cor14 <- fcm(extract_data_30_scaled_weighted[,-1],14,dmetric = "correlation") 
fcm_scaled_weighted_cor15 <- fcm(extract_data_30_scaled_weighted[,-1],15, dmetric = "correlation")
fcm_scaled_weighted_cor_list <- list(fcm_scaled_weighted_cor6,fcm_scaled_weighted_cor7,fcm_scaled_weighted_cor8,fcm_scaled_weighted_cor9,fcm_scaled_weighted_cor10,fcm_scaled_weighted_cor11,fcm_scaled_weighted_cor12,fcm_scaled_weighted_cor13,fcm_scaled_weighted_cor14, fcm_scaled_cor15)
name <- c("fcm6","fcm7","fcm8","fcm9","fcm10","fcm11","fcm12","fcm13","fcm14","fcm15")
names(fcm_scaled_weighted_cor_list) <- name

save(fcm_scaled_weighted_cor_list,file = "clustering/fuzzyClustering/scaled_weighted_cor_list.RData")

fcm_scaled_weighted_cor <- finding_optimal_cluster_number(fcm_scaled_weighted_cor_list,c(6,7,8,9,10,11,12,13,14,15))

# Cluster 6 und 7

# cluster 6
barplot(table(fcm_scaled_weighted_cor_list[[1]]$cluster), main = "number of observations in each cluster")
data <- cbind(fcm_scaled_weighted_cor_list[[1]]$cluster,extract_data_30_gwl)
Cl.timeline(data, "V1",seperated = F)
silhoette_fun(fcm_scaled_weighted_cor_list[[1]])
sil_fun(fcm_scaled_weighted_cor_list[[1]],extract_data_30_scaled_weighted[,-1])
mosaic(extract_data_30_gwl,fcm_scaled_weighted_cor_list[[1]]$cluster,title = "mosaic")
manova_scaled_weighted_cor_6 <- manova.fun( extract_data_30_scaled,fcm_scaled_weighted_cor_list[[1]]$cluster)
manova_scaled_weighted_cor_6[c(which(manova_scaled_weighted_cor_6$significance == "no")),1]
save(manova_scaled_weighted_cor_6,file = "clustering/fuzzyClustering/manova_scaled_weighted_cor_6.RData")

# cluster 7
barplot(table(fcm_scaled_weighted_cor_list[[2]]$cluster), main = "number of observations in each cluster")
data <- cbind(fcm_scaled_weighted_cor_list[[2]]$cluster,extract_data_30_gwl)
Cl.timeline(data, "V1",seperated = F)
silhoette_fun(fcm_scaled_weighted_cor_list[[2]])
sil_fun(fcm_scaled_weighted_cor_list[[2]],extract_data_30_scaled_weighted[,-1])
mosaic(extract_data_30_gwl,fcm_scaled_weighted_cor_list[[2]]$cluster,title = "mosaic")
manova_scaled_weighted_cor_7 <- manova.fun( extract_data_30_scaled,fcm_scaled_weighted_cor_list[[2]]$cluster)
manova_scaled_weighted_cor_7[c(which(manova_scaled_weighted_cor_7$significance == "no")),1]
save(manova_scaled_weighted_cor_7,file = "clustering/fuzzyClustering/manova_scaled_weighted_cor_7.RData")


############### Manhattan

# unscaled manhattan

fcm_man_unscaled_6 <- fcm(extract_data_30[,-1],6,dmetric = "manhattan") 
fcm_man_unscaled_8 <- fcm(extract_data_30[,-1],8, iter.max = 500, dmetric = "manhattan")
fcm_man_unscaled_9 <- fcm(extract_data_30[,-1],9, iter.max = 500, dmetric = "manhattan") 
fcm_man_unscaled_10 <- fcm(extract_data_30[,-1],10, iter.max = 500, dmetric = "manhattan")
fcm_man_unscaled_11 <- fcm(extract_data_30[,-1],11, iter.max = 500, dmetric = "manhattan")
fcm_man_unscaled_15 <- fcm(extract_data_30[,-1],15, iter.max = 500, dmetric = "manhattan")
fcm_man_unscaled_list <- list(fcm_man_unscaled_6,fcm_man_unscaled_8,fcm_man_unscaled_9,fcm_man_unscaled_10,fcm_man_unscaled_11,fcm_man_unscaled_15)
name <- c("fcm6","fcm8","fcm9","fcm10","fcm11","fcm15")
names(fcm_man_unscaled_list) <- name

save(fcm_man_unscaled_list,file = "clustering/fuzzyClustering/unscaled_fcm_man_6_26_30.RData")

fcm_man_unscaled <- finding_optimal_cluster_number(fcm_man_unscaled_list,c(6,8,9,10,11,15))


# scaled manhattan
barplot(table(fcm_man_scaled_8$cluster))
fcm_man_scaled_6 <- fcm(extract_data_30_scaled[,-1],6,dmetric = "manhattan") 
fcm_man_scaled_7 <- fcm(extract_data_30_scaled[,-1],7,dmetric = "manhattan") 
fcm_man_scaled_8 <- fcm(extract_data_30_scaled[,-1],8, dmetric = "manhattan")
fcm_man_scaled_9 <- fcm(extract_data_30_scaled[,-1],9, dmetric = "manhattan") 
fcm_man_scaled_10 <- fcm(extract_data_30_scaled[,-1],10,  dmetric = "manhattan")
fcm_man_scaled_11 <- fcm(extract_data_30_scaled[,-1],11,  dmetric = "manhattan")
fcm_man_scaled_12 <- fcm(extract_data_30_scaled[,-1],12,dmetric = "manhattan") 
fcm_man_scaled_13 <- fcm(extract_data_30_scaled[,-1],13,dmetric = "manhattan") 
fcm_man_scaled_14 <- fcm(extract_data_30_scaled[,-1],14,dmetric = "manhattan") 
fcm_man_scaled_15 <- fcm(extract_data_30_scaled[,-1],15,  dmetric = "manhattan")
fcm_man_scaled_list <- list(fcm_man_scaled_6,fcm_man_scaled_7,fcm_man_scaled_8,fcm_man_scaled_9,fcm_man_scaled_10,fcm_man_scaled_11,fcm_man_scaled_12,fcm_man_scaled_13,fcm_man_scaled_14,fcm_man_scaled_15)
name <- c("fcm6","fcm7","fcm8","fcm9","fcm10","fcm11","fcm12","fcm13","fcm14","fcm15")
names(fcm_man_scaled_list) <- name

save(fcm_man_scaled_list,file = "clustering/fuzzyClustering/fcm_man_scaled_list.RData")

fcm_man_scaled <- finding_optimal_cluster_number(fcm_man_scaled_list,c(6,7,8,9,10,11,12,13,14,15))



# scaled weighted mahattan

fcm_man_scaled_weighted_6 <- fcm(extract_data_30_scaled_weighted[,-1],6,dmetric = "manhattan") 
fcm_man_scaled_weighted_7 <- fcm(extract_data_30_scaled_weighted[,-1],7,dmetric = "manhattan") 
fcm_man_scaled_weighted_8 <- fcm(extract_data_30_scaled_weighted[,-1],8, dmetric = "manhattan")
fcm_man_scaled_weighted_9 <- fcm(extract_data_30_scaled_weighted[,-1],9,  dmetric = "manhattan") 
fcm_man_scaled_weighted_10 <- fcm(extract_data_30_scaled_weighted[,-1],10, dmetric = "manhattan")
fcm_man_scaled_weighted_11 <- fcm(extract_data_30_scaled_weighted[,-1],11, dmetric = "manhattan")
fcm_man_scaled_weighted_12 <- fcm(extract_data_30_scaled_weighted[,-1],12,dmetric = "manhattan") 
fcm_man_scaled_weighted_13 <- fcm(extract_data_30_scaled_weighted[,-1],13,dmetric = "manhattan") 
fcm_man_scaled_weighted_14 <- fcm(extract_data_30_scaled_weighted[,-1],14,dmetric = "manhattan") 
fcm_man_scaled_weighted_15 <- fcm(extract_data_30_scaled_weighted[,-1],15,  dmetric = "manhattan")
fcm_man_scaled_weighted_list <- list(fcm_man_scaled_weighted_6,fcm_man_scaled_weighted_7,fcm_man_scaled_weighted_8,fcm_man_scaled_weighted_9,fcm_man_scaled_weighted_10,fcm_man_scaled_weighted_11,fcm_man_scaled_weighted12,fcm_man_scaled_weighted_13,fcm_man_scaled_weighted_14,fcm_man_scaled_weighted_15)
name <- c("fcm6","fcm7","fcm8","fcm9","fcm10","fcm11","fcm12","fcm13","fcm14","fcm15")
names(fcm_man_scaled_weighted_list) <- name

save(fcm_man_scaled_weighted_list,file = "clustering/fuzzyClustering/fcm_man_scaled_weighted_list.RData")

fcm_man_scaled_weighted <- finding_optimal_cluster_number(fcm_scaled_weighted_list,c(6,7,8,9,10,11,12,13,14,15))


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


#########################################
# fuzzy with FKM 

FKM_unscaled5 <- FKM(extract_data_30,5,seed = 123)
FKM_unscaled6 <- FKM(extract_data_30,6,seed = 123)
FKM_unscaled8 <- FKM(extract_data_30,8,seed = 123)
FKM_unscaled9 <- FKM(extract_data_30,9,seed = 123)
FKM_unscaled10 <- FKM(extract_data_30,10,seed = 123)
FKM_unscaled11 <- FKM(extract_data_30,11,seed = 123)
FKM_unscaled15 <- FKM(extract_data_30,15,seed = 123)



# kleiner exkurs zu silhoette

# umwandeln zu output object von ppclust
fcm_unscaled6Fclust <- ppclust2(fcm_unscaled_6,"fclust")

# aus fclust: Berechnung des  fuzzy silhoettenindex
#output: eine Zahl
silf <- SIL.F(fcm_unscaled6Fclust$Xca,fcm_unscaled6Fclust$U)

# aus fclust: Berechnung des silhouetten index
#output: List mit 
#vector für siljhouettenindex für jeden Wert 
# Siljoeuttenwert ( mean von silhouetten index)
# silhoettenwert ist unterchiedlich zu fuzzy silhouettenwert
sil <- SIL(fcm_unscaled6Fclust$Xca,fcm_unscaled6Fclust$U)

#  gibt ein Objekt der Klasse silhoette zurück, der dann 
#geplottet werden kann ()
# zählt für jedn wert die clusterzugehörigkeit, das nächste cluster und 
# die silhouettenweite auf
sil1 <- silhouette(fcm_unscaled_6$cluster,dist(extract_data_30[,-1]))
plot(sil1, col = c("red","blue","green","pink"))
fviz_silhouette(sil$sil.obj)
summary(sil1)
