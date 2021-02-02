library(ppclust)
library(factoextra)
library(cluster)
library(fclust)


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


# silhouette ( silhouetten function did not work, i have to convert the fuzzy output
# to another output with ppclust 2 in order to plot a silhouette)
sil_fun <- function(cluster_output, data){
  a <- ppclust2(cluster_output,"kmeans")
  sil <- silhouette(a$cluster, dist(data))
  fviz_silhouette(sil)
  
  #b <- silhouette(cluster_output$cluster,dist(data))
  #plot(b)
}



# Function for selecting the optimal cluster number in fuzzy clustering

#Anmerkung: in dieser function wurde validation function verwendet:
# wenn ich weis, wo ich validation fkt. abspeichere, kann ich es mit require
#hier aufrufen

# begin : start value for cluster number
# end: end value for cluster number
# x: extracted data set
# type: gk for gustavon kessel, fcm for fuzzy k means with eucledean
# sclae: data will be scaled

best_cluster_number <- function(begin,end,x,scale = FALSE){
  
  #Defining some objects
  vec <- seq(from = begin, to = end,by = 1)
  name <- vector()
  cluster_object <- list()
  measure_mat <- as.data.frame(matrix(ncol = 4))
  colnames(measure_mat) <- c("fuzzy_silhouette","partition_entropy","partition_coef","modified_part_coef")
  
  
  # calculation of gustavon-kessel-cluster solution with different cluster numbers
  

   for(i in vec){
      name[i] <- paste0("FKM",i)
      cluster_object[[i]] <- gk(x[,-1], centers = i,stand = scale)
    }
    

  
  names(cluster_object) <- name
  cluster_object <- cluster_object[lengths(cluster_object) > 0]
  
  #calculation of silhouette,partition entropy etc.
  
  validation <- lapply(cluster_object, validation_variables)
  names(validation) <- names(cluster_object)
  validation_mat <- as.data.frame(matrix(ncol = 4))
  colnames(validation_mat) <- c("fuzzy_silhouette","partition_entropy","partition_coef","modified_part_coef")
  
  for(i in 1 : length(validation)){
    validation_mat[i,] <- validation[[i]]
  }
  
  cluster <- na.omit(name)
  validation_mat <- cbind(cluster,validation_mat)
  
  # finding minimum or maximum for best cluster number (rownumber)
  rownumber <- vector()
  
  rownumber <- which.max(validation_mat[,2])
  rownumber <- c(rownumber,which.min(validation_mat[,3]))
  rownumber <- c(rownumber,which.max(validation_mat[,4]))
  rownumber <- c(rownumber,which.max(validation_mat[,5]))
  
  print (paste("Best cluster solution in row",rownumber))
  
  # visualsation of the for validation measures
  
  
  plot(validation_mat$fuzzy_silhouette,  type = "b", main = "fuzzy silhouette")
  plot(validation_mat$partition_entropy,  type = "b", main = "partition_entropy")
  plot(validation_mat[,4], type = "b", main = "partition coefficient")
  plot( validation_mat[,5],  type = "b",main = "modified partition coefficient")
  
  
  
  cluster_object[[length(cluster_object) + 1]] <- validation_mat
  return(cluster_object)
  
}


