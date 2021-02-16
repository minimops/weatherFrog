#MANOVA

# data: data frame/data table, that are used for clustering, for example our extract_varaibles_data
#       only contains the variable "date" and variables, that have been clustered
# cluster_vector: cluster vector of the cluster solution, usually cluster_object$cluster or something like that
# data_variable: column name of the date variable, default is "date"

manova.fun <- function(data,cluster_vector,date_variable = date){
  
  cluster <- as.factor(cluster_vector)
  data_for_manova <- cbind(cluster,data)
  data_for_manova <- subset(data_for_manova, select = -date)
  
  # maova
  man <- manova(as.matrix(data_for_manova[,-1]) ~ data_for_manova$cluster)
  print(man)
  #print(summary(man, tol = 0,fit = "Wilks"))
  man.aov <- summary.aov(man)
  
  
  # gather the output og summary.aov in one data.frame
  res_mat <- as.data.frame(matrix(ncol = 5))
  res_mat_residuals <- as.data.frame(matrix(ncol = 5))
  for ( i in seq_len(ncol(data_for_manova)-1)){
    res_mat[i,] <- man.aov[[i]][1,]
    res_mat_residuals[i,] <- man.aov[[i]][2,]
  }
  colnames(res_mat) <- colnames(man.aov[[1]])
  res_mat_residuals <- res_mat_residuals[,-c(4,5)]
  colnames(res_mat_residuals) <- c("residuals_df","residuals_sum_sq","residuals_mean_sq")
  rownames(res_mat) <- seq(1,ncol(data_for_manova)-1,by = 1)
  rownames(res_mat_residuals) <- seq(1,ncol(data_for_manova)-1,by = 1)
  variable <- colnames(data_for_manova[,-1])
  res_mat <- cbind(variable,res_mat,res_mat_residuals)
  
  # creating a colum, that shows, if a single variable contributes to the cluster solution/is significant
  # if p < alpha = 0.05 : yes, significant if p > alpha: no / not significant
  
  res_mat$significance <- "NA"
  for(i in seq_len(nrow(res_mat))) {
    if(res_mat[i,6] < 0.05){
      res_mat[i,10] = "yes"
    }
    else{
      res_mat[i,10] = "no"
    }
  }
  
  return(res_mat)
}