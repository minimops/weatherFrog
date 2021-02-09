#MANOVA

# data: data frame/data table, that are used for clustering, for example our extract_varaibles_data
#       only contains the variable "date" and variables, that have been clustered
# cluster_vector: cluster vector of the cluster solution, usually cluster_object$cluster or something like that
# data_variable: column name of the date variable, default is "date"

anova.fun <- function(data,cluster_vector,date_variable = date){
  
  cluster <- cluster_vector
  data_for_anova <- cbind(cluster,data)
  data_for_anova <- subset(data_for_anova, select = -date)
  
  # aova
  anova_solution <- aov(cluster ~., data = data_for_anova )
  anova_mat <- summary(anova_solution)[[1]]
  anova_mat <- na.omit(anova_mat)
  

  
 
  # creating a colum, that shows, if a single variable contributes to the cluster solution/is significant
  # if p < alpha = 0.05 : yes, significant if p > alpha: no / not significant
  
  anova_mat$significance <- "NA"
  for(i in seq_len(nrow(anova_mat))) {
    if(anova_mat[i,5] < 0.05){
      anova_mat[i,6] = "yes"
    }
    else{
      anova_mat[i,6] = "no"
    }
  }
  
  return(anova_mat)
}
