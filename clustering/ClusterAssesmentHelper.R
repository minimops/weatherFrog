
library(checkmate)
library(data.table)

#function to attach gwl to a dataset with date column
attachGwl <- function(data) {
  assertDataTable(data)
  assertSubset("date", names(data))
  
  gwls <- readRDS("Data/gwl.rds")
  
  merge(data, gwls)
}

#function that prints table of occurence freq of lengths and plots them
#data input has to be a dt with date and id column
#cluster input is used to identify the id column
#title add input can be used to add smth to the plot title
Cl.timeline <- function(data, cluster = "cluster", titleAdd = "") {
  assertDataTable(data)
  assertString(cluster)
  assertString(titleAdd)
  assertSubset(c("date", cluster), names(data))
  
  #this is next level stupid,i cant figure out a different way to extract the
  #cluster column while leaving it a variable
  use <- data.table(gwl = copy(data)[[as.character(cluster)]],
               date = copy(data)[["date"]])
  setorder(use, date)
  
  runLengths <- rle(use[[as.character(cluster)]])
  
  print("distribution of runLengths:")
  print(table(runLengths$lengths))
  
  ggplot(data = as.data.frame(table(runLengths$lengths)), 
         aes(x= Var1, y = Freq)) +
    geom_col() +
    labs(x = "Length", 
         title = paste("Occurence frequencies of lengths", titleAdd))
}





silhouette <- function(cluster.fitted, cluster.vector, distance, algorithm) {
  assertInteger(cluster.vector)
  # assertString(metric)
  assertString(algorithm)
  # assertSubset(metric, choices = c("euclidean", "manhatten", "gower", "mahalanobis"))
  assertSubset(algorithm, choices = c("pam", "kmeans", "fuzzy", "distribution"))
  
  if (algorithm == "fuzzy") {
    fviz_silhouette(cluster.fitted)
  }
  sil <- silhouette(x = cluster.vector, dist = distance)
  print(mean(sil[, 3]))
  fviz_silhouette(sil.obj = sil)
  
}
silhouette(pam_fit, pam_fit$clustering, dissimilarity, "pam")

