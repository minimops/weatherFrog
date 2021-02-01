
library(checkmate)
library(data.table)
library(ggplot2)
library(cluster)
library(factoextra)

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
Cl.timeline <- function(data, cluster = "cluster", titleAdd = "", seperated = FALSE) {
  assertDataTable(data)
  assertString(cluster)
  assertString(titleAdd)
  assertSubset(c("date", cluster), names(data))
  assertLogical(seperated)
  
  #this is next level stupid,i cant figure out a different way to extract the
  #cluster column while leaving it a variable
  use <- data.table(ClustID = copy(data)[[as.character(cluster)]],
               date = copy(data)[["date"]])
  setorder(use, date)
  print("Table of Cluster frequencies:")
  print(table(use$ClustID))
  if(seperated){
           runLengths <- rle(use[["ClustID"]])
           
           for (i in unique(use[["ClustID"]])) {
             
             length.runLengths.part <- runLengths$lengths[which(runLengths$values == i)]
             print(paste("distribution of runLengths", "Cluster:", i))
             print(table(length.runLengths.part))
             
             print(ggplot(data = as.data.frame(table(length.runLengths.part)), 
                    aes(x= length.runLengths.part, y = Freq)) +
               geom_col() +
               labs(x = "Length", 
                    title = paste("Occurence frequencies of lengths", 
                                  paste(titleAdd, "Cluster:", i))))
           }
         } else{
          runLengths <- rle(use[["ClustID"]])
          
          print("distribution of runLengths:")
          print(table(runLengths$lengths))
          
          ggplot(data = as.data.frame(table(runLengths$lengths)), 
                 aes(x= Var1, y = Freq)) +
            geom_col() +
            labs(x = "Length", 
                 title = paste("Occurence frequencies of lengths", titleAdd))
         }
}


# this function is to get the silhouette coefficient. 
# INPUT: - cluster.fittet: Result of a clustering
#        - cluster.vector: the clustering vector of the fittet cluster, normally it is either
#                          cluster.fittet$cluster or cluster.fittet$clustering
#        - distance: an object of class "dist", so for example with dist(...) or daisy(...)
#        - algorithm: method you have chosen. Fuzzy is a bit different and I dont know what the density based
#                     clustering will be like

# OUTPUT: exact mean value of silhouette width and output of fviz_silhouette which is the silhouette width 
#         and a plot


sil <- function(cluster.fitted, cluster.vector, distance, algorithm) {
  assertNumeric(cluster.vector)
  assertString(algorithm)
  assertSubset(algorithm, choices = c("pam", "kmeans", "fuzzy", "distribution"))
  
  if (algorithm == "fuzzy") {
    fviz_silhouette(cluster.fitted)
  }
  sil <- silhouette(x = cluster.vector, dist = distance)
  print(mean(sil[, 3]))
  fviz_silhouette(sil.obj = sil)
  
}
# an example:
#sil(pam_fit, pam_fit$clustering, dissimilarity, "pam")



# function to scale and weight the data.
# weighting is a bit difficult because normally it should be done in a clustering or dissimilarity function
# and not by multiplying the weight and the variable.
# INPUT: - data in format of extrapolate(seq(x-y)), or with selected variables
#        - weight: logical whether oe wants the output to be weighted or not
#        - weights: the weights for the variables, right now for data input of extrapolate(seq(x, y), vars = "all)
#                   if the input data has less variables, the weights vector must be adjusted
#                   date is not included in weighting, so length(weights) == ncol(data)-1

# OUTPUT: a data table with same dimensions as input data, either scaled and weighted or just scaled

scaleNweight <- function(data, weight = FALSE, weights = c(rep(c(1/9, 1/9, 1/6, 1/6, 1/18, 1/18, 1/9, 1/9, 1/9), 2), 
                                                           rep(1/6, 12), rep(1/18, 18))) {
  assertDataTable(data)
  assertSubset("date", colnames(data)[1])
  assertLogical(weight)
  assertNumeric(weights, null.ok = TRUE)
  
  date <- data[, .(date)]
  cols <- colnames(data)[2:ncol(data)]
  
  data.scaled <- copy(data)[, (cols) := lapply(.SD, scale), .SDcols = cols]
  
  if (!weight) {
    return(data.scaled)
  }  
  
  data.weighted <- as.data.frame(matrix(0, ncol = ncol(data)-1, nrow = nrow(data)))
  for (i in 1:(ncol(data) - 1)) {
    data.weighted[, i] <- data.scaled[, .SD, .SDcols = cols[i]] * weights[i]
  }
  
  data.weighted <- data.table(date, data.weighted)
  colnames(data.weighted) <- c("date", cols)
  data.weighted
}


# function that print mosaicplots
# INPUT: - data with date 
#        - clustering vector of clusters
#        - title of plots, input is the used method

mosaic <- function(data, cluster_vector, title = "PAM") {
  assertDataTable(data)
  assertInteger(cluster_vector)
  assertString(title)
  assertSubset("date", colnames(data))
  
  gwl <- readRDS("Data/gwl.rds")
  data.gwl <- gwl[data, on = .(date)]
  data.gwl.cluster <- data.gwl[, cluster := cluster_vector]
  
  mosaicplot(table(data.gwl.cluster$cluster, data.gwl.cluster$gwl), color = TRUE,
             xlab = "Cluster", ylab = "GWL", cex.axis = 0.6, las = 2,
             main = paste0(title, " Cluster - GWL"))
  mosaicplot(table(data.gwl.cluster$gwl, data.gwl.cluster$cluster), color = TRUE,
             ylab = "Cluster", xlab = "GWL", cex.axis = 0.6, las = 2,
             main = paste0(title, " Cluster - GWL"))
}


#function that classifies noise for soft clustering methods
#observations with to low probability or ultiple high probabilities
#are classified as noise
#inputs are the clusterid vector of a cluster result 
#and a cluster probability matrix
#outputs new clusterid vector
noiseAllocation <- function(cluster.id, cluster.prob) {
  assertMatrix(cluster.prob)
  assertNumeric(cluster.id, len = nrow(cluster.prob))
  
  #rows where no probability is greater than 35%
  low.Prob.rows <- which(!apply(cluster.prob, 1, function(r) any(r > 0.35)))
  #rows where probility greater than 50percent to more than one cluster
  mult.high.Prob.rows <- which(apply(cluster.prob, 1, function(r) sum(r > 0.5)) > 1)
  
  #give these observations the cluster id 99 
  #TODO maybe NA instead of 99?
  cluster.id[unique(c(low.Prob.rows, mult.high.Prob.rows))] <- 99
  
  cluster.id
}


# MANOVA function
# 
# evaluates, how much each variable contributes to the cluster solution and how 
# good the cluster solution is; is the variance within a cluster smaller than 
# the variance between?

# p value < alpha: variance between the cluster > variance within a cluster


# input variables: 
# cluster_vector:  A vector of integers (from 1:k) indicating the cluster to which each point is allocated.
# data: our data table with the extracted variables that we are clustering with


manovaFUN <- function(data,cluster_vector){
  data <- cbind(cluster_vector,data)
  data <- as.data.frame(data)
  print(colnames(data))
  
  model <- manova(as.matrix(data[,-1]) ~ data$cluster_vector)
  print(model)
  
  sum_model <- summary(model,test = "Wilks")
  print(sum_model)
  
  aov_model <- summary.aov(model)
  print(aov_model)


}
