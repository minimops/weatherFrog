
library(checkmate)
library(data.table)
library(ggplot2)

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
  use <- data.table(ClustID = copy(data)[[as.character(cluster)]],
               date = copy(data)[["date"]])
  setorder(use, date)
  
  runLengths <- rle(use[["ClustID"]])
  
  print("distribution of runLengths:")
  print(table(runLengths$lengths))
  
  ggplot(data = as.data.frame(table(runLengths$lengths)), 
         aes(x= Var1, y = Freq)) +
    geom_col() +
    labs(x = "Length", 
         title = paste("Occurence frequencies of lengths", titleAdd))
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
  assertInteger(cluster.vector)
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
sil(pam_fit, pam_fit$clustering, dissimilarity, "pam")



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
  assertNumeric(weights, len = ncol(data) - 1)
  
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
