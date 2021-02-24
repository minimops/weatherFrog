
# function for calculating mean, SD, median for every variable in every cluster


# cluster description function from package clusterSim
# output is an  3 dimensional array with following dimension:

#1.Dimension: Cluster
#2.Dimension: input variablen: mean geopot, median geopot usw
#3.Dimension: 1: mean, 2.standardabweichung, 3. median, 4. median absolute deviation, 5.mode

# Cave: output only contains character variables instead of metric

# input: # our data dable containing our aextracted variable/data, that have been clustered
#          WITHOUT date variable
         # vecor containing the cluster numbers
         # k = number of cluster, default = 6
      
descriptive_array <- function(cluster_data,clustering_vector, k = 6){

require(clusterSim)
clusterDescription <- cluster.Description(cluster_data,clustering_vector)
dim(clusterDescription)
labels(clusterDescription)

#naming array dimensions

firstDimNames <- paste0("cluster",seq(from = 1, to = k, by = 1))
secondDimNames <- colnames(cluster_data)
thirdDimNames <- c("mean","SD","median","median_absolute_deviation","mode")

dimnames(clusterDescription) <- list(firstDimNames,secondDimNames,thirdDimNames)

# mode only applicable for ordinal and nominal data, delete mode from array
clusterDescription <- clusterDescription[,,-5]
dim(clusterDescription)
}