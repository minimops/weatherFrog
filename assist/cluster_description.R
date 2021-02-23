# pam example in order to work with this output 

library(cluster)


source("clustering/Var_Extraction_Method/f_extr_funs.R")
source("clustering/ClusterAssesmentHelper.R")

# choose 30 years
extract_data_30 <- extrapolate(seq(1971, 2000))
extract_data_30_scaled <- scale(extract_data_30[,-1])

pam_trial <- pam(extract_data_30_scaled, 6,metric = "manhattan")

cluster <- pam_trial$clustering
data_cluster <- as.data.table(cbind(cluster,extract_data_30_scaled))

table(data_cluster$cluster)

pam_trial$clusinfo

# cluster description function from package clusterSim
# output is an  3 dimensional array with following dimension:

#1.Dimension: Cluster
#2.Dimension: input variablen: mean geopot, median geopot usw
#3.Dimension: 1: mean, 2.standardabweichung, 3. median, 4. median absolute deviation, 5.mode


library(clusterSim)
clusterDescription <- cluster.Description(extract_data_30[,-1],pam_trial$clustering)
dim(clusterDescription)
labels(clusterDescription)

#naming array dimensions

firstDimNames <- c("cluster1", "cluster2","cluster3","cluster4","cluster5","cluster6")
secondDimNames <- colnames(extract_data_30_scaled)
thirdDimNames <- c("mean","SD","median","median_absolute_deviation","mode")

dimnames(clusterDescription) <- list(firstDimNames,secondDimNames,thirdDimNames)

# mode only applicable for ordinal and nominal data, delete mode from array
clusterDescription <- clusterDescription[,,-5]
dim(cluster)
first_cluster <- clusterDescription[1,,]


# Plot for number of observations in each cluster

cluster <- pam_trial$clustering
data_cluster <- as.data.table(cbind(cluster,extract_data_30))
(cluster_number <- as.data.table(table(pam_trial$clustering)))

# would be nice to give one cluster a specified color, that we use in our presentation in each plot
col_vector <- c("darkblue", "darkgreen", "lightgreen","red","hotpink","orange")

ggplot(cluster_number) +
  geom_bar( aes(x = V1, y=N),stat = "identity", fill=col_vector, alpha=0.7, width=0.5) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("NUmber of observations in each cluster") +
  xlab("cluster") +
  ylab("number of observations")



# visualize mean.mlsp in every cluster

library(tidyverse)
library(hrbrthemes)
library(viridis)
library(patchwork)

mean_mslp <- as.data.table(clusterDescription[,1,])
mean_mslp <- as.data.table(sapply(mean_mslp,as.numeric))
cluster <- as.factor(c(1,2,3,4,5,6))
mean_mslp <- cbind(cluster,mean_mslp)



ggplot(mean_mslp) +
  geom_bar( aes(x = cluster, y=mean), stat="identity", fill=col_vector, alpha=0.7, width=0.5) +
  geom_errorbar( aes(x = cluster, ymin = mean - SD, ymax = mean + SD), width=0.4, colour="black", alpha=0.9, size=1) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("mean mslp in every cluster") +
  xlab("cluster number")

# it woulf be insane plotting all 49 variables...
# instead of plotting them, it would be nicer just to plot the "original"
# geopotential and mlsp and plot mean with SD, distrinution in every cluster
# visualize every cluster solution on our map? 


first_cluster <- data_cluster[data_cluster$cluster == 1,]
plot(first_cluster$min.mslp)
