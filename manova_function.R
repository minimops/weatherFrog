# evaluates, how much each variable contributes to the cluster solution and how 
# good the cluster solution is; is the variance within a cluster smaller than 
# the variance between?

# p value < alpha: variance between the cluster > variance within a cluster


# input variables: 
# cluster_vector:  A vector of integers (from 1:k) indicating the cluster to which each point is allocated.
# data: our data table with the extracted variables that we are clustering with

manova <- function(data,cluster_vector){
data <- cbind(cluster,data)
data <- as.data.frame(data)
colnames(data)[colnames(data) == "V1"] <- "cluster_group"

# Boxplots der einzelnen Einflussvariablen aufgeteilt nach Cluster

for ( i in seq_len(ncol(data)) - 1){
  boxplot(data[, i + 1] ~ data$cluster_group, ylab = colnames(data[i + 1]))
  
}

# MANOVA: is the variance within smaller than the variance between?
# manova(v1,v2 ~ vi):
#v1 and v2 are the dependent variables = Zielvariable, contains measure values 
#vi the independent variable = Einflussvariable: splited into facors/groups

model1 <- manova(as.matrix(data[,-1]) ~ data$cluster)
summary(model1, test = "Wilks")

#how much contributes every variable to the cluster solution?
summary.aov(model1)



}



