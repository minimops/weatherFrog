# evaluates, how much each variable contributes to the cluster solution and how 
# good the cluster solution is; is the variance within a cluster smaller than 
# the variance between?

# p value < alpha: variance between the cluster > variance within a cluster


# input variables: 
# cluster_vector:  A vector of integers (from 1:k) indicating the cluster to which each point is allocated.
# data: our data table with the extracted variables that we are clustering with
library(data.table)
wine_subset <- as.data.table(wine_subset)
#data <- wine_subset
#cluster_vector <- cluster

manovaFUN(wine_subset,cluster)

manovaFUN <- function(data,cluster_vector){
data <- data.frame(cbind(cluster_vector,data))
#print(colnames(data))

vars <- as.matrix(data[, !(names(data) %in% c("date", "cluster_vector"))])

model <- manova(vars ~ cluster_vector, data = data)
print(model)
# sum_model <- summary(model, test = "Wilks")
# print(sum_model)
aov_model <- summary.aov(model)
print(aov_model)


# Boxplots der einzelnen Einflussvariablen aufgeteilt nach Cluster

for ( i in seq_len(ncol(data)) - 1){
  boxplot(data[, i + 1] ~ data$cluster_vector, ylab = colnames(data[i + 1]))
  
}
}







