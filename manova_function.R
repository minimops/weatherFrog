# evaluates, how much each variable contributes to the cluster solution and how 
# good the cluster solution is; is the variance within a cluster smaller than 
# the variance between?

# p value < alpha: variance between the cluster > variance within a cluster


# input variables: 
# cluster_vector:  A vector of integers (from 1:k) indicating the cluster to which each point is allocated.
# data: our data table with the extracted variables that we are clustering with
#library(data.table)
#wine_subset <- as.data.table(wine_subset)
#data <- wine_subset
#cluster_vector <- cluster

manovaFUN(wine_subset,cluster)

manovaFUN <- function(data,cluster_vector){
data <- cbind(cluster_vector,data)
data <- as.data.frame(data)
print(colnames(data))

model <- manova(as.matrix(data[,-1]) ~ data$cluster_vector)
print(model)

sum_model <- summary(model, test = "Wilks")
print(sum_model)

aov_model <- summary.aov(model)
print(aov_model)


# Boxplots der einzelnen Einflussvariablen aufgeteilt nach Cluster

for ( i in seq_len(ncol(data)) - 1){
  boxplot(data[, i + 1] ~ data$cluster_vector, ylab = colnames(data[i + 1]))
  
}
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

# was man auch noch machen könnte:
# ANOVA, wo dann z. B. in die Bewertung nur die Zentren eines Clusters mit 
# Streuung eingehen 

#Frage: was wäre besser/genauer? ANOVA mit Zentren der Cluster oder MANOVA,
# wo dann die Mittelwerte aller extract variable je Cluster eingehen?

#BSP wine cluster




