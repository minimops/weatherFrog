#manova trial
library(data.table)
library(cluster)

source("clustering/cluster_extr_var/f_extr_funs.R")
source("clustering/manova_function.R")
source("clustering/ClusterAssesmentHelper.R")

data <- extrapolate(seq(1971, 2000), "all.pca")

dissimilarity <- parallelDist(as.matrix(scaleNweight(copy(data))[, date := NULL]), method = "manhattan",
                              threads = detectCores() - 2)

pam1 <- pam(dissimilarity, 6, diss = TRUE)

sil(pam1, pam1$clustering, dissimilarity, "pam")

man1 <- manova.fun(data, pam1$clustering)


