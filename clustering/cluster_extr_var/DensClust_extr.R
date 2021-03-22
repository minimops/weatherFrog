#testing density based clustering on extracted dataset

library(dbscan)
library(KneeArrower)

source("clustering/cluster_extr_var/f_extr_funs.R")
source("clustering/ClusterAssesmentHelper.R")

resdata <- scaleNweight(extrapolate(c(2006, 2010)))

usedata <- as.matrix(copy(resdata)[, date := NULL])


kNNdistplot(usedata, k = 20)
abline(h=7, col = "red", lty =  2)

y <- kNNdist(usedata, k = 20)
x <- seq(from = 1, to = nrow(usedata), by = 1)
(a <- findCutoff(x,y, method = "curvature")$y)
(b <- findCutoff(x,y, method = "first")$y)

#TODO gewichtung?
result <- dbscan(usedata, eps = 4.8, minPts = 20)
result
