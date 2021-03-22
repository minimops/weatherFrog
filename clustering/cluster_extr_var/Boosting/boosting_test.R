#Boosting Test

source("clustering/cluster_extr_var/Boosting/clusterBoosting.R")
f_data <- readRDS("finalDATA/f_data.rds")

sampling <- c(3, 5)

system.time({
test <- clusterBoosting(0.1, 2, f_data, sampling, start.weights = rep(0.1, 48))
saveRDS(test, "Data/boosting/test.rds")
})

run1 <- clusterBoosting(0.1, 100, f_data, sampling, start.weights = rep(0.1, 48))
saveRDS(run1, "Data/boosting/run1.rds")


run2 <- clusterBoosting(0.1, 100, f_data, sampling, start.weights = rep(0.05, 48))
saveRDS(run2, "Data/boosting/run2.rds")


run3 <- clusterBoosting(0.05, 100, f_data, sampling, start.weights = rep(0.1, 48))
saveRDS(run3, "Data/boosting/run3.rds")


run4 <- clusterBoosting(0.1, 100, f_data, sampling, start.weights = rep(0.1, 48))
saveRDS(run4, "Data/boosting/run4.rds")


run5 <- clusterBoosting(0.1, 100, f_data, sampling, start.weights = rep(0.05, 48))
saveRDS(run5, "Data/boosting/run5.rds")


run6 <- clusterBoosting(0.05, 100, f_data, sampling, start.weights = rep(0.1, 48))
saveRDS(run6, "Data/boosting/run6.rds")

