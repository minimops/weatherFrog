#### Clustering with distribiution clustering

#TODO
#this package seems to only work on R 4.0.3
#and i cant figure out how to get that version on ubuntu
library(ClusterR)


#get extracted dataset
source("clustering/Var_Extraction_Method/f_extr_funs.R")
extr.Data.05 <- extrapolate(seq(2006, 2010))
extr.Data.05.noDate <- as.data.frame(extr.Data.05)
extr.Data.05.noDate$date <- NULL
extr.Data.05.noDate <- center_scale(extr.Data.05.noDate, mean_center = TRUE,
                                    sd_scale = TRUE)

#get optimal number of clusters
opt_gmm <- Optimal_Clusters_GMM(extr.Data.05.noDate, max_clusters = 15, 
                                criterion = "BIC", dist_mode = "maha_dist",
                                seed_mode = "random_subset",
                                km_iter = 10, em_iter = 10, var_floor = 1e-10,
                                plot_data = TRUE)


#lets take 7 for now

gmm <- GMM(extr.Data.05.noDate, 7, dist_mode = "maha_dist",
           seed_mode = "random_subset", km_iter = 10, em_iter = 10,
           verbose = FALSE)

predictClusters <- predict_GMM(extr.Data.05.noDate, gmm$centroids, gmm$covariance_matrices,
                               gmm$weights)

predictClusters$cluster_labels
