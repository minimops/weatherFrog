#final Pam clustering with extracted Variables
library(cluster)
library(data.table)
library(parallel)
library(parallelDist)

source("clustering/cluster_extr_var/f_extr_funs.R")
source("clustering/ClusterAssesmentHelper.R")
source("clustering/PAM_NumCL_finder.R")

#extract Data
data <- extrapolate(seq(1971, 2000), vars = "all.diffDay")

#remove range variables
data[, ":=" (range.mslp = NULL, range.geopot = NULL)]

#scale and apply weights
weights <- c(rep(c(1/3, 1/6, rep(1/3, 2), rep(1/6, 4)), 2),
             rep(1/6, 12), rep(1/9, 18), rep(1/6, 2))

useDat <- as.data.table(scale(copy(data)[, date := NULL]))[, Map("*", .SD, weights)]

#create dist matrix
dissimilarity <- parallelDist(as.matrix(useDat), method = "manhattan",
                                     threads = detectCores() - 2)

#find best number of clusters
###CAUTION: takes a while
###no actual need to run this here
bestClustNumber(dissimilarity, "manhattan", "finalResult", range = 5:9)


#cluster with PAM
finalClust <- pam(dissimilarity, 6, diss = TRUE)

sil(finalClust, finalClust$clustering, dissimilarity, "pam")
#sil = 0.1411
data_pam_final <- data.table(date = data$date, cluster = finalClust$clustering)
tl <- Cl.timeline(data_pam_final, multiplied = T, showOpt = T)
#TLS = 0.3357
mos <- mosaic(data_pam_final, data_pam_final$cluster)
#HBdiff = 0.3309
