#paralleling filtering

library(parallel)
library(data.table)

fillFunkyPerDay <- function(day, data) {
  library(data.table)
  data <- data[date %in% day, ]
  source("clustering/Var_Extraction_Method/filter_funs.R")
  
  return(cbind(t(filterDay(data[, date := NULL], "avg_mslp")$cluster), day))
  #lapply(days, function(x) t(filterDay(data[date %in% x, ][, date := NULL], "avg_mslp")$cluster))
}

fillFunkyPerYear1 <- function(year) {
  library(data.table)
  data <- readRDS("Data/cli_data_30_avgDay.rds")[format(date, "%Y") %in% year, ]
  days <- unique(data[, date])
  source("clustering/Var_Extraction_Method/filter_funs.R")
  
  return(rbind(vapply(days, function(x) 
    c(filterDay(data[date %in% x, ][, date := NULL], "avg_mslp")$cluster, x), 
    FUN.VALUE = numeric(length = 161))))
}

fillFunkyPerYear2 <- function(year) {
  library(data.table)
  data <- readRDS("Data/cli_data_30_avgDay.rds")[format(date, "%Y") %in% year, ]
  days <- unique(data[, date])
  source("clustering/Var_Extraction_Method/filter_funs.R")
  
  return(rbind(vapply(days, function(x) 
    c(filterDay(data[date %in% x, ][, date := NULL], "avg_geopot")$cluster, x), 
    FUN.VALUE = numeric(length = 161))))
}


#filters and saves both parameters for the last 5 year period
### CAUTION: Runtime very high!!!
system.time({
  #TODO dynamic cluster size
  cl <- makeCluster(8)
  
  testM <- as.data.table(clusterApply(cl, seq(1971, 2000), fun = fillFunkyPerYear1))
  saveRDS(testM, "Data/filter/clusID_mslp_30.rds")
  testG <- as.data.table(clusterApply(cl, seq(1971, 2000), fun = fillFunkyPerYear2))
  saveRDS(testG, "Data/filter/clusID_geopot_30.rds")
  
  stopCluster(cl)
})
#2455 seconds (41 min)

#Read always after saved once
testM <- readRDS("Data/filter/clusID_mslp_05.rds")
testG <- readRDS("Data/filter/clusID_geopot_05.rds")

library(data.table)
library(zoo)

#transpose and recode date
mslp_05_filter <- as.data.table(t(testM))[, V161 := as.Date(V161)]
setnames(mslp_05_filter, "V161", "date")
geopot_05_filter <- as.data.table(t(testG))[, V161 := as.Date(V161)]
setnames(geopot_05_filter, "V161", "date")


mslp_30_filter <- as.data.table(t(testM))[, V161 := as.Date(V161)]
setnames(mslp_30_filter, "V161", "date")
geopot_30_filter <- as.data.table(t(testG))[, V161 := as.Date(V161)]
setnames(geopot_30_filter, "V161", "date")

###distance functions

library(fossil)

rand_distance <- function(x, y) {
  rand.index(x, y)
}

#custom distance
custom_distance <- function(x, y) {
  #max nonzero
  maxPoints <- max(length(which(x != 0)), length(which(y != 0)))
  #eliminate all points of both vectors, where x has 0
  ifelse(length(which(x == 0)) <= length(which(y == 0)),
    zeros <- which(x == 0), zeros <- which(y == 0))
  
  if(length(zeros) > 0){
    x <- x[-zeros]
    y <- y[-zeros]
  }
  
  #get points of x, where point is in same cluster in y
  equal <- length(which(x == y))
  #number of points where true/number of points
  1 - equal / maxPoints
}

###

library(usedist)


#system.time(
    distMat_mslp <- dist_make(as.matrix(copy(mslp_30_filter)[, date := NULL]),
                            custom_distance)
    saveRDS(distMat_mslp, "Data/filter/distMat_mslp_30.rds")
#)
#system.time(
  distMat_geopot <- dist_make(as.matrix(copy(geopot_30_filter)[, date := NULL]),
                              custom_distance)
  saveRDS(distMat_geopot, "Data/filter/distMat_geopot_30.rds")
#)
#85 seconds each
#this can be parralelized with library parallelDist, 
#but custom distance function needs to be in C++ for that.
#since it is not too bad for now, maybe do this in the future


#add distMatrix of both parameters together
distMat_both <- distMat_mslp + distMat_geopot

saveRDS(cbind(as.matrix(distMat_both), date = mslp_30_filter$date),
        "Data/filter/distMat_30_date.rds")
distMat_both_date <- readRDS("Data/filter/distMat_30_date.rds")
distMat_both <- as.dist(distMat_both_date[, -ncol(distMat_both_date)])

library(factoextra)

fviz_dist(distMat_both)

##copy from Cluster_filteredData,R

source("clustering/PAM_NumCL_finder.R")

bestClustNumber(distMat_both)
bestClustNumber(distMat_geopot)
bestClustNumber(distMat_mslp)

library(cluster)
source("clustering/ClusterAssesmentHelper.R")

#both custom
pam_both <- pam(distMat_both, diss = TRUE, k = 6)

sil(pam_both, pam_both$clustering, distMat_both, "pam")

both_data <- data.table(date = mslp_05_filter$date, cluster = pam_both$clustering)
Cl.timeline(both_data)

data_both_gwl <- attachGwl(both_data)
mosaic(data_both_gwl, data_both_gwl$cluster)


#both rand.index
#This takes way too long in single threaded mode
distMat_mslp_rand <- dist_make(as.matrix(copy(mslp_05_filter)[, date := NULL]),
                          rand_distance)

distMat_geopot_rand <- dist_make(as.matrix(copy(geopot_05_filter)[, date := NULL]),
                            rand_distance)

distMat_both_rand <- distMat_geopot_rand + distMat_mslp_rand

bestClustNumber(distMat_both, "manhattan", "filter_both")
bestClustNumber(distMat_geopot)
bestClustNumber(distMat_mslp)
###end

#adding to extrapolate dataset
source("clustering/Var_Extraction_Method/f_extr_funs.R")

data <- extrapolate(seq(2006, 2010))
data <- scaleNweight(data)

library(parallelDist)

distMat_data <- parDist(as.matrix(copy(data)[, date := NULL]), method = "manhattan")
bestClustNumber(distMat_data)
# use 6
pam_data <- pam(distMat_data, diss = TRUE, k = 6)

sil(pam_data, pam_data$clustering, distMat_data, "pam")

data_data <- data.table(date = mslp_05_filter$date, cluster = pam_data$clustering)
Cl.timeline(data_data)

data_data_gwl <- attachGwl(data_data)
mosaic(data_data_gwl, data_data_gwl$cluster)


#just add them as is because:
head(distMat_data)
head(distMat_both)

distMat_all <- distMat_both + distMat_data
head(distMat_all)

bestClustNumber(distMat_all)
#deffo reduces our silhouette like this

pam_all_data <- pam(distMat_all, diss = TRUE, k = 6)

sil(pam_all_data, pam_all_data$clustering, distMat_all, "pam")

data_all <- data.table(date = mslp_05_filter$date, cluster = pam_all_data$clustering)
Cl.timeline(data_all)

data_all_gwl <- attachGwl(data_all)
mosaic(data_all_gwl, data_all_gwl$cluster)


#differences?
#just filtering to extr data clustering
rand.index(pam_data$clustering, pam_both$clustering)
#0.742
#extr data to all data
rand.index(pam_data$clustering, pam_all_data$clustering)
#0.958


#test with higher weight
distMat_all_high <- 4 * distMat_both + distMat_data
bestClustNumber(distMat_all_high)


