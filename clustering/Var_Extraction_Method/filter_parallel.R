#paralleling filtering

library(parallel)

fillFunkyPerDay <- function(day) {
  library(data.table)
  data <- readRDS("Data/cli_data_05_avgDay.rds")[date %in% day, ]
  source("clustering/Var_Extraction_Method/filter_funs.R")
  
  return(cbind(t(filterDay(data[, date := NULL], "avg_mslp")$cluster), day))
  #lapply(days, function(x) t(filterDay(data[date %in% x, ][, date := NULL], "avg_mslp")$cluster))
}

fillFunkyPerYear1 <- function(year) {
  library(data.table)
  data <- readRDS("Data/cli_data_05_avgDay.rds")[format(date, "%Y") %in% year, ]
  days <- unique(data[, date])
  source("clustering/Var_Extraction_Method/filter_funs.R")
  
  return(rbind(vapply(days, function(x) 
    c(filterDay(data[date %in% x, ][, date := NULL], "avg_mslp")$cluster, x), 
    FUN.VALUE = numeric(length = 161))))
}

fillFunkyPerYear2 <- function(year) {
  library(data.table)
  data <- readRDS("Data/cli_data_05_avgDay.rds")[format(date, "%Y") %in% year, ]
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
  cl <- makeCluster(5)
  
  testM <- as.data.table(clusterApply(cl, seq(2006, 2010), fun = fillFunkyPerYear1))
  saveRDS(testM, "Data/filter/clusID_mslp_05.rds")
  testG <- as.data.table(clusterApply(cl, seq(2006, 2010), fun = fillFunkyPerYear2))
  saveRDS(testG, "Data/filter/clusID_geopot_05.rds")
  
  stopCluster(cl)
})
#2455 seconds (41 min)

#Read always after saved once
#testM <- readRDS("Data/filter/clusID_mslp_05.rds")
#testG <- readRDS("Data/filter/clusID_geopot_05.rds")

library(data.table)
library(zoo)

#transpose and recode date
mslp_05_filter <- as.data.table(t(testM))[, V161 := as.Date(V161)]
setnames(mslp_05_filter, "V161", "date")
geopot_05_filter <- as.data.table(t(testG))[, V161 := as.Date(V161)]
setnames(geopot_05_filter, "V161", "date")


###distance functions

rand_distance <- function(x, y) {
  rand.index(x, y)
}

#custom distance
custom_distance <- function(x, y) {
  #eliminate all points of both vectors, where x has 0
  ifelse(length(which(x == 0)) <= length(which(y == 0)),
    zeros <- which(x == 0), zeros <- which(y == 0))
  
  if(length(zeros) > 0){
    x <- x[-zeros]
    y <- y[-zeros]
  }
  
  #TODO maybe add penalty for subset 100 percent
  
  #get points of x, where point is in same cluster in y
  equal <- length(which(x == y))
  #number of points where true/number of points
  1 - equal / length(x)
}

###

library(usedist)

system.time(
  distMat_mslp <- dist_make(as.matrix(copy(mslp_05_filter)[, date := NULL]),
                            custom_distance)
)
system.time(
  distMat_geopot <- dist_make(as.matrix(copy(geopot_05_filter)[, date := NULL]),
                              custom_distance)
)
#85 seconds each
#this can be parralelized with library parallelDist, 
#but custom distance function needs to be in C++ for that.
#since it is not too bad for now, maybe do this in the future


#add distMatrix of both parameters together
distMat_both <- distMat_mslp + distMat_geopot

saveRDS(cbind(as.matrix(distMat_both), date = mslp_05_filter$date),
        "Data/filter/distMat_05_date.rds")
#distMat_both_date <- readRDS("Data/filter/distMat_05_date.rds")
#distMat_both <- as.dist(distMat_both_date[, -1827])

library(factoextra)

fviz_dist(distMat_both)

##copy from Cluster_filteredData,R

library(ggplot2)
library(parallel)

cl <- makeCluster(detectCores() - 1)

PamSilFun <- function(i, distM) {
  library(cluster)

  pam_fit <- pam(distM,
                 diss = TRUE,
                 k = i)
  pam_fit$silinfo$avg.width
}

sil_width <- unlist(clusterApply(cl, 4:15, PamSilFun, distM = distMat_both))
stopCluster(cl)

plot(4:15, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width",)
lines(4:15, sil_width)
print(sil_width)
##end copy
