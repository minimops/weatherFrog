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
testM <- readRDS("Data/filter/clusID_mslp_30.rds")
testG <- readRDS("Data/filter/clusID_geopot_30.rds")

#testM <- readRDS("clusData/filter/clusID_mslp_05.rds")

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
    saveRDS(distMat_mslp, "Data/filter/distMat_mslp_30_2.rds")
#)
#system.time(
  distMat_geopot <- dist_make(as.matrix(copy(geopot_30_filter)[, date := NULL]),
                              custom_distance)
  saveRDS(distMat_geopot, "Data/filter/distMat_geopot_30_2.rds")
#)
#85 seconds each
#this can be parralelized with library parallelDist, 
#but custom distance function needs to be in C++ for that.
#since it is not too bad for now, maybe do this in the future


#add distMatrix of both parameters together
distMat_both <- distMat_mslp + distMat_geopot

saveRDS(as.matrix(distMat_both),
        "Data/filter/distMat_30_date_2.rds")


library(factoextra)

fviz_dist(distMat_both)

##copy from Cluster_filteredData,R

source("clustering/PAM_NumCL_finder.R")

bestClustNumber(distMat_both, range = 5:9, metric = "manhattan", fname = "filter2_both")
bestClustNumber(distMat_geopot, range = 5:9, metric = "manhattan", fname = "filter2_only_geopot")
bestClustNumber(distMat_mslp, range = 5:9, metric = "manhattan", fname = "filter2_only_mslp")

library(cluster)
source("clustering/ClusterAssesmentHelper.R")

#just mslp
pam_mslp <- pam(distMat_mslp, diss = TRUE, k = 5)
#k = 5
sil(pam_mslp, pam_mslp$clustering, distMat_mslp, "pam")
#sil = 0.1369 
mslp_data <- data.table(date = mslp_30_filter$date, cluster = pam_mslp$clustering)
Cl.timeline(mslp_data, multiplied = T)
#TLS = -0.2027
mosaic(mslp_data, mslp_data$cluster)
#HBdiff = 0.3980

#just geopot
pam_geopot <- pam(distMat_geopot, diss = TRUE, k = 6)
#k = 6
sil(pam_geopot, pam_geopot$clustering, distMat_geopot, "pam")
#sil = 0.1541
geopot_data <- data.table(date = mslp_30_filter$date, cluster = pam_geopot$clustering)
Cl.timeline(geopot_data, multiplied = T)
#TLS = -0.3898
mosaic(geopot_data, geopot_data$cluster)
#HBdiff = 0.3162

#both custom
pam_both <- pam(distMat_both, diss = TRUE, k = 5)
#k = 5
sil(pam_both, pam_both$clustering, distMat_both, "pam")
#sil = 0.0832
both_data <- data.table(date = mslp_30_filter$date, cluster = pam_both$clustering)
Cl.timeline(both_data, multiplied = T)
#TLS = -0.1527
mosaic(both_data, both_data$cluster)
#HBdiff = 0.4100



#both rand.index
#CAUTION!!!!
#This takes way too long in single threaded mode
distMat_mslp_rand <- dist_make(as.matrix(copy(mslp_05_filter)[, date := NULL]),
                          rand_distance)

distMat_geopot_rand <- dist_make(as.matrix(copy(geopot_05_filter)[, date := NULL]),
                            rand_distance)

distMat_both_rand <- distMat_geopot_rand + distMat_mslp_rand
###end


#adding to extrapolate dataset
source("clustering/Var_Extraction_Method/f_extr_funs.R")
library(parallelDist)
library(parallel)

datextr <- extrapolate(seq(1971, 2000))

#copy from finalExplore.R
weights <- c(rep(c(1/3, 1/6, rep(1/3, 2), rep(1/6, 4)), 2),
             rep(1/6, 12), rep(1/9, 18), rep(1/6, 2))
d <- copy(datextr)[readRDS("Data/change_day_mslp.rds"), on = "date"]
datadiff <- copy(d)[readRDS("Data/change_day_geopot.rds"), on = "date"]
dataNoRange <- copy(datadiff)[, ":=" (range.mslp = NULL, range.geopot = NULL)]

useDat <- as.data.table(scale(copy(dataNoRange)[, date := NULL]))[, Map("*", .SD, weights)]
dissimilarity <- parallelDist(as.matrix(useDat), method = "manhattan",
                              threads = detectCores() - 2)



#just add them as is because:
head(dissimilarity)
head(distMat_both)

sum(dissimilarity) / sum(distMat_both)
# = 6.86 , sprich Gewicht des Filterns heir auf ca 1/7


#1/7
distMat_all <- distMat_both + dissimilarity
head(distMat_all)

bestClustNumber(distMat_all, range = 5:9, metric = "manhattan", fname = "filter+extr")
#deffo reduces our silhouette like this

pam_all_data <- pam(distMat_all, diss = TRUE, k = 5)

sil(pam_all_data, pam_all_data$clustering, distMat_all, "pam")
#sil = 0.0994
data_all <- data.table(date = mslp_30_filter$date, cluster = pam_all_data$clustering)
Cl.timeline(data_all)
#TLS = 0.5663
mosaic(data_all, data_all$cluster)
#HBdiff = 0.3878



#differences in 5 years
#just filtering to extr data clustering
rand.index(pam_data$clustering, pam_both$clustering)
#0.742
#extr data to all data
rand.index(pam_data$clustering, pam_all_data$clustering)
#0.958


#test with higher weight
distMat_all_high <- 4 * distMat_both + dissimilarity
bestClustNumber(distMat_all_high, range = 5:9, metric = "manhattan", fname = "filter+extr")

pam_all_data_higher <- pam(distMat_all_high, diss = TRUE, k = 5)
#k = 5
sil(pam_all_data_higher, pam_all_data_higher$clustering, distMat_all_high, "pam")
#sil = 0.1401
data_all_high <- data.table(date = mslp_30_filter$date, cluster = pam_all_data_higher$clustering)
Cl.timeline(data_all_high, multiplied = T, showOpt = T)
#TLS = 0.1818
mosaic(data_all_high, data_all_high$cluster)
#HBdiff = 0.4405


#test with just mslp added 1/4

distMat_extr_mslp <- dissimilarity + 3 * distMat_mslp

bestClustNumber(distMat_extr_mslp, range = 5:9, metric = "manhattan", fname = "filter_mslp+extr")

pam_mslp_extr <- pam(distMat_extr_mslp, diss = TRUE, k = 5)
#k = 5
sil(pam_mslp_extr, pam_mslp_extr$clustering, distMat_extr_mslp, "pam")
#sil = 0.1373
data_mslp_extr <- data.table(date = mslp_30_filter$date, cluster = pam_mslp_extr$clustering)
tl <- Cl.timeline(data_mslp_extr, multiplied = T, showOpt = T)
#TLS = 0.4118
ggsave("bericht/assets/TL_filterMslp_exa.png", tl, device = "png",
       width = 5, height = 3)
mos <- mosaic(data_mslp_extr, data_mslp_extr$cluster)
#HBdiff = 0.4122
ggsave("bericht/assets/mosaic_filterMslp_exa.png", mos, device = "png",
       width = 9, height = 5)
