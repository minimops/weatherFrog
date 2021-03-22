#filter testing

source("clustering/Var_Extraction_Method/f_spatClust_funs.R")
source("clustering/dateExtractionHelper.R")

datestoCheck <- getDates(count = 10, timeframe = seq(2000, 2010), 
                         following = TRUE, gwltype = "BM")

dates1 <- getDates(count = 2, timeframe = seq(2000, 2010), 
                         following = TRUE, gwltype = "NWZ")

dates2 <- getDates(count = 2, timeframe = seq(2000, 2010), 
                         following = TRUE, gwltype = "HM")

save.DayFilter.Output(datestoCheck, "custom", "testing", type = "mslp", onePage = TRUE)

filterDayData("2006-03-10", "custom", type = "geopot")

library(fossil)


days <- getDates(2, seq(2006, 2010))
day1 <- readRDS("Data/cli_data_2k_avgDay.rds")[date %in% as.Date(days[[1]]), ][, date := NULL]
day2 <- readRDS("Data/cli_data_2k_avgDay.rds")[date %in% as.Date(days[[2]]), ][, date := NULL]

adj.rand.index(filterDay(day1, "avg_mslp")$cluster, filterDay(day2, "avg_mslp")$cluster)
#0.6727
rand.index(filterDay(day1, "avg_mslp")$cluster, filterDay(day2, "avg_mslp")$cluster)
#0.5908



dayx1 <- readRDS("Data/cli_data_2k_avgDay.rds")[date %in% as.Date(dates1[[1]]), ][, date := NULL]
dayx2 <- readRDS("Data/cli_data_2k_avgDay.rds")[date %in% as.Date(dates1[[2]]), ][, date := NULL]

rand.index(filterDay(dayx1, "avg_mslp")$cluster, filterDay(dayx2, "avg_mslp")$cluster)
#0.507 gleiche gwl

dayz1 <- readRDS("Data/cli_data_2k_avgDay.rds")[date %in% as.Date(dates2[[1]]), ][, date := NULL]
dayz2 <- readRDS("Data/cli_data_2k_avgDay.rds")[date %in% as.Date(dates2[[2]]), ][, date := NULL]

rand.index(filterDay(dayz1, "avg_mslp")$cluster, filterDay(dayz2, "avg_mslp")$cluster)
#0.75 gleiche gwl

rand.index(filterDay(dayx1, "avg_mslp")$cluster, filterDay(dayz1, "avg_mslp")$cluster)
#0.439 verschiedene gwl
#0.44 unterschiedliche gwl andere


filterDayData(days[[1]], "custom", "mslp")
filterDayData(days[[2]], "custom", "mslp")


#create distance matrix
library(usedist)

rand_distance <- function(x, y) {
  rand.index(x, y)
}

#custom distance
custom_distance <- function(x, y) {
  #eliminate all points of both vectors, where x has 0
  zerosX <- which(x == 0)
  x <- x[-zerosX]
  y <- y[-zerosX]
  
  #TODO maybe add penalty for subset 100 percent
  
  #get points of x, where point is in same cluster in y
  equal <- length(which(x == y))
  #number of points where true/number of points
  equal / length(x)
}


#make matrix days as rows, cluster
data <- readRDS("Data/cli_data_05_avgDay.rds")
data <- data[format(as.Date(date), "%Y") %in% c(2006), ][1:(10*160), ]
days <- unique(data[, date])
newMat <- data.table(matrix(ncol = 160))

system.time(
  for (day in days) {
    oneDay <- data[date %in% day, ][, date := NULL]
    newMat <- rbind(newMat, t(filterDay(oneDay, "avg_mslp")$cluster))
  }
)

system.time(testorono <- rbind(lapply(days, function(x) t(filterDay(data[date %in% x, ][, date := NULL],
                                     "avg_mslp")$cluster))))

##parallel testing

library(parallel)
data <- readRDS("Data/cli_data_05_avgDay.rds")[format(as.Date(date), "%Y") %in% c(2006, 2007), ]
days <- unique(data[, date])

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

cl <- makeCluster(5)

system.time(test <- clusterApply(cl, days, fun = fillFunkyPerDay))
system.time(test <- clusterApply(cl, c(2006, 2007), fun = fillFunkyPerYear))

system.time({
  testM <- as.data.table(clusterApply(cl, seq(2006, 2010), fun = fillFunkyPerYear1))
  saveRDS(testM, "Data/filter/clusID_mslp_05.rds")
  testG <- as.data.table(clusterApply(cl, seq(2006, 2010), fun = fillFunkyPerYear2))
  saveRDS(testG, "Data/filter/clusID_geopot_05.rds")
})

system.time(test <- clusterApply(cl, c(2006, 2007), fun = function(x) fillFunkyPerYear(x, "avg_geopot")))

stopCluster(cl)


#####testing end


daysMat <- as.matrix(na.omit(newMat))
saveRDS(daysMat, "Data/mslp_filtered_05.rds")


#daysMat <- readRDS("Data/mslp_filtered_05.rds")

#distance matrix
distMat <- dist_make(daysMat, rand_distance)

saveRDS(distMat, "Data/mslp_filtered_05_dist.rds")

#distMat <- readRDS("Data/mslp_filtered_05_dist.rds")

library(cluster)
#clustering
pam_fit <- pam(distMat, diss = TRUE, k = 7)

#attatch GWL and such
dates <- readRDS("Data/cli_data_05_avgDay_wide.rds")[, date]
newDat <- attachGwl(data.table(date = dates, cluster = pam_fit$clustering))

Cl.timeline(newDat)
mosaic(newDat, newDat$cluster)
sil(pam_fit, pam_fit$clustering, distMat, "pam")
