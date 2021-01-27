#filter testing

source("clustering/Var_Extraction_Method/f_spatClust_funs.R")
source("clustering/dateExtractionHelper.R")

datestoCheck <- getDates(count = 10, timeframe = seq(2000, 2010), 
                         following = TRUE, gwltype = "BM")

save.DayFilter.Output(datestoCheck, "custom", "testing", type = "mslp", onePage = TRUE)

library(fossil)


days <- getDates(2, seq(2006, 2010))
day1 <- readRDS("Data/cli_data_2k_avgDay.rds")[date %in% as.Date(days[[1]]), ][, date := NULL]
day2 <- readRDS("Data/cli_data_2k_avgDay.rds")[date %in% as.Date(days[[2]]), ][, date := NULL]

adj.rand.index(filterDay(day1, "avg_mslp")$cluster, filterDay(day2, "avg_mslp")$cluster)
#0.6727
rand.index(filterDay(day1, "avg_mslp")$cluster, filterDay(day2, "avg_mslp")$cluster)
#0.5908

filterDayData(days[[1]], "custom", "mslp")
filterDayData(days[[2]], "custom", "mslp")


#create distance matrix
library(usedist)

rand_distance <- function(x, y) {
  rand.index(x, y)
}

#make matrix days as rows, cluster
data <- readRDS("Data/cli_data_05_avgDay.rds")
data <- data[format(as.Date(date), "%Y") %in% "2006", ]
days <- unique(data[, date])
newMat <- data.table(matrix(ncol = 160))

i <- 0
for (day in days) {
  i <- i + 1
  oneDay <- data[date %in% day, ][, date := NULL]
  newMat <- rbind(newMat, t(filterDay(oneDay, "avg_mslp")$cluster))
}

daysMat <- as.matrix(na.omit(newMat))
#distance matrix
distMat <- dist_make(daysMat, rand_distance)

library(cluster)
#clustering
pam_fit <- pam(distMat, diss = TRUE, k = 7)

#attatch GWL and such
dates <- readRDS("Data/cli_data_05_avgDay_wide.rds")[format(as.Date(date), "%Y") %in% 2006, date]
newDat <- attachGwl(data.table(date = dates, cluster = pam_fit$clustering))

Cl.timeline(newDat)
mosaic(newDat, newDat$cluster)
sil(pam_fit, pam_fit$clustering, distMat, "pam")
