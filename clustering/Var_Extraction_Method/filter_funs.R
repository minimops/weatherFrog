#trial of a filtering function

#returns the max and min rowid in that order of the given column in the dataset
getMinMax <- function(data, param) {
  assertDataTable(data)
  assertSubset(param, names(data))
  
  maxRow <- which(copy(data)[[param]] == max(copy(data)[[param]]))
  minRow <- which(copy(data)[[param]] == min(copy(data)[[param]]))

  c(maxRow, minRow)
}

filterDay <- function(data, param) {
  #TODO assertions
  
  
  oneDay <- readRDS("Data/cli_data_2k_avgDay.rds")[date %in% as.Date(day), ][, date := NULL]
  
  drawDay(oneDay, whichFill = "avg_mslp", discrete = FALSE, showGuide = FALSE)
  
  sc_oneDay <- as.data.table(scale(oneDay))
  
  kNNdistplot(oneDay, k = 10)
  
  y <- kNNdist(sc_oneDay, k = 10)
  x <- seq(from = 1, to = nrow(sc_oneDay), by = 1)
  (a <- findCutoff(x,y, method = "curvature")$y)
  (b <- findCutoff(x,y, method = "first")$y)
  
  eps <- 1.5
  
  maxpoint <- getMinMax(sc_oneDay, "avg_geopot")[[2]]
  
  distList <- c()
  for (mp in seq_len(nrow(sc_oneDay))) {
    distList <- c(distList, euclDist(sc_oneDay[maxpoint, ], sc_oneDay[mp, ]))
  }
  Nbs <- as.data.table(distList)[distList < eps, clT := 1][is.na(clT), clT := 0]
  for (i in seq(1,3)) {
    new_eps <- eps - i * 0.5
    for (nbs in which(Nbs$clT == 1)) {
      for (mp in seq_len(nrow(sc_oneDay))) {
        #x <- c(x, euclDist(sc_oneDay[nbs, ], sc_oneDay[mp, ]))
        if(euclDist(sc_oneDay[nbs, ], sc_oneDay[mp, ]) < new_eps) Nbs$clT[mp] <- 1
      }
    }
  }

  
  
  maxClust <- data.table(copy(oneDay), clT = Nbs$clT)
  
  drawDay(maxClust, "clT", showGuide = F)
  
  drawDay(oneDay, whichFill = "avg_geopot", discrete = FALSE, showGuide = FALSE)
}


euclDist <- function(Point1, Point2) {
  #TODO assertions
  
  dist(rbind(Point1, Point2), method = "euclidean")
}
