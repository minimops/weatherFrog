#trial of a filtering function

library(data.table)
library(KneeArrower)
library(checkmate)

#returns the max and min rowid in that order of the given column in the dataset
getMinMax <- function(data, param) {
  assertDataTable(data)
  assertSubset(param, names(data))
  
  maxRow <- which(copy(data)[[param]] == max(copy(data)[[param]]))
  minRow <- which(copy(data)[[param]] == min(copy(data)[[param]]))

  c(maxRow, minRow)
}


#creates clusterinfo (density based with fixed starting points)
filterDay <- function(data, param) {
  #TODO assertions
  
  day <- "2006-12-24"
  
  oneDay <- readRDS("Data/cli_data_2k_avgDay.rds")[date %in% as.Date(day),
                                  ][, date := NULL]
  
  #drawDay(oneDay, whichFill = "avg_mslp", discrete = FALSE, showGuide = FALSE)
  
  
  sc_oneDay <- as.data.table(scale(oneDay))
  sc_oneDay <- sc_oneDay[, avg_geopot := NULL]
  #TODO weights
  #sc_oneDay[, ":=" (latitude = 1.5 * latitude, longitude = 1.5 * longitude)]
  
  #get denity threshold
  eps <- findCutoff(seq(1, nrow(sc_oneDay)),
                  kNNdist(sc_oneDay, k = 10)
                  , method = "curvature")$y

  #TODO something wrong here
  startingPoints <- getMinMax(sc_oneDay, "avg_mslp")
  runIndex <- 0
  distDT <- copy(sc_oneDay)[, clT := 0]
  
  for (cNum in startingPoints) {
    runIndex <- runIndex + 1
    
    distDT[, paste0("distList", runIndex) := euclDist(sc_oneDay, cNum)]

    distDT[get(paste0("distList", runIndex)) < eps, clT := runIndex]
    
    for (i in seq(1, 2)) {
      new_eps <- eps - i * eps/3
      #new_eps <- eps / (i + 1)
      
      #points in the cluster
      dts <- copy(distDT)[clT == runIndex, ][, 
                      paste0("distList", runIndex) := NULL][, clT := NULL]
      #TODO could be made more efficient, dont calc already calculated distances
      #matrix of the points and the distances to all other points
      logixMatrix <- apply(dts, MARGIN = 1, FUN = function(x) euclDist(sc_oneDay, 
                    which(duplicated(rbind(x, as.matrix(sc_oneDay))))) < new_eps)

      changeIndecies <- unique(which(logixMatrix, arr.ind = TRUE)[,1])
      
      lapply(changeIndecies, FUN = function(x) {
          switch (as.character(distDT[x, clT]),
          "0" = set(distDT, as.integer(x), "clT", runIndex),
          runIndex = "",
          if(distDT[x, get(paste0("distList", runIndex))] < 
             distDT[x, get(paste0("distList", as.character(distDT[x, clT])))]){
            set(distDT, as.integer(x), "clT", runIndex)
          }
          )})
      
      # for (nbs in which(Nbs$clT == runIndex)) {
      #   for (mp in seq_len(nrow(sc_oneDay))) {
      #     if(euclDist(sc_oneDay[nbs, ], sc_oneDay[mp, ]) < new_eps) Nbs$clT[mp] <- runIndex
      #   }
      # }
    }
  }
  
  
  maxClust <- data.table(copy(oneDay), clT = distDT$clT)
  
  drawDay(maxClust, "clT", showGuide = F)
  
  drawDay(oneDay, whichFill = "avg_mslp", discrete = FALSE, showGuide = FALSE)
}


#returns a vector of euclidean distances of all points in data of row number 
#pointNr of data
euclDist <- function(data, pointNr) {
  #TODO assertions
  useMat <- as.matrix(data)
  apply(useMat, MARGIN = 1, FUN = function(x) dist(rbind(useMat[pointNr, ], x)))
  
}
