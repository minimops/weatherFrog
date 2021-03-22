#trial of a filtering function

library(data.table)
library(KneeArrower)
library(checkmate)
library(dbscan)
library(akmedoids)


###CAUTION: This files gets sourced###

#returns the max and min rowid in that order of the given column in the dataset
getMinMax <- function(data, param) {
  assertDataTable(data)
  assertSubset(param, names(data))
  
  dat <- copy(data)[[param]]
  
  maxRow <- which(dat == max(dat))
  minRow <- which(dat == min(dat))

  c(maxRow, minRow)
}

# 
# world_map_local <- readRDS("Data/world_map_local.rds")
# coords_diff <- readRDS("Data/diff_coords.rds")
# 
# world_map_local +
#   geom_rect(data = copy(oneDay)[, ":=" (dist = distDT$distList1, id = distDT$clT)], 
#             mapping=aes(xmin=longitude - coords_diff[[1]],
#                                      xmax=longitude + coords_diff[[1]],
#                                      ymin=latitude - coords_diff[[2]],
#                                      ymax=latitude + coords_diff[[2]],
#                                      fill = as.numeric(id)), alpha = 0.7) +
#   scale_fill_gradient(low = "blue", high = "red",
#                       guide = F) +
#   geom_point(data = oneDay[c(24, 2), ], aes(x = longitude, y = latitude))
#   labs(title ="test", x = "", y = "") +
#   theme(axis.title.x=element_blank(),
#         axis.text.x=element_blank(),
#         axis.ticks.x=element_blank(),
#         axis.title.y=element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks.y=element_blank())





#creates clusterinfo (density based with fixed starting points)
filterDay <- function(data, param) {
  assertDataTable(data)
  assertSubset(param, c("avg_mslp", "avg_geopot"))
  ifelse(param == "avg_mslp", negparam <- "avg_geopot", 
         negparam <- "avg_mslp")
  # day <- "2005-05-20"
  # 
  # oneDay <- readRDS("Data/cli_data_2k_avgDay.rds")[date %in% as.Date(day),
  #                                 ][, date := NULL]
  # 
  # drawDay(oneDay, whichFill = "avg_mslp", discrete = FALSE, showGuide = FALSE)
  
  
  sc_oneDay <- as.data.table(scale(data))
  sc_oneDay <- sc_oneDay[, c(negparam) := NULL]

  #get density threshold
  # eps <- elbowPoint(seq(1, nrow(sc_oneDay)),
  #                               kNNdist(sc_oneDay, k = 10))$y
  #   
    eps <- findCutoff(seq(1, nrow(sc_oneDay)),
                  kNNdist(sc_oneDay, k = 10)
                  , method = "curvature")$y  #+ 0.008

  startingPoints <- getMinMax(sc_oneDay, param)
  runIndex <- 0
  distDT <- copy(sc_oneDay)[, clT := 0]
  
  Numcol <- ncol(sc_oneDay)
  
  for (cNum in startingPoints) {
    runIndex <- runIndex + 1
    
    distDT[, paste0("distList", runIndex) := euclDist(sc_oneDay, cNum)]

    distDT[get(paste0("distList", runIndex)) < eps, clT := runIndex]
    
    #case where only one point is added, add the closest one
    if(nrow(distDT[clT == runIndex , ]) == 1) {
      distDT[get(paste0("distList", runIndex)) == 
               sort(distDT[, get(paste0("distList", runIndex))])[[2]], clT := runIndex]
    }
    
    i <- 0
    repeat {
      oldClSize <- nrow(distDT[clT == runIndex, ])
      i <- i + 1
      new_eps <- eps - (i-1) * eps/6

      #points in the cluster
      dts <- copy(distDT)[clT == runIndex, ][, 1:Numcol]
      #TODO could be made more efficient, dont calc already calculated distances
      #matrix of the points and the distances to all other points
      logixMatrix <- apply(dts, MARGIN = 1, FUN = function(x) euclDist(sc_oneDay, 
                    which(duplicated(rbind(x, as.matrix(sc_oneDay)))) - 1) < new_eps)

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
      
      #break out of repeat loop if no new points are added to cluster
      if(oldClSize == nrow(distDT[clT == runIndex, ]) || new_eps < 0) break
    }
  }
  
  
  data.table(copy(data), cluster = distDT$clT)
  
  # drawDay(maxClust, "clT", showGuide = F)
  # 
  # drawDay(oneDay, whichFill = "avg_mslp", discrete = FALSE, showGuide = FALSE)
}


#returns a vector of euclidean distances of all points in data of row number 
#pointNr of data
euclDist <- function(data, pointNr) {
  #TODO assertions
  useMat <- as.matrix(data)
  apply(useMat, MARGIN = 1, FUN = function(x) dist(rbind(useMat[pointNr, ], x)))
  
}



###distance functions for clustering between days

library(fossil)

rand_distance <- function(x, y) {
  rand.index(x, y)
}


#custom distances
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
