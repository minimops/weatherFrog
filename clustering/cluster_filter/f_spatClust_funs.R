###functions for spacial Clustering and its outputs

###CAUTION: This files gets sourced###

library(checkmate)
library(KneeArrower)
library(data.table)
library(ggplot2)
library(grid)
library(gridExtra)
library(dbscan)
library(e1071)

source("clustering/dayDrawer.R")
source("clustering/cluster_filter/filter_funs.R")

#this function spatial clusters a day with the a given algorythm
#inputs: a date (string or date class)
#        type = ("both" or "mslp" or "geopot") decide which params to run with
#        weights = a numeric of length 4 to give params different weights
#
filterDayData <- function(day, algo, type = "both", 
                          weights = c(1, 1, 1, 1), out = "grob", 
                          noPlot = FALSE) {
  assertDate(as.Date(day))
  assertSubset(type, c("both", "mslp", "geopot"))
  assertSubset(out, c("grob", "raw"))
  assertNumeric(weights, len = 4)
  assertSubset(algo, c("dbscan", "fuzzy", "custom"))
  
  if(type == "both" && algo == "custom") stop("doesn't work atm.")
  
  #subset day
  oneDay <- readRDS("Data/cli_data_2k_avgDay.rds")[date %in% as.Date(day), ]
  resultDT <- copy(oneDay)
  oneDay[, ":=" (longitude = weights[[1]] * scale(longitude), 
                 latitude = weights[[2]] * scale(latitude),
                 avg_mslp = weights[[3]] * scale(avg_mslp), 
                 avg_geopot = weights[[4]] * scale(avg_geopot))]
  
  switch(type,
         "both" = oneDay <- oneDay[, date := NULL],
         "mslp" = oneDay <- oneDay[, ":=" (date = NULL, 
                                           avg_geopot = NULL)],
         "geopot" = oneDay <- oneDay[, ":=" (date = NULL, 
                                             avg_mslp = NULL)]
  )
  
  #run algo
  switch (algo,
          "dbscan" = result <- runDBSCAN(oneDay),
          "fuzzy" = result <- runFUZZY(oneDay),
          "custom" = result <- filterDay(oneDay, paste0("avg_", type))
  )
  
  #attach clusterinfo to result dt
  plotRes <- data.frame(resultDT, cluster = as.factor(result$cluster))
  
  #plot dbresult
  dbResPlot <- drawDay(plotRes, whichFill = "cluster", showGuide = FALSE,
                       discrete = TRUE)
  
  
  world_map_local <- readRDS("Data/world_map_local.rds")
  coords_diff <- readRDS("Data/diff_coords.rds")
  
  #plot actual values:
  mslpPlot <- drawDay(plotRes, whichFill = "avg_mslp", showGuide = FALSE,
                      discrete = FALSE)
  
  geopotPlot <- drawDay(plotRes, whichFill = "avg_geopot", showGuide = FALSE,
                        discrete = FALSE)
  
  #gwl on that day
  gwlOnDay <- readRDS("Data/gwl.rds")[date %in% as.Date(day), ]$gwl
  
  text <- paste("The GWL on", as.character(day), "is", as.character(gwlOnDay))
  switch(type,
         "both" = plots <- list(dbResPlot, mslpPlot, geopotPlot),
         "mslp" = plots <- list(dbResPlot, mslpPlot),
         "geopot" = plots <- list(dbResPlot, geopotPlot)
  )
  ifelse(noPlot,
         {
        dbDay <- arrangeGrob(grobs = plots, nrow = 1,  top = textGrob(text))
        switch (out,
                "grob" = return(dbDay),
                "raw" = return(plots)
        )
    },
    grid.arrange(grobs = plots, nrow = 1,  top = textGrob(text))
  )
}



#this function saves outputs of a filter function as png's
#dates can be a vector of dates 
#if onePage is changed to TRUE, it will save the vector dates as a single image 
#without headers

save.DayFilter.Output <- function(dates, algo, filename ,onePage = FALSE, 
                          path = "documentation/plots", 
                          pathext  = NULL, type = "both", 
                          weights = c(1, 1, 1, 1)) {
  lapply(list(dates), function (x) assertDate(as.Date(x)))
  assertLogical(onePage)
  assertString(filename)
  assertSubset(type, c("both", "mixed", "mslp", "geopot"))
  assertString(algo)
  
  if(length(dates) > 10) stop("Keep number of days to <= 10. 
                              Greater Numbers lead to nonsensically sized images 
                              and Memmory overload.")
  if(type == "mixed" && isFALSE(onePage)) stop("mixed type only works in onePage
                                               mode for now.")
  
  finalPath <- ifelse(is.null(pathext), path, paste(path, pathext, sep = "/"))
  finalFilename <- paste(filename, algo, type, sep = "_")
  
  if(onePage){
    
    plist <- c()
    if(type == "mixed") { 
      for (i in seq_len(length(dates))) {
        plist <- c(plist, filterDayData(dates[[i]], algo = algo, noPlot = TRUE,
                                        out = "raw", type = "both", weights),
                   filterDayData(dates[[i]], algo = algo, out = "raw", 
                                 noPlot = TRUE, type = "mslp", weights)[1],
                   filterDayData(dates[[i]], algo = algo, out = "raw", 
                                 noPlot = TRUE, type = "geopot", weights)[1]
        )
      }
    } else {
          for (i in seq_len(length(dates))) {
            plist <- c(plist, filterDayData(dates[[i]], algo = algo,
                                            noPlot = TRUE, 
                                            out = "raw", type, weights))
          }
    }
    
    ggsave(filename = finalFilename, path = finalPath,
           grid.arrange(grobs = plist, nrow = length(dates)), 
           device = "png", height = 3 * length(dates), 
           width = 3 * length(plist) / length(dates))
  } else{
    
    for (i in seq_len(length(dates))) {
      ggsave(filename = paste(finalFilename, i, sep = "-"), 
             path = finalPath,
             filterDayData(dates[[i]], algo = algo, type, weights, 
                           noPlot = TRUE),
             device = "png", 
             width = 9, height = 3)
    }
  }
}


#this function performes DBSCAN
#it uses an eps caclulated by the kneeArrower.
#this is caluculated via kNN with k = 6
#the middle of the firstderivative and curviture calc is used as eps
#minPoints of dbscan is set to 9

runDBSCAN <- function(data) {
  tryCatch(assertDataTable(data), error = function(cond) assertMatrix(data))
  
  data <- as.matrix(data)
  #calc knee
  y <- kNNdist(data, k = 6)
  x <- seq(from = 1, to = 160, by = 1)
  (a <- findCutoff(x,y, method = "curvature")$y)
  (b <- findCutoff(x,y, method = "first")$y)
  
  return(dbscan(data, eps = (a+b)/2, minPts = 9))
}


#This function performes fuzzy clustering
#start points are selected as the extreme points of the day
#in both mslp and geopot
#TODO fix for when the values fall in a similar vicinity

runFUZZY <- function(data) {
  assertDataTable(data)
  
  startingPoints <- c()
  
  tryCatch({
    startingPoints <- rbind(startingPoints, 
                            copy(data)[avg_geopot == max(avg_geopot), ],
                            copy(data)[avg_geopot == min(avg_geopot), ])
  },
  error = function(cond) {
  }
  )
  
  tryCatch({
    startingPoints <- rbind(startingPoints, 
                            copy(data)[avg_mslp == max(avg_mslp), ],
                            copy(data)[avg_mslp == min(avg_mslp), ])
  },
  error = function(cond) {
  }
  )
  return(cmeans(data, centers = unique(startingPoints)))
}