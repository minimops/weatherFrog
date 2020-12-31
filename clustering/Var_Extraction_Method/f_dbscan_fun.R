###function for normal dbscan

library(checkmate)
library(KneeArrower)
library(data.table)
library(ggplot2)
library(grid)
library(gridExtra)
library(dbscan)

#this function spatial clusters a day with the dbscan algorythm
#inputs: a date (string or date class)
#        type = ("both" or "mslp" or "geopot") decide which params to run with
#        weights = a numeric of length 4 to give params different weights
#
#it uses an eps caclulated by the kneeArrower.
#this is caluculated via kNN with k = 6
#the middle of the firstderivative and curviture calc is used as eps
#minPoints of dbscan is set to 9

dbRun <- function(day, type = "both", weights = c(1.5, 1.5, 1, 1)) {
  assertDate(as.Date(day))
  assertSubset(type, c("both", "mslp", "geopot"))
  assertNumeric(weights, len = 4)
  
  #subset day
  oneDay <- readRDS("Data/cli_data_2k_avgDay.rds")[date %in% as.Date(day), ]
  resultDT <- copy(oneDay)
  oneDay[, ":=" (longitude = weights[[1]] * scale(longitude), latitude = weights[[2]] * scale(latitude),
                 avg_mslp = weights[[3]] * scale(avg_mslp), avg_geopot = weights[[4]] * scale(avg_geopot))]
  
  switch(type,
         "both" = oneDay <- as.matrix(oneDay[, date := NULL]),
         "mslp" = oneDay <- as.matrix(oneDay[, ":=" (date = NULL, avg_geopot = NULL)]),
         "geopot" = oneDay <- as.matrix(oneDay[, ":=" (date = NULL, avg_mslp = NULL)])
         )
  
  #calc knee
  y <- kNNdist(oneDay, k = 6)
  x <- seq(from = 1, to = 160, by = 1)
  (a <- KneeArrower::findCutoff(x,y, method = "curvature")$y)
  (b <- KneeArrower::findCutoff(x,y, method = "first")$y)
  
  result <- dbscan(oneDay, eps = (a+b)/2, minPts = 9)
  
  #attach clusterinfo to result dt
  plotRes <- data.frame(resultDT, cluster = as.factor(result$cluster))
  
  world_map_local <- readRDS("Data/world_map_local.rds")
  coords_diff <- readRDS("Data/diff_coords.rds")
  
  #plot dbresult
  dbResPlot <- world_map_local +
    geom_rect(data = plotRes, mapping=aes(xmin=longitude - coords_diff[[1]], xmax=longitude + coords_diff[[1]],
                                          ymin=latitude - coords_diff[[2]], ymax=latitude + coords_diff[[2]], 
                                          fill = cluster), alpha = 0.7) +
    scale_fill_brewer(palette = "Set1", guide = FALSE) +
    labs(title = "dbscan", x = "", y = "") +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
  
  #plot actual values:
  mslpPlot <- world_map_local +
    geom_rect(data = plotRes, mapping=aes(xmin=longitude - coords_diff[[1]], xmax=longitude + coords_diff[[1]],
                                          ymin=latitude - coords_diff[[2]], ymax=latitude + coords_diff[[2]], 
                                          fill = avg_mslp), alpha = 0.7) +
    scale_fill_gradient(low = "blue", high = "red", guide = FALSE) + 
    labs(title = "mslp", x = "", y = "") +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
  
  geopotPlot <- 
    world_map_local +
    geom_rect(data = plotRes, mapping=aes(xmin=longitude - coords_diff[[1]], xmax=longitude + coords_diff[[1]],
                                          ymin=latitude - coords_diff[[2]], ymax=latitude + coords_diff[[2]], 
                                          fill = avg_geopot), alpha = 0.7) +
    scale_fill_gradient(low = "blue", high = "red", guide = FALSE) + 
    labs(title = "geopot", x = "", y = "") +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
  
  #gwl on that day
  gwlOnDay <- readRDS("Data/gwl.rds")[date %in% as.Date(day), ]$gwl

  text <- paste("The GWL on", as.character(day), "is", as.character(gwlOnDay))
  switch(type,
         "both" = plots <- list(dbResPlot, mslpPlot, geopotPlot),
         "mslp" = plots <- list(dbResPlot, mslpPlot),
         "geopot" = plots <- list(dbResPlot, geopotPlot)
        )
  grid.arrange(grobs = plots, nrow = 1,  top = textGrob(text))
  
}
