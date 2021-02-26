# plot helper

library(data.table)
library(checkmate)
library(ggplot2)
library(gridExtra)

#This funcion draws the local world map with a specified fill. 
#you can also choose if you wish a legend 
#you have to specify if the scale is discrete or continuous

###CAUTION: This files gets sourced###

drawDay <- function(data, whichFill, showGuide = TRUE, discrete = TRUE) {
  assertDataFrame(data)
  assertSubset(whichFill, names(data))
  assertLogical(c(showGuide, discrete))
  
  data <- as.data.frame(data)
  
  world_map_local <- readRDS("Data/world_map_local.rds")
  coords_diff <- readRDS("Data/diff_coords.rds")
  
  fFill <- data[, whichFill]

 ifelse(discrete, 
   plot <- 
     world_map_local +
       geom_rect(data = data, mapping=aes(xmin=longitude - coords_diff[[1]], 
                                             xmax=longitude + coords_diff[[1]],
                                             ymin=latitude - coords_diff[[2]], 
                                             ymax=latitude + coords_diff[[2]], 
                                             fill = as.factor(fFill)), alpha = 0.7) +
       scale_fill_brewer(palette = "Set1", guide = showGuide) +
       labs(title =whichFill, x = "", y = "") +
       theme(axis.title.x=element_blank(),
             axis.text.x=element_blank(),
             axis.ticks.x=element_blank(),
             axis.title.y=element_blank(),
             axis.text.y=element_blank(),
             axis.ticks.y=element_blank())
   ,
    plot <-
      world_map_local +
        geom_rect(data = data, mapping=aes(xmin=longitude - coords_diff[[1]],
                                              xmax=longitude + coords_diff[[1]],
                                              ymin=latitude - coords_diff[[2]],
                                              ymax=latitude + coords_diff[[2]],
                                              fill = as.numeric(fFill)), alpha = 0.7) +
        # scale_fill_gradient(low = "blue", high = "red",
        #                     guide = showGuide) +
        scale_fill_gradient(name = "Mslp in Pa", low = "blue", high = "red") +
        ggtitle("Mslp am 01-01-2006 um 0 Uhr") 
        #labs(title =whichFill, x = "", y = "") +
        # theme(axis.title.x=element_blank(),
        #       axis.text.x=element_blank(),
        #       axis.ticks.x=element_blank(),
        #       axis.title.y=element_blank(),
        #       axis.text.y=element_blank(),
        #       axis.ticks.y=element_blank())
    )
  return(plot)
}


multDays <- function(dates, param) {
  assertSubset(param, c("mslp", "geopot"))
  ifelse(param == "mslp", negParam <- "avg_geopot", negParam <- "avg_mslp")
  
  data <- readRDS("Data/cli_data_30_avgDay.rds")
  plist <- list()
  i <- 0
  for (day in dates) {
    i <- i + 1
    oneDay <- copy(data)[date %in% as.Date(day), ][, ":=" (avg_mslp = NULL,
                                                           date = NULL)]
    plot <- drawDay(as.data.frame(oneDay), 
                    whichFill = paste0("avg_", param),
                    showGuide = FALSE, discrete = F)
    plist[[i]] <- plot
  }
  grid.arrange(grobs = plist, nrow = ceiling(sqrt(length(plist))))
  
}

