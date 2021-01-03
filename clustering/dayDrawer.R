# plot helper

library(data.table)
library(checkmate)
library(ggplot2)


#This funcion draws the local world map with a specified fill. 
#you can also choose if you wish a legend 
#you have to specify if the scale is discrete or continuous

###CAUTION: This files gets sourced###

drawDay <- function(data, whichFill, showGuide = TRUE, discrete = TRUE) {
  assertDataFrame(data)
  assertSubset(whichFill, names(data))
  assertLogical(c(showGuide, discrete))
  
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
        scale_fill_gradient(low = "blue", high = "red",
                            guide = showGuide) +
        labs(title =whichFill, x = "", y = "") +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank())
    )
  return(plot)
}