#save grid of histograms for idiv params
library(data.table)
library(ggplot2)

#import our data
source("clustering/cluster_extr_var/f_extr_funs.R")
source("clustering/ClusterAssesmentHelper.R")

extr_30 <- extrapolate(seq(1971, 2000))
extr_30_gwl <- as.data.frame(attachGwl(extr_30))


for (param in names(extr_30)[2:49]) {
 
   plot <- ggplot(extr_30_gwl, aes_string(x = param)) +
    geom_histogram(aes(y = ..density..), bins = 30) +
    ggtitle(paste("Verteilung", param, "pro GWL")) +
    facet_wrap(~ gwl)
    
  ggsave(plot, file=paste0("plot_", param,".png"))
  # 
  # plot <- ggplot(extr_30_gwl, aes_string(x = param)) +
  #  ggtitle(paste("Verteilung", param, "pro GWL")) +
  #  geom_histogram(aes(y=..density..), bins = 30) +
  #  facet_wrap(~ gwl)
  # 
  #  ggsave(plot, filename = paste0(param, "Verteilung pro GWL.png"),
  #         device = "png", width = 130, height = 100, limitsize = FALSE)
}


#function to get winter/summer
getWinSum <- function(DATES) {
  W <- as.Date("2012-10-16", format = "%Y-%m-%d") # Winter Solstice
  S <- as.Date("2012-4-16",  format = "%Y-%m-%d") # Summer Solstice
  
  # Convert dates from any year to 2012 dates
  d <- as.Date(strftime(DATES, format="2012-%m-%d"))
  
  ifelse (d >= W | d < S, "W", "S")
}

extr_30_gwl_season <- copy(as.data.table(extr_30_gwl))[, season := getWinSum(date)]


for (param in names(extr_30_gwl_season)[2:49]) {
  
  plot <- ggplot(extr_30_gwl_season, aes_string(x = param)) +
    geom_histogram(aes(y = ..density.., color = season), bins = 30, alpha = 0.4) +
    ggtitle(paste("Verteilung", param, "pro GWL")) +
    facet_wrap(~ gwl)
  
  ggsave(plot, file=paste0("plot_season", param,".png"))

}
