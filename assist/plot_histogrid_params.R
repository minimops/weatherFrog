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


##difference over day trial

Change_day_gwl <- attachGwl(diffDay(seq(1971, 2010)))

ggplot(as.data.frame(Change_day_gwl), aes(x = diff)) +
  ggtitle("Verteilung Änderung über den Tag pro GWL") +
  geom_histogram(aes(y=..density..), bins = 30) +
  facet_wrap(~ gwl)
#doesnt really seem to have any distribution differences


#per season

#attach season
Change_day_gwl_season <- copy(Change_day_gwl)[, season := getWinSum(date)]

ggplot(as.data.frame(Change_day_gwl_season), aes(x = diff)) +
  ggtitle("Verteilung Änderung über den Tag pro GWL pro Saison") +
  geom_histogram(aes(y=..density.., color = season, fill = season),
                 bins = 30, alpha = 0.4) +
  facet_wrap(~ gwl)



#per param
cli_data_val <- melt(cli_data_30, id.vars = c("date", "time", "longitude", "latitude"),
                     measure.vars = c("mslp", "geopotential"), variable.name = "type",
                     value.name = "value")

wideTime_val <- longToWide(cli_data_val, id = c("date", "time", "type"),
                           col = c("longitude", "latitude"),
                           vars = c("value"))

cols2 <- names(wideTime_val)[-c(1, 2, 3)]
maxChange_tile_day_val <- copy(wideTime_val)[, (cols2) := lapply(.SD, function(x) max(x) - min(x)),
                                             by = c("date", "type"), .SDcols = cols2][, .SD[1], by = c("date", "type")][, time := NULL] 

Change_day_val <- copy(maxChange_tile_day_val)[, .(diff = sum(.SD)), by = c("date", "type"), .SDcols = cols2]

ggplot(as.data.frame(Change_day_val), aes(x = diff)) +
  ggtitle("Verteilung Änderung über den Tag pro Messwert") +
  geom_histogram(aes(y=..density..),
                 bins = 30) +
  facet_wrap(~ type)


Change_day_val_gwl <- attachGwl(Change_day_val)

ggplot(as.data.frame(Change_day_val_gwl), aes(x = diff)) +
  ggtitle("Verteilung Änderung über den Tag pro GWL pro Messwert") +
  geom_histogram(aes(y=..density.., fill = type), bins = 30) +
  facet_wrap(~ gwl)


#attach season
Change_day_val_gwl_season <- copy(Change_day_val_gwl)[, season := getWinSum(date)][, seasonType := paste0(season, type)]

ggplot(as.data.frame(Change_day_val_gwl_season), aes(x = diff)) +
  ggtitle("Verteilung Änderung über den Tag pro GWL pro Saison * Messwert") +
  geom_histogram(aes(y=..density.., fill = seasonType),
                 bins = 30) +
  facet_wrap(~ gwl)
