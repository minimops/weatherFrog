library(ggplot2)
library(data.table)

#read f_data and PAMres

#function to get winter/summer
getWinSum <- function(DATES) {
  W <- as.Date("2012-10-16", format = "%Y-%m-%d") # Winter Solstice
  S <- as.Date("2012-4-16",  format = "%Y-%m-%d") # Summer Solstice
  
  # Convert dates from any year to 2012 dates
  d <- as.Date(strftime(DATES, format="2012-%m-%d"))
  
  ifelse (d >= W | d < S, "W", "S")
}

data1 <- data.table(cluster = PAMres$clustering, season = getWinSum(f_data$date))

data1[, fillVar := paste0(cluster, season)][season == "W", 
                        season := "Winter"][season == "S", season := "Sommer"]


saisonPlot2 <- ggplot(data1, aes(cluster)) +
  geom_bar(aes(fill = as.factor(fillVar)), position = "fill") +
  scale_fill_manual(values = c("#E41A1C", "#FBB4AE", "#377EB8", "#B3CDE3",
                               "#4DAF4A", "#CCEBC5", "#984EA3", "#DECBE4",
                               "#FF7F00", "#FED9A6", "#FFFF33", "#FFFFCC")) +
  labs(x = "Cluster", y = "Anteil", title = "Verteilung der Cluster über Saison") +
  theme_bw() +
  theme(legend.position = "none")+
  scale_x_continuous(breaks = seq(1, 6))

ggsave(plot = saisonPlot2, "documentation/plots/PAMfinal/SaisonPlot2.png", device = "png",
       width = 5, height = 3.66, units = "in")


PAMres <- pamfinal
f_data <- datafinal
