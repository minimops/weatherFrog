library(data.table)
library(ggplot2)
library(plyr)

source("assist/functions_for_cluster_description.R")



# Histogram for every extracted variable in every cluster
############
#import PAMres and f_data from our cluster solution
cluster <- PAMres$clustering
f_data$mean.mslp <- f_data$mean.mslp/100
f_data$max.mslp <- f_data$max.mslp/1000
f_data$min.mslp <- f_data$min.mslp/1000

f_data$mean.geopot <- f_data$mean.geopot/9.80665
f_data$max.geopot <- f_data$max.geopot/9.80665
f_data$min.geopot <- f_data$min.geopot/9.80665
f_data <- cbind(cluster,f_data)


library(ggplot2)

for (param in names(f_data)[3:50]) {
  plot <- ggplot(aes_string(x = param, fill = as.factor(cluster), color = as.factor(cluster)), data = f_data) +
    geom_histogram(alpha = 0.9) +
    facet_wrap(~as.factor(cluster), scale="free_x") +
    theme_bw() +
    theme(
      panel.spacing = unit(0.1, "lines"),
      axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      legend.position = "none"
    ) +
    scale_color_brewer(palette="Set1")+
    scale_fill_brewer(palette="Set1") + 
    ggtitle(paste("Verteilung", param, "in jedem Cluster"))
  
  
  ggsave(plot, file=paste0("histogram_boxplot_clustersolution/hist_distribution_in_variable_", param,".png"))
  
}   



# Boxplot for all parameters
for (param in names(f_data)[3:50]) {
  plot <- ggplot( aes_string(x= as.factor(cluster), y=param, fill=as.factor(cluster)), data = f_data) + 
    geom_boxplot() +
    xlab("cluster") +
    ylab("param") +
    theme_bw() +
    theme(legend.position="none") +
    ggtitle(paste("Verteilung",param,"in jedem Cluster")) +
    scale_fill_brewer(palette = "Set1")
  
  ggsave(plot, file=paste0("histogram_boxplot_clustersolution/boxplot_distribution_in_variable_", param,".png"))
  
  
}

# Boxplot of mean.mslp in every cluster

plot <- ggplot( aes(x= as.factor(cluster), y= mean.mslp, fill=as.factor(cluster)), data = f_data) + 
  geom_boxplot() +
  xlab("Cluster") +
  ylab("mslp in [hPa]") +
  theme_bw() +
  theme(legend.position="none") +
  ggtitle("Mittelwert des Luftdrucks in jedem Cluster") +
  scale_fill_brewer(palette = "Set1")

ggsave(plot, file = "final_cluster/mean_mslp_boxplot.png")

# Boxplot of mean.geopot in every cluster

plot <- ggplot( aes(x= as.factor(cluster), y= mean.geopot, fill=as.factor(cluster)), data = f_data) + 
  geom_boxplot() +
  xlab("Cluster") +
  ylab("Geopotential in [gpm]") +
  theme_bw() +
  theme(legend.position="none") +
  ggtitle("Mittelwert des Geopotentials in jedem Cluster") +
  scale_fill_brewer(palette = "Set1")

ggsave(plot, file = "final_cluster/mean_geopot_boxplot.png")    


# Boxplot of intensitiy- high. geopot in every cluster

plot <- ggplot( aes(x= as.factor(cluster), y= intensity.high.geopot, fill=as.factor(cluster)), data = f_data) + 
  geom_boxplot() +
  xlab("Cluster") +
  ylab("Geopotential in [gpm]") +
  theme_bw() +
  theme(legend.position="none") +
  ggtitle("Intensity.high.geopot in jedem Cluster") +
  scale_fill_brewer(palette = "Set1")

ggsave(plot, file = "final_cluster/intensity.high.geopot_boxplot.png")    




# Plot for number of observations in each cluster


(cluster_number <- as.data.table(table(PAMres$clustering)))


plot <- ggplot(cluster_number) +
  geom_bar( aes(x = V1, y= N,fill = V1),stat = "identity", alpha=0.7, width=0.5) +
  theme_bw() +
  theme(
    legend.position="none",
    # plot.title = element_text(size=12)
  ) +
  ggtitle("Anzahl der Beobachtungen in jedem Cluster") +
  xlab("Cluster") +
  ylab("Häufigkeit") +
  scale_fill_brewer(palette = "Set1")
ggsave(plot, file="final_cluster/number_of_observations_in_each_cluster.png")
?ggsave()


# Boxplots with min and max mslp in one plot

min_max_boxplot <- f_data[,c(1,5,6)]
min_max_boxplot <- reshape2::melt(min_max_boxplot, id.vars = "cluster", measure.vars = c("max.mslp", "min.mslp"))


plot <- ggplot( aes(x=as.factor(cluster), y=value, fill = as.factor(cluster)), data = min_max_boxplot) + 
  geom_boxplot() +
  theme_bw() +
  facet_wrap(~variable) +
  theme(legend.position="none") +
  xlab("Cluster") +
  ylab ("mslp in [hPa]") +
  ggtitle ("Minimaler und maximaler Luftdruck in jedem Cluster") +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1")
ggsave(plot, file="final_cluster/min_max_mslp.png")


min_max_boxplot1 <- f_data[,c(1,13,14)]
min_max_boxplot1 <- reshape2::melt(min_max_boxplot1, id.vars = "cluster", measure.vars = c("max.geopot", "min.geopot"))


 plot <- ggplot(aes(x=as.factor(cluster), y=value, fill = as.factor(cluster)), data = min_max_boxplot1) + 
  geom_boxplot() +
  theme_bw() +
  facet_wrap(~variable) +
  theme(legend.position="none") +
  xlab("Cluster") +
  ylab ("Geopotential in [gpm]")+
  ggtitle ("Minimaler und maximaler Luftdruck in jedem Cluster") +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1")

ggsave(plot, file="final_cluster/min_max_geopot.png")


# histogram: distribution of the cluster over the years

data <-f_data 
data$year <- format(data$date,"%Y")
data$year <- as.numeric(data$year)

 plot <- ggplot(aes(x =year, fill = as.factor(cluster), color = as.factor(cluster)), data = data) +
  geom_histogram(aes(y = ..density..), alpha = 0.9, bins = 30) +
  facet_wrap(~as.factor(cluster), scale="free_x") +
  theme_bw() +
  theme(
    panel.spacing = unit(0.9, "lines"),
    #axis.title.x=element_text(size = 10),
    #axis.text.x=element_text(size = 8),
    #axis.ticks.x=element_blank(),
    legend.position = "none"
  ) +
  scale_color_brewer(palette="Set1")+
  scale_fill_brewer(palette="Set1") + 
  ggtitle("Verteilung der Cluster über die Jahre") +
  ylab("Anteil") +
  xlab("Jahr")

ggsave("documentation/plots/fplots/clust_years.png", plot, device = "png",
       width = 5, height = 3)
ggsave(plot,file ="final_cluster/distribution of years over cluster.png")

# Distribution of clusters split in seasons
# wie Farben ändern?



data1 <- data.table(cluster = data$cluster, season = getWinSum(data$date))


data1[, fillVar := paste0(cluster, season)][season == "W", season := "Winter"][season == "S", season := "Sommer"]

library(cowplot)
newlegend <- get_legend(ggplot(data1, aes(cluster, fill = season)) +
                          geom_bar() +
                          scale_fill_grey(name = "Saison") +
                          theme_bw())

plotSeason <- ggplot(data1, aes(cluster)) +
  geom_bar(aes(fill = as.factor(fillVar)), position = "fill") +
  scale_fill_manual(values = c("#E41A1C", "#FBB4AE", "#377EB8", "#B3CDE3",
                               "#4DAF4A", "#CCEBC5", "#984EA3", "#DECBE4",
                               "#FF7F00", "#FED9A6", "#FFFF33", "#FFFFCC")) +
  labs(x = "Cluster", y = "Anteil", title = "Veteilung der Cluster über Saison") +
  theme_bw() +
  theme(legend.position = "none")
  
grid.arrange(plotSeason, right = newlegend)
  

legendSomm <- get_legend()

ggplot(data1, aes(cluster)) +
  geom_bar(aes(fill = as.factor(cluster))) +
  scale_fill_brewer(name = "Cluster", palette = "Set1") +
  theme(legend.direction = "horizontal",
        legend.box.just = "top",
        legend.box.margin = margin(1, 6))



custPal <- data.frame(rbind(
  cbind(seq(1, 11, by = 2), brewer.pal(6, "Set1")),
  cbind(seq(2, 12, by =2), brewer.pal(6, "Pastel1"))
))

custPal[order(as.numeric(custPal$X1)), ]$X2 
  


# barplot of mean mslp

clusterDescription <- descriptive_array(f_data[,-c(1,2)],PAMres$clustering) 
descriptive_mean <- as.data.table(clusterDescription[,,1])
descriptive_mean <- as.data.table(sapply(descriptive_mean,as.numeric))
cluster <- as.factor(c(1,2,3,4,5,6))
descriptive_mean <- cbind(cluster,descriptive_mean)



ggplot(descriptive_mean,aes(x = cluster,y = mean.mslp)) +
  geom_bar(stat="identity", fill= cluster, alpha=0.7, width=0.5) +
  theme_bw() +
  theme(
    legend.position="none",
    plot.title = element_text(size = 12)) +
  ggtitle("mean mslp in every cluster") +
  xlab("cluster number")+
  scale_fill_brewer(palette = "Set1")

ggsave(plot, file="final_cluster/mean_mslp_for_every_cluster.png")

# barplot of scaled means, mean.mslp

data <- as.data.table(scale(descriptive_mean[,-1], scale = FALSE))
data <-cbind(cluster, data)

plot <- ggplot(data,aes(x = cluster,y = mean.mslp, fill = cluster)) +
  geom_bar(stat="identity", alpha=0.7, width=0.5) +
  theme_bw() +
  theme(
    legend.position="none",
    plot.title = element_text(size = 12)) +
  ggtitle("Unterschiede des mittleren Luftdrucks in den Clustern zum 
                                  Gesamtmittelwert") +
  xlab("Cluster")+
  ylab("Luftdruck in [hPa]") +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1")

ggsave(plot, file="final_cluster/scaled_mean.mslp in every cluster.png") 


# barplot of scaled means, mean.geopot

data <- as.data.table(scale(descriptive_mean[,-1], scale = FALSE))
data <-cbind(cluster, data)

plot <- ggplot(data,aes(x = cluster,y = mean.geopot, fill = cluster)) +
  geom_bar(stat="identity", alpha=0.7, width=0.5) +
  theme_bw() +
  theme(
    legend.position="none",
    plot.title = element_text(size = 12)) +
  ggtitle("Unterschiede des mittleren Geopotentials in den Clustern zum 
                                    Gesamtmittelwert") +
  xlab("Cluster")+
  ylab("Geopotential in gpm") +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1")

ggsave(plot, file="final_cluster/scaled_geopot.mslp in every cluster.png") 





