library(data.table)
library(ggplot2)
library(dplyr)

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

ggsave(plot, file = "final_cluster/mean_mslp_boxplot.png", height = 3, width = 5)

# Boxplot of mean.geopot in every cluster

plot <- ggplot( aes(x= as.factor(cluster), y= mean.geopot, fill=as.factor(cluster)), data = f_data) + 
  geom_boxplot() +
  xlab("Cluster") +
  ylab("Geopotential in [gpm]") +
  theme_bw() +
  theme(legend.position="none") +
  ggtitle("Mittelwert des Geopotentials in jedem Cluster") +
  scale_fill_brewer(palette = "Set1")

ggsave(plot, file = "final_cluster/mean_geopot_boxplot.png", height = 3, width = 5)    


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

# some things and numbers for writing the report

aggregate(f_data[,c(3,5,6,11,13,14)], by = list(as.factor(f_data$cluster)),sd)

cli_gwl_1971 <- readRDS("Data\\cli_gwl_1971.rds")

#Season vector

cli_gwl_1971_Month_Day <- cli_gwl_1971
cli_gwl_1971_Month_Day$Month_Day <- format(cli_gwl_1971_Month_Day$date, "%m-%d")

day_16_to_31 <- seq(from = 16, to = 31, by = 1)
day_16_to_30 <- seq(from = 16, to = 30, by = 1)
day_1_to_9 <- seq(from = 1, to = 9, by = 1)
day_1_to_9 <- paste0(0, day_1_to_9)
day_10_to_15 <- seq(from = 10, to = 15, by = 1)
day_1_to_15 <- c(day_1_to_9,day_10_to_15)

month_10_day_16_31 <- paste(10,day_16_to_31,sep = "-")
month_4_day_1_15 <- paste("04",day_1_to_15,sep = "-")
month_4_day_16_30 <- paste("04",day_16_to_30,sep = "-")
month_10_day_1_15 <- paste(10,day_1_to_15,sep = "-")




cli_gwl_1971_Month_Day <- cli_gwl_1971_Month_Day %>%
  mutate(Jahreszeit = case_when(month %in% c("11","12","01","02","03") ~ "Winterzeit",
                                month %in% c("05","06","07","08","09") ~ "Sommerzeit",
                                Month_Day %in% month_10_day_16_31 ~ "Winterzeit",
                                Month_Day %in% month_4_day_1_15 ~ "Winterzeit",
                                Month_Day %in% month_4_day_16_30 ~ "Sommerzeit",
                                Month_Day %in% month_10_day_1_15 ~ "Sommerzeit"))

data_2000 <- cli_gwl_1971_Month_Day[cli_gwl_1971_Month_Day$date <= "2000-12-31",]
Jahreszeit <- data_2000$Jahreszeit


gwl_data <- cli_gwl_1971[cli_gwl_1971$date <= "2000-12-31",]
gwl <- gwl_data$gwl
f_data_gwl <- cbind(gwl,f_data)


table(f_data$cluster,f_data_gwl$gwl)
prop.table(table(f_data$cluster,f_data_gwl$gwl),2)


# filter cluster greater than 2 days

index_length <- rleid(f_data_gwl$cluster)
f_data_gwl <- cbind(index_length,f_data_gwl)


table_cluster <- as.data.table(table(index_length))
less_equal2 <- table_cluster[table_cluster$N <= 2,]


f_data_gwl_greater2 <- f_data_gwl

for ( i in less_equal2$index_length){
  f_data_gwl_greater2 <- subset(f_data_gwl_greater2,index_length != i)
  
}

# proof, if there are no continous days greater 2
table_cluster <- as.data.table(table(f_data_gwl_greater2$index_length))
less_equal2 <- table_cluster[table_cluster$N <= 2,]



# filter cluster greater than 3 days

table_cluster <- as.data.table(table(index_length))
less_equal3 <- table_cluster[table_cluster$N <= 3,]


f_data_gwl_greater3 <- f_data_gwl

for ( i in less_equal3$index_length){
  f_data_gwl_greater3 <- subset(f_data_gwl_greater3,index_length != i)
  
}

# proof, if there are no continous days greater 3
table_cluster <- as.data.table(table(f_data_gwl_greater3$index_length))
less_equal3 <- table_cluster[table_cluster$N <= 3,]


# mosaicplot without clusterdays smaller than 2 days
library(ggmosaic)
 ggplot(data = f_data_gwl_greater2) +
  geom_mosaic(aes(x = product(cluster, gwl), fill = cluster), offset = 0.005) +
  theme_classic() +
  ggtitle("Mosaikplot für Cluster ~ GWL") +
  labs(x = "GWL") +
  scale_fill_brewer(palette = "Set1") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  guides(fill=guide_legend(title = "Cluster", reverse = TRUE)) +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.line.y = element_blank())


ggsave("bericht/assets/mosaic_gwl_greater2days.png", plot,
       device = "png", width = 5, height = 3)




# mosaicplot without clusterdays smaller than 3 days
library(ggmosaic)
plot <- ggplot(data = f_data_gwl_greater3) +
  geom_mosaic(aes(x = product(cluster, gwl), fill = cluster), offset = 0.005) +
  theme_classic() +
  ggtitle("Mosaikplot für Cluster ~ GWL") +
  labs(x = "GWL") +
  scale_fill_brewer(palette = "Set1") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  guides(fill=guide_legend(title = "Cluster", reverse = TRUE)) +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.line.y = element_blank())


ggsave("bericht/assets/mosaic_gwl_greater3days.png", plot,
       device = "png", width = 5, height = 3)

library(data.table)


# Mean. mslp per season
f_data_gwl <- cbind(Jahreszeit,f_data_gwl)

  plot <- ggplot(aes(x=Jahreszeit, y=mean.mslp, fill = Jahreszeit), data = f_data_gwl) + 
  geom_boxplot() +
  theme_bw() +
  theme(legend.position="none") +
  xlab("Jahreszeit") +
  ylab ("Luftdruck in [hPa]")+
  ggtitle ("Verteilung des Mittelwerts des Luftdrucks 
 getrennt nach Jahreszeit") +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1")

ggsave(plot, file="final_cluster/jahreszeit_mean.mslp.png", width = 5, height = 3)

# mean.geopot per season

 plot <- ggplot(aes(x=Jahreszeit, y=mean.geopot, fill = Jahreszeit), data = f_data_gwl) + 
  geom_boxplot() +
  theme_bw() +
  theme(legend.position="none") +
  xlab("Jahreszeit") +
  ylab ("Geopotential in gpm")+
  ggtitle ("Verteilung des Mittelwerts des Geopotentials 
 getrennt nach Jahreszeit") +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1")

ggsave(plot, file="final_cluster/jahreszeit_mean.geopot.png", width = 5, height = 3)

# min.mslp per season
 plot <- ggplot(aes(x=Jahreszeit, y=min.mslp, fill = Jahreszeit), data = f_data_gwl) + 
  geom_boxplot() +
  theme_bw() +
  theme(legend.position="none") +
  xlab("Jahreszeit") +
  ylab ("Luftdruck in [hPa]")+
  ggtitle ("Verteilung des minimalen Luftdrucks getrennt nach Jahreszeit") +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1")

ggsave(plot, file="final_cluster/jahreszeit_min.mslp.png", width = 5, height = 3)


# max.mslp per season
plot <- ggplot(aes(x=Jahreszeit, y=max.mslp, fill = Jahreszeit), data = f_data_gwl) + 
  geom_boxplot() +
  theme_bw() +
  theme(legend.position="none") +
  xlab("Jahreszeit") +
  ylab ("Luftdruck in [hPa]")+
  ggtitle ("Verteilung des maximalen Luftdrucks getrennt nach Jahreszeit") +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1")

ggsave(plot, file="final_cluster/jahreszeit_max.mslp.png", width = 5, height = 3)


# min.geopot per season
plot <- ggplot(aes(x=Jahreszeit, y=min.geopot, fill = Jahreszeit), data = f_data_gwl) + 
  geom_boxplot() +
  theme_bw() +
  theme(legend.position="none") +
  xlab("Jahreszeit") +
  ylab ("Geopotential in gpm")+
  ggtitle ("Verteilung des minimalen Geopotentials getrennt nach Jahreszeit") +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1")

ggsave(plot, file="final_cluster/jahreszeit_min.geopot.png", width = 5, height = 3)

# max.geopot per season
plot <- ggplot(aes(x=Jahreszeit, y=max.geopot, fill = Jahreszeit), data = f_data_gwl) + 
  geom_boxplot() +
  theme_bw() +
  theme(legend.position="none") +
  xlab("Jahreszeit") +
  ylab ("Geopotential in gpm")+
  ggtitle ("Verteilung des maximalen Geopotentials getrennt nach Jahreszeit") +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1")

ggsave(plot, file="final_cluster/jahreszeit_max.geopot.png", width = 5, height = 3)






