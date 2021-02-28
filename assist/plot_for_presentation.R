library(data.table)
library(ggplot2)
library(plyr)

source("assist/functions_for_cluster_description.R")



# Histogram for every extracted variable in every cluster

#import PAMres and f_data fro our cluster solution
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
 #data <- cbind(Jahreszeit, data) 
 #Jahreszeitvektor als spalte hinzufügen
data$year <- format(data$date,"%Y")
data$year <- as.numeric(data$year)

plot <-  ggplot(aes(x =year, fill = as.factor(cluster), color = as.factor(cluster)), data = data) +
  geom_histogram(alpha = 0.9) +
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
  ylab("Dichte") +
  xlab("Jahr")


ggsave(plot,file ="final_cluster/distribution of years over cluster.png")

# Distribution of clusters split in seasons
# wie Farben ändern?

data1 <- as.data.table(table(as.factor(data$cluster),data$Jahreszeit))
colnames(data1) <- c("cluster","Jahreszeit","Anzahl")

 (plot <- ggplot(aes(x = as.factor(cluster), y = Anzahl, group = Jahreszeit,fill = Jahreszeit), data = data1) +
  geom_bar(position = "dodge", stat = "identity") +
  theme_bw() +
  theme(
    legend.position = "none"
  ) +
  scale_fill_brewer(palette="Set1") + 
  ggtitle("Verteilung der Cluster über die Jahre aufgeteilt 
           nach Sommer und Winterzeit") +
  xlab("Cluster") +
  ylab("relative Häufigkeit"))
 


ggsave(plot,file = "final_cluster/distribution of years over cluster_ splited_in_seasons1.png")

#same, not grouprd, but as mosaic plot

data1 <- as.data.table(table(as.factor(data$cluster),data$Jahreszeit))
colnames(data1) <- c("cluster","Jahreszeit","Anzahl")

(plot <- ggplot(aes(x = as.factor(cluster), y = Anzahl, group = Jahreszeit,fill = Jahreszeit), data = data1) +
    geom_bar(position = "fill", stat = "identity") +
    theme_bw() +
    theme(
      legend.position = "none"
    ) +
    scale_fill_brewer(palette="Set1") + 
    ggtitle("Verteilung der Cluster über die Jahre aufgeteilt 
           nach Sommer und Winterzeit") +
    xlab("Cluster") +
    ylab("relative Häufigkeit"))



ggsave(plot,file = "final_cluster/mosail_seasons_cluster.png")
