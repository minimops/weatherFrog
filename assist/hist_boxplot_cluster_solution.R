library(data.table)
library(ggplot2)
library(plyr)

source("assist/functions_for_cluster_description.R")



# Histogram for every extracted variable in every cluster

#load(file= "weatherFrog/final_cluster/f_data.rds")
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
  ylab("Luftdruck [hPa]") +
  theme_bw() +
  theme(legend.position="none") +
  ggtitle("Mittelwert des Luftdrucks in jedem Cluster") +
  scale_fill_brewer(palette = "Set1")

ggsave(plot, file = "final_cluster/mean_mslp_boxplot.png")

# Boxplot of mean.geopot in every cluster

  ggplot( aes(x= as.factor(cluster), y= mean.geopot, fill=as.factor(cluster)), data = f_data) + 
  geom_boxplot() +
  xlab("Cluster") +
  ylab("Geopotential in [gpm]") +
  theme_bw() +
  theme(legend.position="none") +
  ggtitle("Mittelwert des Geopotentials in jedem Cluster") +
  scale_fill_brewer(palette = "Set1")

ggsave(plot, file = "final_cluster/mean_geopot_boxplot.png")    


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
par(mfrow = c(1,2))
min_max_boxplot <- f_data[,c(1,5,6)]
min_max_boxplot <- reshape2::melt(min_max_boxplot, id.vars = "cluster", measure.vars = c("max.mslp", "min.mslp"))


  plot <-  ggplot( aes(x=as.factor(cluster), y=value, fill = as.factor(cluster)), data = min_max_boxplot) + 
  geom_boxplot() +
    theme_bw() +
    facet_wrap(~variable) +
  theme(legend.position="none") +
  xlab("Cluster") +
  ylab ("Luftdruck in [hPa]")+
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
 
 
  
 
 
  
 

  # boxplot with mean in each cluster
  mean_location <- f_data[,c(1,13,23,33,34,35,36,37,38,39)]
  mean_location <- scale(mean_location[,-1], scale = FALSE)
  names <- colnames(f_data[,c(1,13,23,33,34,35,36,37,38,39)])
  mean_location <- reshape2::melt(mean_location, id.vars = "cluster", measure.vars = names )
  
  ggplot(mean_location, aes(fill=Var2, y=value, x= Var2)) + 
    geom_bar(position="dodge", stat="identity") +
    facet_wrap(~ as.factor(Var1))
  
  ggplot( aes(x=as.factor(cluster), y=value, fill= variable), data = mean_location) + 
    geom_boxplot() +
    xlab("cluster") +
    theme(legend.position="none") +
    xlab("") +
    xlab("") 
#interesting, but not practicable  
  
# histogram: destribution of the cluster over the years

data <-f_data 
data <- cbind(Jahreszeit, data)
data$year <- format(data$date,"%Y")
data$year <- as.numeric(data$year)

plot <- ggplot(aes(x =year, fill = as.factor(cluster), color = as.factor(cluster)), data = data) +
  geom_histogram(alpha = 0.9) +
  facet_wrap(~as.factor(cluster), scale="free_x") +
  theme_bw() +
  theme(
    panel.spacing = unit(0.1, "lines"),
    #axis.title.x=element_blank(),
    #axis.text.x=element_blank(),
    #axis.ticks.x=element_blank(),
    legend.position = "none"
  ) +
  scale_color_brewer(palette="Set1")+
  scale_fill_brewer(palette="Set1") + 
  ggtitle("Verteilung der Cluster über die Jahre") +
  ylab("Häufigkeit") +
  xlab("Jahr")


ggsave(plot,file ="final_cluster/distribution of years over cluster.png")

# Distribution of clusters split in seasons


plot <- ggplot(aes(x = year, fill = Jahreszeit), data = data) +
  geom_histogram(alpha = 0.9) +
  facet_wrap(~as.factor(cluster), scale="free_x") +
  theme_bw() +
  theme(
    panel.spacing = unit(0.1, "lines"),
    #axis.title.x=element_blank(),
    #axis.text.x=element_blank(),
    #axis.ticks.x=element_blank()
  ) +
  scale_color_brewer(palette="Set1")+
  scale_fill_brewer(palette="Set1") + 
  ggtitle("Verteilung der Cluster über die Jahre aufgeteilt nach Sommer und Winterzeit") +
  xlab("Jahr") +
  ylab("Häufigkeit")

ggsave(plot,file = "final_cluster/distribution of years over cluster_ splited_in_seasons1.png")


 ggplot(aes(x = year, fill = as.factor(cluster)), data = data) +
  geom_histogram(alpha = 0.9) +
  facet_wrap(~as.factor(Jahreszeit), scale="free_x") +
  theme_bw() +
  theme(
    panel.spacing = unit(0.1, "lines"),
    #axis.title.x=element_blank(),
    axis.text.x=element_text(),
    #axis.ticks.x=element_blank()
  ) +
   labs(fill ="Cluster") +
  scale_color_brewer(palette="Set1")+
  scale_fill_brewer(palette="Set1") + 
  ggtitle("Verteilung der Cluster über die Jahre aufgeteilt nach
                      Sommer und Winterzeit") +
  xlab("Jahr") +
  ylab("Häufigkeit")

ggsave(plot,file = "final_cluster/distribution of years over cluster_ splited_in_seasons2.png")
