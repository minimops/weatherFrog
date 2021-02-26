# Histogram for every extracted variable in every cluster

#load(file= "weatherFrog/final_cluster/f_data.rds")
cluster <- PAMres$clustering
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



  # Boxplot
for (param in names(f_data)[3:50]) {
    ggplot( aes_string(x= as.factor(cluster), y=param, fill=as.factor(cluster)), data = f_data) + 
    geom_boxplot() +
    xlab("cluster") +
    ylab("param") +
    theme_bw() +
    theme(legend.position="none") +
    ggtitle(paste("Verteilung",param,"in jedem Cluster")) +
      scale_fill_brewer(palette = "Set1")
    
    ggsave(plot, file=paste0("histogram_boxplot_clustersolution/boxplot_distribution_in_variable_", param,".png"))
    

}
    

       