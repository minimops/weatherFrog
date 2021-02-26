# Histogram for every extracted variable in every cluster

#load(file= "weatherFrog/final_cluster/f_data.rds")
cluster <- PAMres$clustering
f_data <- cbind(cluster,f_data)




f_data %>%
  ggplot( aes(x= mean.mslp)) +
  geom_histogram(fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  facet_wrap(~cluster, scale="free_x") +
  theme_bw() +
  theme(
    panel.spacing = unit(0.1, "lines"),
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank() 
  ) +
  ggtitle("mean.mslp")
