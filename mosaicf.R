#mosaikplot
library(ggmosaic)

clustID <- readRDS("finalDATA/PAMres.rds")$clustering
date <- readRDS("finalDATA/f_data.rds")$date

source("clustering/ClusterAssesmentHelper.R")

MosDat <- attachGwl(data.table(date = date, cluster  = as.factor(clustID)))
saveRDS(MosDat, file = "date_cluster_gwl.rds")

mosaicLegend <- ggplot(data = MosDat) +
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
  
  
ggsave("documentation/plots/fplots/mosaicLegend.png", mosaicLegend,
       device = "png", width = 9, height = 5)



mosaicYaxis <- ggplot(data = MosDat) +
  geom_mosaic(aes(x = product(cluster, gwl), fill = cluster), offset = 0.005) +
  theme_classic() +
  ggtitle("Mosaik cluster ~ GWL") +
  scale_fill_brewer(palette = "Set1") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "none")

ggsave("documentation/plots/fplots/mosaicYaxis.png", mosaicYaxis,
       device = "png", width = 7, height = 4)


