#pca plot pres

library(ggfortify)

dat_wide <- readRDS("Data/cli_data_30_avgDay_wide.rds")

pca30 <- prcomp(as.data.frame(dat_wide)[, 2:321], scale. = TRUE)

pcaPlot <- autoplot(pca30, alpha = 0.1, colour = "darkblue") +
  theme_bw() + 
  ggtitle("Visualisierung der Daten mit PCA")

ggsave("documentation/plots/fplots/pcaPlot.png", pcaPlot, device = "png",
       width = 5, height = 3)
