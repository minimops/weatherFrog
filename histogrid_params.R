#save grid of histograms for idiv params
library(data.table)
library(ggplot2)

#import our data
source("clustering/Var_Extraction_Method/f_extr_funs.R")

extr_30 <- extrapolate(seq(1971, 2000))
extr_30_gwl <- as.data.frame(attachGwl(extr_30))

for (param in names(extr_30)[2:3]) {
 
  # png(filename = paste0(param, "Verteilung_pro_GWL.png"), )
  # 
  # ggplot(extr_30_gwl, aes_string(x = param)) +
  #   ggtitle(paste("Verteilung", param, "pro GWL")) +
  #   geom_histogram(aes(y=..density..), bins = 30) +
  #   facet_wrap(~ gwl)
  # 
  # dev.off()
  
  # plot <- ggplot(extr_30_gwl, aes_string(x = param)) +
  #  ggtitle(paste("Verteilung", param, "pro GWL")) +
  #  geom_histogram(aes(y=..density..), bins = 30) +
  #  facet_wrap(~ gwl)
  # 
  #  ggsave(plot, filename = paste0(param, "Verteilung pro GWL"),
  #         device = "png", width = 130, height = 100, limitsize = FALSE)
}

