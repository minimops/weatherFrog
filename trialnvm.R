
df <- data.frame(range = 5:9, sil_width = c(0.12, 0.141, 0.115, 0.105, 0.095))


trialP <- ggplot(df, aes(x = range, y = sil_width)) + 
  geom_point() +
  geom_line() +
  theme_bw() +
  labs(x = "Cluster Anzahl", y = "Silhouettenkoeffizient", 
       title = "Optimale Anzahl an Cluster")


ggsave(paste0("documentation/plots/PAMfinal/"
                    , fname, ".png"), plot = trialP, device = "png", width = 5, height = 3)



