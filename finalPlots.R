
pamfinal <- readRDS("wetransfer-ed1938/PAMres.rds")
distfinal <- readRDS("wetransfer-ed1938/f_dist.rds")
datafinal <- readRDS("wetransfer-ed1938/f_data.rds")

datafinalID <- copy(datafinal)[, cluster := pamfinal$clustering]


# 1. Timeline ganz
Cl.timeline(copy(datafinalID))

ggsave("TimelineFull", path = "Documentation/plots/PAMfinal/", device = "jpeg",
       width = 5, height = 3)


# 2. Timeline cut
Cl.timeline(copy(datafinalID), cut = 41)

ggsave("TimelineCut", path = "Documentation/plots/PAMfinal/", device = "jpeg",
       width = 5, height = 3)

# 3. Timeline nach Cluster ganz
Cl.timeline(copy(datafinalID), seperated = TRUE)
ggsave("TimelineClusterFull", path = "Documentation/plots/PAMfinal/", device = "jpeg",
       width = 5, height = 3)

# 4. Timeline nach Cluster cut
Cl.timeline(copy(datafinalID), seperated = TRUE, cut = 40)

ggsave("TimelineClusterCut", path = "Documentation/plots/PAMfinal/", device = "jpeg",
       width = 5, height = 3)







reanalyse <- readRDS("Data/data_reanalysis_20201109.rds")
a <- head(reanalyse, 4)
time <- c(rep("1900-01-01 00:00:00", 4), "1900-01-01 18:00:00", "1900-01-02 00:00:00", 
          rep("2010-12-31 18:00:00", 2))
a$time <- time
mytable <- tableGrob(a, rows = NULL, theme = ttheme_default(core = list(bg_params = list(fill = "grey99"))))
drw <- grid.draw(mytable)
?ggsave
ggsave(path = "Documentation/plots/PAMfinal/Data1.jpeg", grid.table(drw), device = "jpeg",
       width = 5, height = 3)

png("Data1.png", height = 800, width = 1600)
grid.arrange(mytable)
grid.table(a)
dev.off()



?grid.table
class(grid.table(a))


d$time <- time
d


