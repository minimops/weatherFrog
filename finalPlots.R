
pamfinal <- readRDS("wetransfer-ed1938/PAMres.rds")
distfinal <- readRDS("wetransfer-ed1938/f_dist.rds")
datafinal <- readRDS("wetransfer-ed1938/f_data.rds")

datafinalID <- copy(datafinal)[, cluster := pamfinal$clustering]


# 1. Timeline ganz
tl <- Cl.timeline(copy(datafinalID))

ggsave(plot = tl, "documentation/plots/PAMfinal/timeline.png", device = "png",
       width = 5, height = 3)

#1.1 Timeline multiplied
tlMult <- Cl.timeline(copy(datafinalID), multiplied = TRUE)
ggsave(plot = tlMult, "documentation/plots/PAMfinal/timelineMult.png", device = "png",
       width = 5, height = 3)

# 2. Timeline cut
timelineCut <- Cl.timeline(copy(datafinalID), cut = 41)

ggsave("documentation/plots/PAMfinal/timelineCut.png", timelineCut, device = "png",
       width = 5, height = 3)


# 3. Timeline nach Cluster ganz
timelineSep <- Cl.timeline(copy(datafinalID), seperated = TRUE)
ggsave("documentation/plots/PAMfinal/timelineSep.png", timelineSep, device = "png",
       width = 10, height = 6)

# 4. Timeline nach Cluster cut
timelineSepCut <- Cl.timeline(copy(datafinalID), seperated = TRUE, cut = 40)


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



ggsave("documentation/plots/PAMfinal/timelineSepCut.png",timelineSepCut, device = "png",
       width = 10, height = 6)

