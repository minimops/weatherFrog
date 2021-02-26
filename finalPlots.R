
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