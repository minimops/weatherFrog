
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

ggsave("documentation/plots/PAMfinal/timelineSepCut.png",timelineSepCut, device = "png",
       width = 10, height = 6)
