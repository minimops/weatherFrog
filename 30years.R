data <- extrapolate(seq(1971, 2000))

scaled <- scaleNweight(copy(data))
diss.pam.manhat.30.all <- dissimilarityPAM(scaleNweight(copy(data)),
                                       dist = FALSE, metric = "manhattan")


####### PAM with Manhattan ##################
diss.pam.manhat.30.all <- daisy(scaled[, 2:ncol(scaled)], metric = "manhattan", 
                                weights = c(rep(c(1/9, 1/9, 1/6, 1/6, 1/18, 1/18, 1/9, 1/9, 1/9), 2), 
                                            rep(1/6, 12)))
?daisy
pam.manhat.30 <- pam(diss.pam.manhat.30.all, diss = TRUE, k = 6)
cluster.manhat.30 <- pam.manhat.30$clustering
mosaic(copy(data), cluster.manhat.30, title = "PAM WITH MANHATTAN")
# sil_width: 0.149

## Measurement 
# 1.
sil(pam.manhat.30, cluster.manhat.30, diss.pam.manhat.30, "pam")
?manova
dat.manhat.30 <- copy(data)[, cluster := cluster.manhat.30]
# 2.
Cl.timeline(copy(dat.manhat.30))
# 3.

# 4.
mosaic(copy(data), cluster.manhat.30, title = "PAM WITH MANHAT - 30 YEARS")

saveRDS(diss.pam.manhat.30.all, "Data/diss.pam.manhat.30.all.rds")
saveRDS(diss.pam.manhat.30, "Data/diss.pam.manhat.30.rds")
class(diss.pam.manhat.30)
dist <- readRDS("Data/diss.pam.manhat.30.rds")
class(dist)




