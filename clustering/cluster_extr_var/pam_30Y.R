####################################################################################
################## Clustering with 30 Years ########################################
# 1. Cluster tryouts wit PAM and different dissimilarities
# 2. Cluster with kmeans and euclidean

dist.all <- readRDS("Data/diss.pam.manhat.30.all.rds")
dist.allpca <- readRDS("Data/diss.pam.manhat.30.rds")

source("clustering/cluster_extr_var/f_extr_funs.R")
# made distances with
# data <- extrapolate(seq(1971, 2000))
# and data <- extrapolate(seq(1971, 2000), "all.pca")
# scaled <- scaleNweight(copy(data))
# diss.pam.manhat.30.all <- dissimilarityPAM(scaleNweight(copy(data)),
#                                       dist = FALSE, metric = "manhattan")
data <- extrapolate(seq(1971, 2000))
data <- extrapolate(seq(1971, 2000), "all.pca")
scaled <- scaleNweight(copy(data))
diss.pam.manhat.30.all <- dissimilarityPAM(scaleNweight(copy(data)),
                                       dist = FALSE, metric = "manhattan")

####### PAM with Manhattan ##################
diss.pam.manhat.30.all <- daisy(scaled[, 2:ncol(scaled)], metric = "manhattan")


saveRDS(diss.pam.manhat.30.all, "Data/diss.pam.manhat.30.all.rds")
diss.pam.manhat.30.all <- readRDS("Data/diss.pam.manhat.30.all.rds")

pam.manhat.30 <- pam(diss.pam.manhat.30.all, diss = TRUE, k = 6)
saveRDS(pam.manhat.30, "Data/pam.manhat.30.rds" )
pam.manhat.30 <- readRDS("Data/pam.manhat.30.rds")
cluster.manhat.30 <- pam.manhat.30$clustering

# sil_width: 0.147

## Measurement 
# 1.
sil(pam.manhat.30, cluster.manhat.30, diss.pam.manhat.30.all, "pam")

dat.manhat.30 <- copy(data)[, cluster := cluster.manhat.30]
# 2.
Cl.timeline(copy(dat.manhat.30))
# 3.
manova.fun(copy(data), cluster.manhat.30)
# 4.
mosaic(copy(data), cluster.manhat.30, title = "PAM WITH MANHAT - 30 YEARS")



####### PAM with Manhattan - weighted in daisy ##################

diss.pam.manhat.30.all.weighted <- dissimilarityPAM(scaleNweight(copy(data), weight = TRUE),
                                           dist = FALSE, metric = "manhattan")

diss.pam.manhat.30.all.weighted <- daisy(scaled[, 2:ncol(scaled)], 
                                         metric = "manhattan", 
                                         weights = c(rep(c(1/9, 1/9, 1/6, 1/6, 1/18, 1/18, 1/9, 1/9, 1/9), 2), 
                                                     rep(1/6, 12), rep(1/18, 18)))

                                         
saveRDS(diss.pam.manhat.30.all.weighted, "Data/diss.pam.manhat.30.all.weighted.rds")
diss.pam.manhat.30.all.weighted <- readRDS("Data/diss.pam.manhat.30.all.weighted.rds")
pam.manhat.30.weighted <- pam(diss.pam.manhat.30.all.weighted, diss = TRUE, k = 8)
saveRDS(pam.manhat.30.weighted, "Data/pam.manhat.30.weighted.rds")
cluster.manhat.30.weighted <- pam.manhat.30.weighted$clustering

# sil_width: 0.096

## Measurement 
# 1.
sil(pam.manhat.30.weighted, cluster.manhat.30.weighted, diss.pam.manhat.30.all.weighted, "pam")

dat.manhat.30.weighted <- copy(data)[, cluster := cluster.manhat.30.weighted]
# 2.
Cl.timeline(copy(dat.manhat.30.weighted))
# 3.
manova.manhat.30.weighted <- manova.fun(copy(data), cluster.manhat.30.weighted)
# 4.
mosaic(copy(data), cluster.manhat.30.weighted, title = "PAM WITH MANHAT - 30 YEARS")

####### PAM with Manhattan - weighted in scaleNweight ##################
diss.pam.manhat.30.all.weighted2 <- daisy(scaleNweight(copy(data), weight = TRUE)[, 2:ncol(data)], 
                                          metric = "manhattan") 

saveRDS(diss.pam.manhat.30.all.weighted, "Data/diss.pam.manhat.30.all.weighted.rds")
?daisy
pam.manhat.30.weighted2 <- pam(diss.pam.manhat.30.all.weighted2, diss = TRUE, k = 8)
cluster.manhat.30.weighted2 <- pam.manhat.30.weighted2$clustering
saveRDS(pam.manhat.30.weighted2, "Data/pam.manhat.30.weighted2.rds")
# sil_width: 0.106

## Measurement 
# 1.
sil(pam.manhat.30.weighted2, cluster.manhat.30.weighted2, diss.pam.manhat.30.all.weighted2, "pam")

dat.manhat.30.weighted2 <- copy(data)[, cluster := cluster.manhat.30.weighted2]
# 2.
Cl.timeline(copy(dat.manhat.30.weighted2), seperated = TRUE)
# 3.
manova.manhat.30.weighted2 <- manova.fun(copy(data), cluster.manhat.30.weighted2)
# 4.
mosaic(copy(data), cluster.manhat.30.weighted2, title = "PAM WITH MANHAT - 30 YEARS - weighted")

####### PAM with MANHATTAN - allpca ########################

diss.pam.manhat.30.allpca <- dissimilarityPAM(scaleNweight(copy(data)), 
                                              dist = FALSE, 
                                              metric = "manhattan",
                                              weights = c(rep(c(1/9, 1/9, 1/6, 1/6, 1/18, 1/18, 1/9, 1/9, 1/9), 2), 
                                                          rep(1/6, 12), rep(1/21, 21)))

dist.allpca <- readRDS("Data/diss.pam.manhat.30.rds")
#saveRDS(diss.pam.manhat.30, "Data/diss.pam.manhat.30.rds")

pam.manhat.30.allpca <- pam(diss.pam.manhat.30.allpca, diss = TRUE, k = 6)
saveRDS(pam.manhat.30.allpca, "Data/pam.manhat.30.allpca.rds" )
pam.manhat.30.allpca <- readRDS("Data/pam.manhat.30.allpca.rds")
cluster.manhat.30.allpca <- pam.manhat.30.allpca$clustering
# sil_width: 0.15297

## Measurement 
# 1.
sil(pam.manhat.30.allpca, cluster.manhat.30.allpca, dist.allpca, "pam")

dat.manhat.30.allpca <- copy(data)[, cluster := cluster.manhat.30.allpca]
# 2.
Cl.timeline(copy(dat.manhat.30.allpca))
# 3.
manova.fun(copy(dataPCA), cluster.manhat.30.allpca)
# 4.
mosaic(copy(data), cluster.gower.30.allpca, title = "PAM WITH MANHAT - 30 YEARS - allpca")


####### PAM with MANHATTAN - allpca - weighted ########################

diss.pam.manhat.30.allpca.weighted <- dissimilarityPAM(scaleNweight(copy(data), 
                                                                    weight = TRUE),
                                                                    weights = c(rep(c(1/9, 1/9, 1/6, 1/6, 
                                                                                  1/18, 1/18, 1/9, 1/9, 1/9), 2), 
                                                                                rep(1/6, 12), rep(1/21, 21)),
                                                       weights = NULL,
                                                       dist = FALSE, 
                                                       metric = "manhattan")

weighted <- scaleNweight(copy(data), weight = TRUE, 
                         weights = c(rep(c(1/9, 1/9, 1/6, 1/6, 1/18, 1/18, 1/9, 1/9, 1/9), 2), 
                                     rep(1/6, 12), rep(1/21, 21)))

diss.pam.manhat.30.allpca.weighted <- daisy(weighted[, 2:ncol(weighted)], metric = "manhattan")
pam.manhat.30.allpca.weighted <- pam(diss.pam.manhat.30.allpca.weighted, diss = TRUE, k = 8)

saveRDS(pam.manhat.30.allpca.weighted, "Data/pam.manhat.30.allpca.weighted.rds" )

cluster.manhat.30.allpca.weighted <- pam.manhat.30.allpca.weighted$clustering
# sil_width: 0.106

## Measurement 
# 1.
sil(pam.manhat.30.allpca.weighted, cluster.manhat.30.allpca.weighted, diss.pam.manhat.30.allpca.weighted, "pam")

dat.manhat.30.allpca.weighted <- copy(data)[, cluster := cluster.manhat.30.allpca.weighted]
# 2.
Cl.timeline(copy(dat.manhat.30.allpca.weighted))
# 4.
mosaic(copy(data), cluster.manhat.30.allpca.weighted, title = "PAM WITH MANHAT - weighted - allpca")


####### PAM with GOWER ##################
diss.pam.gower.30.all <- daisy(scaled[, 2:ncol(scaled)], metric = "gower", 
                                weights = c(rep(c(1/9, 1/9, 1/6, 1/6, 1/18, 1/18, 1/9, 1/9, 1/9), 2), 
                                            rep(1/6, 12), rep(1/18, 18)))
saveRDS(diss.pam.gower.30.all, "Data/diss.pam.gower.30.all.rds")
diss.pam.gower.30.all <- readRDS("Data/diss.pam.gower.30.all.rds")
pam.gower.30 <- pam(diss.pam.gower.30.all, diss = TRUE, k = 7)
saveRDS(pam.gower.30, "Data/pam.gower.30.rds" )


cluster.gower.30 <- pam.gower.30$clustering
mosaic(copy(data), cluster.gower.30, title = "PAM WITH GOWER")
# sil_width: 0.147

## Measurement 
# 1.
sil(pam.gower.30, cluster.gower.30, diss.pam.gower.30.all, "pam")
?manova
dat.gower.30 <- copy(data)[, cluster := cluster.gower.30]
# 2.
Cl.timeline(copy(dat.gower.30))
# 3.

# 4.
mosaic(copy(data), cluster.gower.30, title = "PAM WITH GOWER - 30 YEARS")

diss.pam.gower.30.all <- dissimilarityPAM(scaleNweight(copy(data)),
                                           dist = FALSE, metric = "gower")





dist.all <- readRDS("Data/diss.pam.manhat.30.all.rds")
dist.allpca <- readRDS("Data/diss.pam.manhat.30.rds")


####### PAM with GOWER - ohne gewichte ##################
diss.pam.gower.30.all.noweights <- daisy(scaled[, 2:ncol(scaled)], metric = "gower") 
                              
saveRDS(diss.pam.gower.30.all, "Data/diss.pam.gower.30.all.rds")
diss.pam.gower.30.all <- readRDS("Data/diss.pam.gower.30.all.rds")
pam.gower.30.noweights <- pam(diss.pam.gower.30.all.noweights, diss = TRUE, k = 7)
cluster.gower.30.noweights <- pam.gower.30.noweights$clustering
mosaic(copy(data), cluster.gower.30, title = "PAM WITH GOWER")
# sil_width: 0.147

## Measurement 
# 1.
sil(pam.gower.30.noweights, cluster.gower.30.noweights, diss.pam.gower.30.all.noweights, "pam")
?manova
dat.gower.30.noweights <- copy(data)[, cluster := cluster.gower.30]
# 2.
Cl.timeline(copy(dat.gower.30))
# 3.

# 4.
mosaic(copy(data), cluster.gower.30, title = "PAM WITH GOWER - 30 YEARS")

diss.pam.gower.30.all <- dissimilarityPAM(scaleNweight(copy(data)),
                                          dist = FALSE, metric = "gower")


####### PAM with GOWER + PCA ##################

data <- extrapolate(seq(1971, 2000), "all.pca")
scaled <- scaleNweight(copy(data))

diss.pam.gower.30.allpca <- dissimilarityPAM(scaleNweight(copy(data)), 
                                              weights = c(rep(c(1/9, 1/9, 1/6, 1/6, 1/18, 1/18, 1/9, 1/9, 1/9), 2), 
                                                          rep(1/6, 12), rep(1/21, 21)),
                                              dist = FALSE, metric = "gower")

diss.pam.gower.30.allpca <- daisy(scaled[, 2:ncol(scaled)], metric = "gower", 
                               weights = c(rep(c(1/9, 1/9, 1/6, 1/6, 1/18, 1/18, 1/9, 1/9, 1/9), 2), 
                                           rep(1/6, 12), rep(1/21, 21)))

saveRDS(diss.pam.gower.30.allpca, "Data/diss.pam.gower.30.allpca.rds")
pam.gower.30.allpca <- pam(diss.pam.gower.30.allpca, diss = TRUE, k = 6)
cluster.gower.30.allpca <- pam.gower.30.allpca$clustering

# sil_width: 0.158

## Measurement 
# 1.
sil(pam.gower.30.allpca, cluster.gower.30.allpca, diss.pam.gower.30.allpca, "pam")
?manova
dat.gower.30.allpca <- copy(data)[, cluster := cluster.gower.30.allpca]
# 2.
Cl.timeline(copy(dat.gower.30.allpca))
# 4.
mosaic(copy(data), cluster.gower.30.allpca, title = "GOWER - 30 YEARS - allpca")


### 3. try EUCLIDEAN 
diss.pam.euc <- dissimilarityPAM(scaleNweight(copy(data)), 
                                 weights = NULL, 
                                 metric = "euclidean", 
                                 dist = FALSE)
saveRDS(diss.pam.euc, "Data/diss.pam.euc.rds")


## sil von 0.09 bei 5 Clustern


############################### Nicht extrahierte Daten ##################################################

data.wide <- readRDS("Data/cli_data_30_avgDay_wide.rds")

fviz_nbclust(as.data.frame(scale(data.wide[, 2:321])), FUNcluster = clara,
             method = "silhouette")

set.seed(1289)
# dist.data.scaled <- dist(scale(data.wide[, 2:321]), method = "euclidean")
clusterclara.30 <- clara(scale(data.wide[, 2:321]), k = 6, metric = "euclidean", 
                      stand = TRUE, samples = 1000, sampsize = 300)
summary(clusterclara.30)
?clara
cluster.vector.clara.30 <- clusterclara.30$clustering
?clara
## Measurements EUC ########### 
# 1.
sil(clusterclara.30, cluster.vector.clara.30, dist(scale(copy(data.wide)[, 2:321])), "kmeans")

?manova
dat.clara.30 <- copy(data.wide)[, cluster := cluster.vector.clara.30]
# 2.
Cl.timeline(copy(dat.clara.30))
# 3.
model.clara <- manova(as.matrix(dat.clara.30[, 2:321]) ~ dat.clara.30$cluster)
summary(as.matrix(dat.clara.30[, 2:321]) ~ dat.clara.30$cluster, test = "Wilks")
summary.aov(model.clara)
# 4.
mosaic(copy(data.wide), cluster.vector.clara.30, title = "CLARA WITH EUC - 30 years")




