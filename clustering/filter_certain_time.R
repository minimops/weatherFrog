library(tidyverse)
library(data.table)
library(checkmate)
library(factoextra)

data <- readRDS("Data/cli_data_05.rds")
data

dataS <- data %>% filter(time == 18)
   
toGeoIndex <- function(data) {
  assertDataTable(data)
  out <- copy(data)
  setorder(data, date, longitude, latitude)
  out[, geoIndex := 1:.N, by = date][, ":=" (longitude = NULL, latitude = NULL)]
}       

dataS <- toGeoIndex(copy(dataL))
dataS

longToWide <- function(data, id = "date", col = "geoIndex", vars = c("avg_mslp", "avg_geopot")) {
  assertDataTable(data)
  assertCharacter(id)
  assertCharacter(col, len = 1)
  assertCharacter(vars)
  
  dcast(data, 
        paste(paste(id, collapse = "+"), "~", col), 
        value.var = vars)
}

dataS <- longToWide(dataS, vars = c("mslp", "geopotential"))
dataS
datscaled <- scaleNweight(copy(dataS))

############ CLARA #####################
set.seed(1289)

fviz_nbclust(as.data.frame(scale(dataS[, 2:321])), FUNcluster = clara,
             method = "silhouette")
# dist.data.scaled <- dist(scale(data.wide[, 2:321]), method = "euclidean")
clusterclara.12 <- clara(scale(dataS[, 2:321]), k = 5, metric = "euclidean", 
                      stand = TRUE, samples = 1000, sampsize = 300)
summary(clusterclara.12)
?clara
cluster.vector.clara.12 <- clusterclara.12$clustering

## Measurements EUC ########### 
# 1.
sil(clusterclara.12, cluster.vector.clara.12, dist(scale(copy(dataS)[, 2:321])), "kmeans")

?manova
dat.clara.12 <- copy(dataS)[, cluster := cluster.vector.clara.12]
# 2.
Cl.timeline(copy(dat.clara.12))
# 3.
model.clara.12 <- manova(as.matrix(dat.clara.12[, 2:321]) ~ dat.clara.12$cluster)
summary(as.matrix(dat.clara.12[, 2:321]) ~ dat.clara.12$cluster, test = "Wilks")
summary.aov(model.clara.12)
# 4.
mosaic(copy(dataS), cluster.vector.clara.12, title = "CLARA WITH EUC - 18h")


######### JACCARD ##################################
clusterclara.jac.12 <- clara(scale(dataS[, 2:321]), k = 5, metric = "jaccard", 
                             stand = TRUE, samples = 1000, sampsize = 300)
summary(clusterclara.jac.12)
cluster.vector.clara.jac.12 <- clusterclara.jac.12$clustering
?clara
## Measurements EUC ########### 
# 1.
sil(clusterclara.jac.12, cluster.vector.clara.jac.12, dist(scale(copy(dataS)[, 2:321])), "kmeans")

?manova
dat.clara.jac.12 <- copy(dataS)[, cluster := cluster.vector.clara.jac.12]
# 2.
Cl.timeline(copy(dat.clara.jac.12))
# 3.
model.clara.jac.12 <- manova(as.matrix(dat.clara.jac.12[, 2:321]) ~ dat.clara.jac.12$cluster)
summary(as.matrix(dat.clara.jac.12[, 2:321]) ~ dat.clara.jac.12$cluster, test = "Wilks")
summary.aov(model.clara.jac.12)
# 4.
mosaic(copy(dataS), cluster.vector.clara.jac.12, title = "CLARA WITH JACCARD - 18h")



######### PAM #############################################
####################

sil.width.12 <- c(NA)
for(i in 4:15){
  pam_fit.12 <- pam(datscaled[, 2:321],
                 diss = FALSE,
                 k = i)
  
  sil.width.12[i-3] <- pam_fit.12$silinfo$avg.width
}
plot(4:15, sil.width.12,
     xlab = "Number of clusters",
     ylab = "Silhouette Width",)
lines(4:15, sil.width.12)

#######################################

pam.12.euc <- pam(datscaled[, 2:321], diss = FALSE, k = 5, metric = "euclidean")
## Measurement 
# 1.
sil(pam.12.euc, pam.12.euc$cluster, dist(copy(datscaled)[, 2:321]), "kmeans")
pam.12.euc$silinfo
fviz_silhouette(pam.12.euc)
?manova
dat.pam.12 <- copy(datscaled)[, cluster := pam.12.euc$clustering]
# 2.
Cl.timeline(copy(dat.pam.12))
# 3.
model.kmeans.euc <- manova(as.matrix(dat.kmeans[, 2:49]) ~ dat.kmeans$cluster)
summary(as.matrix(dat.kmeans[, 2:49]) ~ dat.kmeans$cluster, test = "Wilks")
summary.aov(model.kmeans.euc)
# 4.
mosaic(copy(datscaled), pam.12.euc$clustering, title = "PAM WITH EUC - 18h")
