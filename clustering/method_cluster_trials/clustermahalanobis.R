## Cluster mit Mahalanobisdistanz mit 320 Dimensionen

## Pakete laden
library(data.table)
library(ggplot2)
library(ggmosaic)
library(factoextra)
library(ggfortify)
library(cluster)

## erstmal einlesen
data.wide <- readRDS("Data/cli_data_05_avgDay_wide.rds")
gwl <- readRDS("Data/gwl.rds")

#### Variaben skalieren
data.scaled <- scale(data.wide[, 2:321], scale=TRUE, center=TRUE)

#### Distanzmatrix mit NA erstellen
dist_mahal_scaled <- matrix(NA, nrow = n, ncol = n)

## Mahalanobisdistanz berechnen und in Distanzmatrix abspeichern
#for(i in seq_len(n)){
#  dist_mahal_scaled[i,] <- mahalanobis(data.scaled, data.scaled[i,], variance)
#}

rownames(dist_mahal_scaled) <- colnames(dist_mahal_scaled) <- rn <- 1:1826

# dist_mahal_scaled <- as.dist(dist_mahal)
#saveRDS(dist_mahal_scaled, "Data/dist_mahal_scaled.rds")

dist_mahal_scaled <- readRDS("Data/dist_mahal_scaled.rds")

## erster Clusterversuch, hierarchisch
complete_linkage <- hclust(as.dist(dist_mahal_scaled), method="complete")
summary(complete_linkage)
plot(complete_linkage)


## zweiter Clusterversuch, hierarchisch
clust <- hcut(as.dist(dist_mahal_scaled), k = 9, hc_func = "hclust", hc_method = "complete")
plot(clust)

### PAM mit Mahalanobis
sil_width <- c(NA)
for(i in 5:9){
  pam_fit <- pam(as.dist(dist_mahal_scaled),
                 diss = TRUE,
                 k = i)
  
  sil_width[i-4] <- pam_fit$silinfo$avg.width
}
plot(5:9, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width",)
lines(5:9, sil_width)

clusterpam.mahal <- pam(as.dist(dist_mahal_scaled), k = 6, diss = TRUE)
clusterpam.mahal.vector <- clusterpam.mahal$clustering


## Measurement 
# 1. Silhouette
sil(clusterpam.mahal, clusterpam.mahal$clustering, as.dist(dist_mahal_scaled), "pam")
### s = -0.0014021

# 2. Timeline
dat.pam.mahal <- copy(data.wide)[, cluster := clusterpam.mahal$clustering]
Cl.timeline(copy(dat.pam.mahal))
### TLS = -0.665745

# 3. Mosaikplot
mosaic(copy(data.wide), clusterpam.mahal.vector, title = "PAM WITH MAHAL")
### HB_diff = 0.4126577


cor(data.wide[, 2:321])
