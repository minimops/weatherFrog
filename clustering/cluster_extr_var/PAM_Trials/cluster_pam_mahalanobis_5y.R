# mahalanobis with PAM TRYOUT

source("clustering/cluster_extr_var/f_extr_funs.R")

data <- extrapolate(yearspan = seq(2006, 2010), vars = "all")

data.selected <- copy(data)[, date := NULL]
# auswählen, da manche variable kollinear sind
data.selected <- data.selected[, c(1:6, 8:15, 17:38, 40:47)]

datascaled <- scale(copy(data.selected))
colnames(datascaled)


variance <- var(datascaled)
n <- nrow(datascaled)

dist.mahal <- matrix(NA, nrow = n, ncol = n)

# Mahalanobisdistanz berechnen und in Distanzmatrix abspeichern
for(i in seq_len(n)){
  dist.mahal[i,] <- mahalanobis(datascaled, datascaled[i,], variance)
}


sil.width.maha <- c(NA)
for(i in 4:15){
  pam.fit <- pam(dist.mahal,
                 diss = TRUE,
                 k = i)
  
  sil.width.maha[i-3] <- pam.fit$silinfo$avg.width
}
plot(4:15, sil.width.maha,
     xlab = "Number of clusters",
     ylab = "Silhouette Width",)
lines(4:15, sil.width.maha)

pam.mahalanobis <- pam(as.dist(dist.mahal), diss = TRUE, k = 10)
cluster.mahal <- pam.mahalanobis$clustering

## Measurement 
# 1.
sil(pam.mahalanobis, cluster.mahal, as.dist(dist.mahal), "pam")

dat.mahal <- copy(data)[, cluster := cluster.mahal]
# 2.
Cl.timeline(copy(dat.mahal))

# 3.
mosaic(copy(data), cluster.mahal, title = "PAM WITH MAHALANOBIS")







