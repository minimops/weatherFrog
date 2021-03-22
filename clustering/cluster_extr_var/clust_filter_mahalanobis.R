
data.selected <- copy(data)[, date := NULL]
data.selected <- data.selected[, c(1:6, 8:15, 17:38, 40:47)]

datascaled <- scale(copy(data.selected))
colnames(datascaled)


variance <- var(datascaled)
solve(variance)

# Anzahl der Iterationen
n <- nrow(datascaled)

?mahalanobis

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
?manova
dat.mahal <- copy(data)[, cluster := cluster.mahal]
# 2.
Cl.timeline(copy(dat.mahal))
# 3.
model.pam.mahal <- manova(as.matrix(dat.mahal[, 2:49]) ~ dat.mahal$cluster)
summary(as.matrix(dat.mahal[, 2:49]) ~ dat.mahal$cluster, test = "Wilks")
summary.aov(model.pam.mahal)
# 4.
mosaic(copy(data), cluster.mahal, title = "PAM WITH MAHALANOBIS")







