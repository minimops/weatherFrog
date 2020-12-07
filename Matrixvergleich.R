#### Versuch mit 8x20 Matrizen

library(data.table)

data.all <- readRDS("Data/cli_data_05_wide.rds")

# Ich versuche es erst einmal nur mit mslp
data.mslp <- readRDS("Data/cli_data_05_mslp_wide.rds")

# die data.wide nehm ich für die Spalten und Zeilen Namen
data.wide <- readRDS("Data/cli_data_05_avgDay_wide.rds")
# hier werden die Tage gespeichert.
names <- data.wide$date
class(as.character(names))
dim(data.all)[[1]] / 8
# es sind insgesamt 1826 Tage 

# das hier ist die Distanzmatrix, die anfangs noch mit Nas gefüllt ist
matrix.dist <- matrix(NA, nrow = 1826, ncol = 1826)

# Hier wird eine id Spalte hinzugefügt, die von 1-1826 geht. Dabei stehen die Zahlen für die jeweiligen Tage
data.mslp[, id := rep(1:1826, each = 8)]

# das hier ist die Funktion, die die Norm von zwei voneinander subtrahierten Matrizen ist. Ich habe hier 
# versuchsweise mal die 1er Norm genommen.
normmat <- function(mat1, mat2) {
  distance <- abs(mat1 - mat2)
  norm(as.matrix(distance), type = "O")
}



## datensatz teilen, ist jetzt also eine Liste mit 1826 Einträgen, also eine Matrix pro Tag
mslp.split <- split(data.mslp, data.mslp[, id])
class(mslp.split)
mslp.split[[1826]]
# Liegt noch in der Dimension, aber
mslp.split[[1827]]
# nicht mehr, sollte also richtig gesplittet sein


# Hier wird alles ausgrechnet und in die Distanzmatrix eingetragen. Es geht garantiert effizienter, das hab ich aber 
# noch nicht geschafft.. vielleicht mapply... achtung das hier dauert ca. 2 Stunden!!

for (j in 1:1826) {
  for (i in j:1826) {
    matrix.dist[i, j] <- normmat(mslp.split[[j]][, 3:22], mslp.split[[i]][, 3:22])
  }
  
}
colnames(matrix.dist) <- as.character(names)
rownames(matrix.dist) <- as.character(names)
saveRDS(matrix.dist, "Data/matrix_dist.rds")




## und nochmal für die euklidische
normmat_2 <- function(mat1, mat2) {
  distance <- abs(mat1 - mat2)
  norm(as.matrix(distance), type = "2")
}

matrix.dist_2 <- matrix(NA, nrow = 1826, ncol = 1826)

for (j in 1:1826) {
  for (i in j:1826) {
    matrix.dist_2[i, j] <- normmat_2(mslp.split[[j]][, 3:22], mslp.split[[i]][, 3:22])
  }
}

saveRDS(matrix.dist_2, "Data/matrix_dist_eukl.rds")
matrix.dist_2 <- readRDS("data/matrix_dist_eukl.rds")
matrix.dist <- readRDS("data/matrix_dist.rds")

# zeilen und spalten namen ändern
colnames(matrix.dist_2) <- as.character(names)
rownames(matrix.dist_2) <- as.character(names)


?norm
# mit as.dist() ist es eine richtige Distanzmatrix
class(as.dist(matrix.dist_2))


### einmal ohne distanzmatrix und clara versuchen
library(cluster)
?clara
a <- clara(data.wide[, 2:321], k = 9,metric = "euclidean")
plot(a)


# hclust mit clusterabstand ward 
hclust <- hclust(as.dist(matrix.dist_2), method = "ward.D2")
plot(hclust)


## hab einfahc mal 9 genommen, muss man da auch einen elbow plot machen ?
clusters <- cutree(hclust, k = 9)

plot(clusters, col = colorspace::diverge_hsv(9))
library(sf)
install.packages("motif", type = "source")
library(motif)
# es findet leider kein Paket motif... obwohl ich R geupdatet habe..
library(stars)
library(abind)


climate_cove <-  lsp_signature(data.mslp, type = "cove",
                               window = 100, normalization = "pdf")
grid <- lsp_add_clusters()


##### ich habe mich hierbei an der wesite spatial clustering orientiert, die Noah mal reingeschickt hat.
# Aber das clustert zwar räumlic aber nur zu einem zeitpunkt... deshalb verstehe ich hier grade 
# nicht ganz, inwiefern man sich an die website halten kann..


library(factoextra)

distance.test <- readRDS("Data/cli_data_05_avgDay_wide.rds")
distance.test <- scale(data.frame(distance.test)[, -1])[1:500, ]
dist.eucl <- dist(distance.test, method = "euclidean")
fviz_dist(dist.eucl)
fviz_dist(as.dist(matrix_dist_eukl[1:500, 1:500]))



# dist.mat500 <- as.matrix(dist(distance.test))[1:500, 1:500]
# dist.over <- 1/dist.mat500 # one over, as qgraph takes similarity matrices as input
# library(qgraph)
# jpeg('example_forcedraw.jpg', width=1000, height=1000, unit='px')
# qgraph(dist.over, layout='spring', vsize=3)
# dev.off()
