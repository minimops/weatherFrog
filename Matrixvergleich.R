#### Versuch mit 8x20 Matrizen

library(data.table)

data.all <- readRDS("Data/cli_data_05_wide.rds")

# Ich versuche es erst einmal nur mit mslp
data.mslp <- readRDS("Data/cli_data_05_mslp_wide.rds")

# die data.wide nehm ich für die Spalten und Zeilen Namen
data.wide <- readRDS("Data/cli_data_05_avgDay_wide.rds")
# hier werden die Tage gespeichert.
names <- data.wide$date

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

saveRDS(matrix.dist, "Data/matrix_dist.rds")

mat <- readRDS("data/matrix_dist.rds")


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


