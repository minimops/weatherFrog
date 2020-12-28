### FilterToDiscrete #######################################################################

## Diskrete Merkmale aus Datensatz ziehen

## Laden der Pakete
library(data.table)


## Einlesen der Dateien
data.mslp <- readRDS("Data/cli_data_05_mslp_wide.rds")
data.geopot <- readRDS("Data/cli_data_05_geo_wide.rds")
data.wide <- readRDS("Data/cli_data_05_avgDay_wide.rds")
gwl <- readRDS("Data/gwl.rds")
date <- data.wide[, .(date)]

# hier wird wieder eine id Spalte für den Tag gemacht.
data.mslp[, id := rep(1:1826, each = 8)]
data.geopot[, id := rep(1:1826, each = 8)]

# hier werden die reihen nach sinkenden latitude Werten geordnet, damit man selber besser das Grid in Nord, Süd, Ost und West-
# Ausrichtung vor Augen hat.
data.mslp <- data.mslp[order(id, -latitude, decreasing = FALSE)]
data.geopot <- data.geopot[order(id, -latitude, decreasing = FALSE)]



### 1. Mittelwert extrahieren ################################################################################

# dafür benutze ich den wide Datensatz, da dort jeder Tag in einer Zeile ist.
# ersteinmal extrahiere ich mslp aus dem wide Datensatz und bilde dann für jede Zeile den Mittelwert
mslp.wide <- data.wide[, 2:161]
mean.mslp <- mslp.wide[, apply(mslp.wide, 1, mean)]


# und hier nochmal das gleiche für das geopotential
geopot.wide <- data.wide[, 162:321]
mean.geopot2 <- geopot.wide[, apply(geopot.wide, 1, mean)]

## einfach nur abchecken, obs funktioniert hat
if (length(mean.mslp) != nrow(data.wide) | length(mean.geopot) != nrow(data.wide)) {
  stop("Da ist was schiefgegangen")
}


### das hier ist einfach nur kurz zur kontrolle
mean(apply(data.mslp[1:8, 3:22], 2, mean))

### 2. Median ausrechnen #####################################################################################

median.mslp <- apply(mslp.wide[, 1:160], 1, median)
length(median.mslp)

median.geopot <- apply(geopot.wide[, 1:160], 1, median)
length(median.mslp)

### 3. Maximum ###############################################################################################

max.mslp <- apply(mslp.wide[, 1:160], 1, max)
max.geopot <- apply(geopot.wide[, 1:160], 1, max)


### 4.Minimum ################################################################################################

min.mslp <- apply(mslp.wide[, 1:160], 1, min)
min.geopot <- apply(geopot.wide[, 1:160], 1, min)


## einmal max und min geplottet, man erkennt deutlich saisonale unterschiede
plot(min.mslp)
hist(min.mslp)
plot(density(minimum, na.rm = TRUE))
plot(max.mslp)
hist(max.mslp)
plot(density(max.mslp))


### 5. Range der Parameter

range.mslp <- max.mslp - min.mslp
range.geopot <- max.geopot - min.geopot

# Der Luftdruck hat am Erdboden einen Normalwert von 1013,25 hPa = 101325 Pa. Ein Hochdruckgebiet besitzt damit 
# einen darüber liegenden Luftdruck.
# In Mitteleuropa liegt der Druck eines Tiefs für gewöhnlich bei 990 - 1000 hPa 
# und in Orkantiefs bei 950 - 970 hPa.
## das sind die tief und hochdruck gebiete... dadurch dass es eine 8x20 matrix ist, kann man gut sehen wo auf der karte sich
# die gebiete ungefähr befinden


### 6. Kategorisierung der Stärke des Tief/Hochpunkts #######################################################
## hier kategorisiere ich die mins und maxs, aber ich glaube das brauchen wir am ende gar nicht..
tiefpunkt <- numeric(length = 1826)
hochpunkt <- numeric(length = 1826)


for (i in seq_len(1826)) {
  if (is.na(min.mslp[i])) {
    tiefpunkt[i] <- NA
  } else if (min.mslp[i] < 97000) {
    tiefpunkt[i] <- 3
  } else if (min.mslp[i] < 98500) {
    tiefpunkt[i] <- 2
  } else {
    tiefpunkt[i] <- 1
  }
}

for (i in seq_len(1826)) {
  if (is.na(max.mslp[i])) {
    hochpunkt[i] <- NA
  } else if (max.mslp[i] < 103000) {
    hochpunkt[i] <- 1
  } else if (max.mslp[i] < 104000) {
    hochpunkt[i] <- 2
  } else {
    hochpunkt[i] <- 3
  }
}

# an 145 tagen gibt es kein tiefdruckgebiet
sum(is.na(min.mslp))
table(tiefpunkt)
table(hochpunkt)
# 0 entspricht also keinem Tiefpunkt falls wirs wirklich diskret machen,
# ansonsten würde ich das minimum lassen
## tief und hochpunkt entspricht nicht der anzahl der hoch oder tiefpunkte, sondern
# eine art faktor variable

### 7. Intensität von mslp ###############################################################################
mslp.hoch <- mslp.wide[, apply(mslp.wide[, 1:160], 2, 
                               function(x) lapply(x, function(y) if (y < 102000) {y <-  NA} 
                                                  else {y <- y}))]
mslp.tief <- mslp.wide[, apply(mslp.wide[, 1:160], 2, 
                            function(x) lapply(x, function(y) if (y > 100000) {y <-  NA} 
                                               else {y <- y}))]
## intensität zeigt, wie groß die hochdruck bzw tiefdruckgebiete sind
intensitaet.hoch <- apply(mslp.hoch, 1, function(x) sum(!is.na(x)))
intensitaet.tief <- apply(mslp.tief, 1, function(x) sum(!is.na(x)))


### 8. Quadrant von Minimum/MAximum ##################################################################

## dafür benutze ich extra einen dataframe
coordinates.min <- data.frame(row = numeric(length = 1826), col = numeric(length = 1826))
coordinates.max2 <- data.frame(row = numeric(length = 1826), col = numeric(length = 1826))

## und splitte mslp nach Tag auf in eine list , die als inahlate die tageswerte im format 8x20 enthält
mslp.split <- split(data.mslp[, 3:23], data.mslp[, id])

## dann entnehme ich die koordinaten von den minima und maxima
for (i in seq_len(1826)) {
  if (all(is.na(mslp.split[[i]][, 1:20]))){
    coordinates.min[i, ] <- NA
  } else {
  coordinates.min[i, ]<- which(mslp.split[[i]] == min(mslp.split[[i]][, 1:20], na.rm = TRUE), arr.ind = TRUE)
  }
}

for (i in seq_len(1826)) {
  if (all(is.na(mslp.split[[i]][, 1:20]))){
    coordinates.max[i, ] <- NA
  } else {
    coordinates.max[i, ]<- which(mslp.split[[i]] == max(mslp.split[[i]][, 1:20], na.rm = TRUE), arr.ind = TRUE)
  }
}
head(coordinates.min)
head(coordinates.max)
##   1 ...     4|5 ...     8|9  ...   12|13 ...   16|17...    20|
##  ____________________________________________________________
## 1|1  1  1  1 | 2  2  2  2| 3  3  3  3| 4  4  4  4|5  5  5  5 |
## 2|1  1  1  1 | 2  2  2  2| 3  3  3  3| 4  4  4  4|5  5  5  5 |
## 3|6  6  6  6 | 7  7  7  7| 8  8  8  8| 9  9  9  9|10 10 10 10|
## 4|6  6  6  6 | 7  7  7  7| 8  8  8  8| 9  9  9  9|10 10 10 10|
## 5|11 11 11 11|12 12 12 12|13 13 13 13|14 14 14 14|15 15 15 15|
## 6|11 11 11 11|12 12 12 12|13 13 13 13|14 14 14 14|15 15 15 15|
## 7|16 16 16 16|17 17 17 17|18 18 18 18|19 19 19 19|20 20 20 20|
## 8|16 16 16 16|17 17 17 17|18 18 18 18|19 19 19 19|20 20 20 20|

# so teil ich das jetzt ein, falls euch was anderes einfällt, gerne bescheid sagen
# 0 wenn kein minimum oder maximum besteht

quadrant.min <- integer(length = 1826)
for (i in 1:1826) {
  if (is.na(coordinates.min[i, ][[1]])) {
    quadrant.min[i] <- 0
  }
  else if (coordinates.min[i, ][[1]] < 3) {
    if (coordinates.min[i, ][[2]] < 5) {
      quadrant.min[i] <- 1
    }
    else if (coordinates.min[i, ][[2]] < 9) {
      quadrant.min[i] <- 2
    }
    else if (coordinates.min[i, ][[2]] < 13) {
      quadrant.min[i] <- 3
    }
    else if (coordinates.min[i, ][[2]] < 17){
      quadrant.min[i] <- 4
    }
    else {
      quadrant.min[i] <- 5
    }
  }
  else if (coordinates.min[i, ][[1]] < 5) {
    if (coordinates.min[i, ][[2]] < 5) {
      quadrant.min[i] <- 6
    }
    else if (coordinates.min[i, ][[2]] < 9) {
      quadrant.min[i] <- 7
    }
    else if (coordinates.min[i, ][[2]] < 13) {
      quadrant.min[i] <- 8
    }
    else if (coordinates.min[i, ][[2]] < 17) {
      quadrant.min[i] <- 9
    }
    else {
      quadrant.min[i] <- 10
    }
  }
  else if (coordinates.min[i, ][[1]] < 7) {
    if (coordinates.min[i, ][[2]] < 5) {
      quadrant.min[i] <- 11
    }
    else if (coordinates.min[i, ][[2]] < 9) {
      quadrant.min[i] <- 12
    }
    else if (coordinates.min[i, ][[2]] < 13) {
      quadrant.min[i] <- 13
    }
    else if (coordinates.min[i, ][[2]] < 17) {
      quadrant.min[i] <- 14
    }
    else {
      quadrant.min[i] <- 15
    }
  }
  else {
    if (coordinates.min[i, ][[2]] < 5) {
      quadrant.min[i] <- 16
    }
    else if (coordinates.min[i, ][[2]] < 9) {
      quadrant.min[i] <- 17
    }
    else if (coordinates.min[i, ][[2]] < 13) {
      quadrant.min[i] <- 18
    }
    else if (coordinates.min[i, ][[2]] < 17) {
      quadrant.min[i] <- 19
    }
    else {
      quadrant.min[i] <- 20
    }
  }
}

table(quadrant.min)
quadrant.max <- integer(length = 1826)
for (i in 1:1826) {
  if (is.na(coordinates.max[i, ][[1]])) {
    quadrant.max[i] <- 0
  }
  else if (coordinates.max[i, ][[1]] < 3) {
    if (coordinates.max[i, ][[2]] < 5) {
      quadrant.max[i] <- 1
    }
    else if (coordinates.max[i, ][[2]] < 9) {
      quadrant.max[i] <- 2
    }
    else if (coordinates.max[i, ][[2]] < 13) {
      quadrant.max[i] <- 3
    }
    else if (coordinates.max[i, ][[2]] < 17){
      quadrant.max[i] <- 4
    }
    else {
      quadrant.max[i] <- 5
    }
  }
  else if (coordinates.max[i, ][[1]] < 5) {
    if (coordinates.max[i, ][[2]] < 5) {
      quadrant.max[i] <- 6
    }
    else if (coordinates.max[i, ][[2]] < 9) {
      quadrant.max[i] <- 7
    }
    else if (coordinates.max[i, ][[2]] < 13) {
      quadrant.max[i] <- 8
    }
    else if (coordinates.max[i, ][[2]] < 17) {
      quadrant.max[i] <- 9
    }
    else {
      quadrant.max[i] <- 10
    }
  }
  else if (coordinates.max[i, ][[1]] < 7) {
    if (coordinates.max[i, ][[2]] < 5) {
      quadrant.max[i] <- 11
    }
    else if (coordinates.max[i, ][[2]] < 9) {
      quadrant.max[i] <- 12
    }
    else if (coordinates.max[i, ][[2]] < 13) {
      quadrant.max[i] <- 13
    }
    else if (coordinates.max[i, ][[2]] < 17) {
      quadrant.max[i] <- 14
    }
    else {
      quadrant.max[i] <- 15
    }
  }
  else {
    if (coordinates.max[i, ][[2]] < 5) {
      quadrant.max[i] <- 16
    }
    else if (coordinates.max[i, ][[2]] < 9) {
      quadrant.max[i] <- 17
    }
    else if (coordinates.max[i, ][[2]] < 13) {
      quadrant.max[i] <- 18
    }
    else if (coordinates.max[i, ][[2]] < 17) {
      quadrant.max[i] <- 19
    }
    else {
      quadrant.max[i] <- 20
    }
  }
}
which(quadrant.max == quadrant.min)
## an keinem tag liegt das max und min von mslp im gleichen quadranten
table(quadrant.max)


# brauchen wir wahrscheinlich nicht, aber ich nehme mal das jahr, monat und tag als einzelne variablen mit 
# in den datensatz

year <- as.numeric(format(data.wide$date,"%Y"))
month <- as.numeric(format(data.wide$date, "%m"))
day <- as.numeric(format(data.wide$date, "%d"))
discrete <- data.table(date, day, month, year, min.mslp, intensitaet.tief, quadrant.min, tiefpunkt, 
                       max.mslp, intensitaet.hoch, quadrant.max, hochpunkt, mean.mslp, median.mslp, 
                       mean.geopot, median.geopot, min.geopot, max.geopot, range.mslp, range.geopot)


# quadrant als factor variable damit das beim clustern als ordinal gewertet wird
discrete$quadrant.min <- as.factor(as.character(discrete$quadrant.min))
discrete$quadrant.max <- as.factor(as.character(discrete$quadrant.max))
discrete$month <- as.factor(as.character(discrete$month))


### 9.Euklidische Distanz zwischen hoch und tief ########################################################
discrete[, c("y.max", "x.max") := .(coordinates.max[, 1], coordinates.max[, 2])]
discrete[, c("y.min", "x.min") := .(coordinates.min[, 1], coordinates.min[, 2])]
discrete <- discrete[, euclidean := sqrt((x.max - x.min)^2 + (y.max - y.min)^2)]
any(is.na(discrete[, euclidean]))
# keine NAs


