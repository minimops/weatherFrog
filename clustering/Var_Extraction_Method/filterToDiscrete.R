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
data.long <- readRDS("Data/cli_data_05_avgDay.rds")

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
mean.geopot <- geopot.wide[, apply(geopot.wide, 1, mean)]

## einfach nur abchecken, obs funktioniert hat
if (length(mean.mslp) != nrow(data.wide) | length(mean.geopot) != nrow(data.wide)) {
  stop("Da ist was schiefgegangen")
}


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
plot(density(min.mslp, na.rm = TRUE))
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
    stop("min.mslp contains NAs")
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
    stop("max.mslp contains NAs")
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

### 7. Intensität ###############################################################################

mslp.hoch <- mslp.wide[, apply(mslp.wide[, 1:160], 2, 
                               function(x) lapply(x, function(y) if (y < 102000) {y <-  NA} 
                                                  else {y <- y}))]
mslp.tief <- mslp.wide[, apply(mslp.wide[, 1:160], 2, 
                            function(x) lapply(x, function(y) if (y > 100000) {y <-  NA} 
                                               else {y <- y}))]
## intensität zeigt, wie groß die hochdruck bzw tiefdruckgebiete sind
intensitaet.hoch.mslp <- apply(mslp.hoch, 1, function(x) sum(!is.na(x)))
intensitaet.tief.mslp <- apply(mslp.tief, 1, function(x) sum(!is.na(x)))

table(intensitaet.hoch.mslp)
table(intensitaet.tief.mslp)
sum(intensitaet.hoch.mslp)
sum(intensitaet.tief.mslp)

## erst hatte ich überlegt, das 0.25 bzw 0.75 Quantil zu nehmen. das macht über die Zeilen auf jeden fall keinen sinn,
## da einfach in jeder Zeile die 40 niedrigsten Werte ausgewählt haben.
## deshalb habe ich überlegt, die quantile über die spalten, also je nach den verschiedenen Messorten zu machen
## TO DO Entscheidung ob ja/nein

#geopot.hoch <- geopot.wide[, apply(geopot.wide[, 1:160], 2, function(x) {
#                                                              quantile_0.75 <- quantile(x, probs = 0.75)
#                                                              lapply(x, function(y) 
#                                                                if (y < quantile_0.75) {
#                                                                  y <-  NA
#                                                                } 
#                                                                else {
#                                                                 y <- y
#                                                                }
#                                                              )
#                                                              })]

quantile(data.long[, avg_geopot])
mean(data.long[, avg_geopot])
## ich orientiere mich ersteinmal an den 25 und 75 prozent quartilen von geopotential insgesamt
q_0.25 <- quantile(data.long[, avg_geopot], probs = 0.25)
## -> 52570.22
q_0.75 <- quantile(data.long[, avg_geopot], probs = 0.75)
## -> 55867.28

geopot.hoch <- geopot.wide[, apply(geopot.wide[, 1:160], 2, 
                               function(x) lapply(x, function(y) if (y < q_0.75) {y <-  NA} 
                                                  else {y <- y}))]
geopot.tief <- geopot.wide[, apply(geopot.wide[, 1:160], 2, 
                               function(x) lapply(x, function(y) if (y > q_0.25) {y <-  NA} 
                                                  else {y <- y}))]

## intensität zeigt, wie groß die hochdruck bzw tiefdruckgebiete sind
intensitaet.hoch.geopot <- apply(geopot.hoch, 1, function(x) sum(!is.na(x)))
intensitaet.tief.geopot <- apply(geopot.tief, 1, function(x) sum(!is.na(x)))
table(intensitaet.hoch.geopot)
table(intensitaet.tief.geopot)
sum(intensitaet.hoch.geopot)
sum(intensitaet.tief.geopot)


### 8. Quadrant von Minimum/MAximum ##################################################################

## dafür benutze ich extra einen dataframe
coordinates.min.mslp <- data.frame(row = numeric(length = 1826), col = numeric(length = 1826))
coordinates.max.mslp <- data.frame(row = numeric(length = 1826), col = numeric(length = 1826))

## und splitte mslp nach Tag auf in eine list , die als inahlate die tageswerte im format 8x20 enthält
mslp.split <- split(data.mslp[, 3:23], data.mslp[, id])
mslp.split[[1]]
## dann entnehme ich die koordinaten von den minima und maxima
for (i in seq_len(1826)) {
  coordinates.min.mslp[i, ]<- which(mslp.split[[i]] == min(mslp.split[[i]][, 1:20], na.rm = TRUE), 
                                    arr.ind = TRUE)
}

for (i in seq_len(1826)) {
    coordinates.max.mslp[i, ]<- which(mslp.split[[i]] == max(mslp.split[[i]][, 1:20], na.rm = TRUE), 
                                      arr.ind = TRUE)
}
head(coordinates.min.mslp)
head(coordinates.max.mslp)
##   1 ...       7|8 ...    13|14  ...    20|
##  _________________________________________
## 1|1 1 1 1 1 1 1|2 2 2 2 2 2|3 3 3 3 3 3 3|
## 2|1 1 1 1 1 1 1|2 2 2 2 2 2|3 3 3 3 3 3 3|
## 3|1 1 1 1 1 1 1|2 2 2 2 2 2|3 3 3 3 3 3 3|
## 4|4 4 4 4 4 4 4|5 5 5 5 5 5|6 6 6 6 6 6 6| 
## 5|4 4 4 4 4 4 4|5 5 5 5 5 5|6 6 6 6 6 6 6|
## 6|7 7 7 7 7 7 7|8 8 8 8 8 8|9 9 9 9 9 9 9|
## 7|7 7 7 7 7 7 7|8 8 8 8 8 8|9 9 9 9 9 9 9|
## 8|7 7 7 7 7 7 7|8 8 8 8 8 8|9 9 9 9 9 9 9|

# so teil ich das jetzt ein, falls euch was anderes einfällt, gerne bescheid sagen


quadrant.min.mslp <- integer(length = 1826)
for (i in 1:1826) {
  if (is.na(coordinates.min.mslp[i, ][[1]])) {
    stop("coordinates.min.mslp contains NAs")
  }
  else if (coordinates.min.mslp[i, ][[1]] < 4) {
    if (coordinates.min.mslp[i, ][[2]] < 8) {
      quadrant.min.mslp[i] <- 1
    }
    else if (coordinates.min.mslp[i, ][[2]] < 14) {
      quadrant.min.mslp[i] <- 2
    }
    else {
      quadrant.min.mslp[i] <- 3
    }
  }
  else if (coordinates.min.mslp[i, ][[1]] < 6) {
    if (coordinates.min.mslp[i, ][[2]] < 8) {
      quadrant.min.mslp[i] <- 4
    }
    else if (coordinates.min.mslp[i, ][[2]] < 14) {
      quadrant.min.mslp[i] <- 5
    }
    else {
      quadrant.min.mslp[i] <- 6
    }
  }
  else {
    if (coordinates.min.mslp[i, ][[2]] < 8) {
      quadrant.min.mslp[i] <- 7
    }
    else if (coordinates.min.mslp[i, ][[2]] < 14) {
      quadrant.min.mslp[i] <- 8
    }
    else {
      quadrant.min.mslp[i] <- 9
    }
  }
}

table(quadrant.min.mslp)

quadrant.max.mslp <- integer(length = 1826)
for (i in 1:1826) {
  if (is.na(coordinates.max.mslp[i, ][[1]])) {
    stop("coordinates.max.mslp contains NAs")
  }
  else if (coordinates.max.mslp[i, ][[1]] < 4) {
    if (coordinates.max.mslp[i, ][[2]] < 8) {
      quadrant.max.mslp[i] <- 1
    }
    else if (coordinates.max.mslp[i, ][[2]] < 14) {
      quadrant.max.mslp[i] <- 2
    }
    else {
      quadrant.max.mslp[i] <- 3
    }
  }
  else if (coordinates.max.mslp[i, ][[1]] < 6) {
    if (coordinates.max.mslp[i, ][[2]] < 8) {
      quadrant.max.mslp[i] <- 4
    }
    else if (coordinates.max.mslp[i, ][[2]] < 14) {
      quadrant.max.mslp[i] <- 5
    }
    else {
      quadrant.max.mslp[i] <- 6
    }
  }
  else {
    if (coordinates.max.mslp[i, ][[2]] < 8) {
      quadrant.max.mslp[i] <- 7
    }
    else if (coordinates.max.mslp[i, ][[2]] < 14) {
      quadrant.max.mslp[i] <- 8
    }
    else {
      quadrant.max.mslp[i] <- 9
    }
  }
}
length(which(quadrant.max.mslp == quadrant.min.mslp))
## an 17 tagen liegt das max und min von mslp im gleichen quadranten
table(quadrant.max.mslp)

## und alles nochmal für geopotential ####################################
## dafür benutze ich extra einen dataframe
coordinates.min.geopot <- data.frame(row = numeric(length = 1826), col = numeric(length = 1826))
coordinates.max.geopot <- data.frame(row = numeric(length = 1826), col = numeric(length = 1826))

## und splitte mslp nach Tag auf in eine list , die als inahlate die tageswerte im format 8x20 enthält
geopot.split <- split(data.geopot[, 3:23], data.geopot[, id])

## dann entnehme ich die koordinaten von den minima und maxima
for (i in seq_len(1826)) {
  if (all(is.na(geopot.split[[i]][, 1:20]))){
    stop("geopot.split enthält NAs")
  } else {
    coordinates.min.geopot[i, ]<- which(geopot.split[[i]] == min(geopot.split[[i]][, 1:20], na.rm = TRUE), 
                                        arr.ind = TRUE)
  }
}

for (i in seq_len(1826)) {
  if (all(is.na(geopot.split[[i]][, 1:20]))){
    stop("geopot.split enthält NAs")
  } else {
    coordinates.max.geopot[i, ]<- which(geopot.split[[i]] == max(geopot.split[[i]][, 1:20], na.rm = TRUE), 
                                        arr.ind = TRUE)
  }
}
head(coordinates.min.geopot)
head(coordinates.max.geopot)
##   1 ...       7|8 ...    13|14  ...    20|
##  _________________________________________
## 1|1 1 1 1 1 1 1|2 2 2 2 2 2|3 3 3 3 3 3 3|
## 2|1 1 1 1 1 1 1|2 2 2 2 2 2|3 3 3 3 3 3 3|
## 3|1 1 1 1 1 1 1|2 2 2 2 2 2|3 3 3 3 3 3 3|
## 4|4 4 4 4 4 4 4|5 5 5 5 5 5|6 6 6 6 6 6 6| 
## 5|4 4 4 4 4 4 4|5 5 5 5 5 5|6 6 6 6 6 6 6|
## 6|7 7 7 7 7 7 7|8 8 8 8 8 8|9 9 9 9 9 9 9|
## 7|7 7 7 7 7 7 7|8 8 8 8 8 8|9 9 9 9 9 9 9|
## 8|7 7 7 7 7 7 7|8 8 8 8 8 8|9 9 9 9 9 9 9|

# so teil ich das jetzt ein, falls euch was anderes einfällt, gerne bescheid sagen


quadrant.min.geopot <- integer(length = 1826)
for (i in 1:1826) {
  if (is.na(coordinates.min.geopot[i, ][[1]])) {
    stop("coordinates.min.geopot contains NAs")
  }
  else if (coordinates.min.geopot[i, ][[1]] < 4) {
    if (coordinates.min.geopot[i, ][[2]] < 8) {
      quadrant.min.geopot[i] <- 1
    }
    else if (coordinates.min.geopot[i, ][[2]] < 14) {
      quadrant.min.geopot[i] <- 2
    }
    else {
      quadrant.min.geopot[i] <- 3
    }
  }
  else if (coordinates.min.geopot[i, ][[1]] < 6) {
    if (coordinates.min.geopot[i, ][[2]] < 8) {
      quadrant.min.geopot[i] <- 4
    }
    else if (coordinates.min.geopot[i, ][[2]] < 14) {
      quadrant.min.geopot[i] <- 5
    }
    else {
      quadrant.min.geopot[i] <- 6
    }
  }
  else {
    if (coordinates.min.geopot[i, ][[2]] < 8) {
      quadrant.min.geopot[i] <- 7
    }
    else if (coordinates.min.geopot[i, ][[2]] < 14) {
      quadrant.min.geopot[i] <- 8
    }
    else {
      quadrant.min.geopot[i] <- 9
    }
  }
}

table(quadrant.min.geopot)

quadrant.max.geopot <- integer(length = 1826)
for (i in 1:1826) {
  if (is.na(coordinates.max.geopot[i, ][[1]])) {
    stop("coordinates.max.geopot contains NAs")
  }
  else if (coordinates.max.geopot[i, ][[1]] < 4) {
    if (coordinates.max.geopot[i, ][[2]] < 8) {
      quadrant.max.geopot[i] <- 1
    }
    else if (coordinates.max.geopot[i, ][[2]] < 14) {
      quadrant.max.geopot[i] <- 2
    }
    else {
      quadrant.max.geopot[i] <- 3
    }
  }
  else if (coordinates.max.geopot[i, ][[1]] < 6) {
    if (coordinates.max.geopot[i, ][[2]] < 8) {
      quadrant.max.geopot[i] <- 4
    }
    else if (coordinates.max.geopot[i, ][[2]] < 14) {
      quadrant.max.geopot[i] <- 5
    }
    else {
      quadrant.max.geopot[i] <- 6
    }
  }
  else {
    if (coordinates.max.geopot[i, ][[2]] < 8) {
      quadrant.max.geopot[i] <- 7
    }
    else if (coordinates.max.geopot[i, ][[2]] < 14) {
      quadrant.max.geopot[i] <- 8
    }
    else {
      quadrant.max.geopot[i] <- 9
    }
  }
}
length(which(quadrant.max.geopot == quadrant.min.geopot))
## an 4 tagen liegt das max und min von mslp im gleichen quadranten
table(quadrant.max.geopot)



# brauchen wir wahrscheinlich nicht, aber ich nehme mal das jahr, monat und tag als einzelne variablen mit 
# in den datensatz

year <- as.numeric(format(data.wide$date,"%Y"))
month <- as.numeric(format(data.wide$date, "%m"))
day <- as.numeric(format(data.wide$date, "%d"))
discrete <- data.table(date, day, month, year, min.mslp, intensitaet.tief.mslp, quadrant.min.mslp, 
                       max.mslp, intensitaet.hoch.mslp, quadrant.max.mslp, mean.mslp, median.mslp,
                       range.mslp, min.geopot, intensitaet.hoch.geopot, quadrant.min.geopot, max.geopot, 
                       intensitaet.tief.geopot, quadrant.max.geopot,
                       mean.geopot, median.geopot, range.geopot)


# quadrant als factor variable damit das beim clustern als ordinal gewertet wird
discrete$quadrant.min.mslp <- as.factor(as.character(discrete$quadrant.min.mslp))
discrete$quadrant.max.mslp <- as.factor(as.character(discrete$quadrant.max.mslp))
discrete$quadrant.min.geopot <- as.factor(as.character(discrete$quadrant.min.geopot))
discrete$quadrant.max.geopot <- as.factor(as.character(discrete$quadrant.max.geopot))
discrete$month <- as.factor(as.character(discrete$month))


### 9.Euklidische Distanz zwischen hoch und tief ########################################################
discrete[, c("y.max.mslp", "x.max.mslp") := .(coordinates.max.mslp[, 1], coordinates.max.mslp[, 2])]
discrete[, c("y.min.mslp", "x.min.mslp") := .(coordinates.min.mslp[, 1], coordinates.min.mslp[, 2])]
discrete <- discrete[, euclidean.mslp := sqrt((x.max.mslp - x.min.mslp)^2 + (y.max.mslp - y.min.mslp)^2)]
any(is.na(discrete[, euclidean.mslp]))
# keine NAs

discrete[, c("y.max.geopot", "x.max.geopot") := .(coordinates.max.geopot[, 1], coordinates.max.geopot[, 2])]
discrete[, c("y.min.geopot", "x.min.geopot") := .(coordinates.min.geopot[, 1], coordinates.min.geopot[, 2])]
discrete <- discrete[, euclidean.geopot := sqrt((x.max.geopot - x.min.geopot)^2 + (y.max.geopot - y.min.geopot)^2)]
any(is.na(discrete[, euclidean.geopot]))



### 10. Datensatz speichern #############################################################################

saveRDS(discrete, "Data/discrete.rds")
discrete <- readRDS("Data/discrete.rds")
discrete


