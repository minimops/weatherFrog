library(data.table)
data.mslp <- readRDS("Data/cli_data_05_mslp_wide.rds")
data.geopot <- readRDS("Data/cli_data_05_geo_wide.rds")
data.wide <- readRDS("Data/cli_data_05_avgDay_wide.rds")

date <- data.wide[, .(date)]

# Der Luftdruck hat am Erdboden einen Normalwert von 1013,25 hPa = 101325 Pa. Ein Hochdruckgebiet besitzt damit 
# einen darüber liegenden Luftdruck.
# In Mitteleuropa liegt der Druck eines Tiefs für gewöhnlich bei 990 - 1000 hPa 
# und in Orkantiefs bei 950 - 970 hPa. 

# hier wird wieder eine id Spalte für den Tag gemacht.
data.mslp[, id := rep(1:1826, each = 8)]

# hier werden die reihen nach sinkenden latitude Werten geordnet, damit man selber besser das Grid in Nord, Süd, Ost und West-
# Ausrichtung vor Augen hat.
data.mslp <- data.mslp[order(id, -latitude, decreasing = FALSE)]
# max und min werte filtern
data.mslp[, hochdruck := apply(data.mslp[, 3:22], 1, max)]
data.mslp[, tiefdruck := apply(data.mslp[, 3:22], 1, min)]

data.mslp[tiefdruck > 100000, tiefdruck := 0]
data.mslp[hochdruck < 101400, hochdruck := 0]

druck <- data.mslp[, .(id, hochdruck, tiefdruck)]
dat <- data.mslp[, 3:22]
colnames(dat) <- as.character(c(1:20))

dat <- as.data.frame(dat)



## das sind die tief und hochdruck gebiete... dadurch dass es eine 8x20 matrix ist, kann man gut sehen wo auf der karte sich
# die gebiete ungefähr befinden
data_both <- dat
for (i in 1:20) {
  for (j in seq_len(14608)) {
    if (data_both[j, i] < 102000 && dat[j, i] > 100000) {
      data_both[j, i] <- NA
    }
  }
}

hochdruck <- dat
for (i in 1:20) {
  for (j in seq_len(14608)) {
    if (hochdruck[j, i] < 102000) {
      hochdruck[j, i] <- NA
    }
  }
}

tiefdruck <- dat
for (i in 1:20) {
  for (j in seq_len(14608)) {
    if (tiefdruck[j, i] > 100000) {
      tiefdruck[j, i] <- NA
    }
  }
}

### hier werden die tief und hochdruck gebiete tageweise aufgesplittet
# -> liste mit 1826 einträgen; einträge sind 8x20 matrizen
tiefdruck <- as.data.table(tiefdruck)
tiefdruck[, id := rep(1:1826, each = 8)]
tiefdruck.split <- split(tiefdruck, tiefdruck[, id])

hochdruck <- as.data.table(hochdruck)
hochdruck[, id := rep(1:1826, each = 8)]
hochdruck.split <- split(hochdruck, hochdruck[, id])


class(tiefdruck.split[[1]])
# immer noch data.table in den listen


# minimum und maximum sind 2 vektoren, die an jedem tag das minimum bzw maximum an luftdruck festhalten
minimum <- numeric(length = 1826)
maximum <- numeric(length = 1826)
for(i in seq_len(1826)) {
  if (all(is.na(tiefdruck.split[[i]][, 1:20]))) {
    minimum[[i]] <- NA
  } else {
  minimum[[i]] <- min(tiefdruck.split[[i]][, 1:20], na.rm = TRUE)
  }
}
length(tiefdruck.split)

for(i in seq_len(1826)) {
  if (all(is.na(hochdruck.split[[i]][, 1:20]))) {
    maximum[[i]] <- NA
  } else {
    maximum[[i]] <- max(hochdruck.split[[i]][, 1:20], na.rm = TRUE)
  }
}


## einmal max und min geplottet, man erkennt deutlich saisonale unterschiede
plot(minimum)
hist(minimum)
plot(density(minimum, na.rm = TRUE))
plot(maximum)
hist(maximum)
plot(density(maximum))

## hier kategorisiere ich die mins und maxs, aber ich glaube das brauchen wir am ende gar nicht..
tiefpunkt <- numeric(length = 1826)
hochpunkt <- numeric(length = 1826)

for (i in seq_len(1826)) {
  if (is.na(minimum[i])) {
    minimum[i] <- NA
  } else if (minimum[i] < 97000) {
    tiefpunkt[i] <- 3
  } else if (minimum[i] < 98500) {
    tiefpunkt[i] <- 2
  } else {
    tiefpunkt[i] <- 1
  }
}

for (i in seq_len(1826)) {
  if (is.na(maximum[i])) {
    maximum[i] <- NA
  } else if (maximum[i] < 103000) {
    hochpunkt[i] <- 1
  } else if (maximum[i] < 104000) {
    hochpunkt[i] <- 2
  } else {
    hochpunkt[i] <- 3
  }
}

# an 145 tagen gibt es kein tiefdruckgebiet
sum(is.na(minimum))
table(tiefpunkt)
table(hochpunkt)
# 0 entspricht also keinem Tiefpunkt falls wirs wirklich diskret machen,
# ansonsten würde ich das minimum lassen
## tief und hochpunkt entspricht nicht der anzahl der hoch oder tiefpunkte, sondern
# eine art faktor variable


## intensität zeigt, wie groß die hochdruck bzw tiefdruckgebiete sind
intensitaet.tief <- numeric(length = 1826)
intensitaet.hoch <- numeric(length = 1826)
for (i in seq_len(1826)) {
  intensitaet.tief[i] <- sum(sapply(tiefdruck.split[[i]][, 1:20], function(x) sum(!is.na(x))))
}

for (i in seq_len(1826)) {
  intensitaet.hoch[i] <- sum(sapply(hochdruck.split[[i]][, 1:20], function(x) sum(!is.na(x))))
}



coordinates.min <- data.frame(row = numeric(length = 1826), col = numeric(length = 1826))
coordinates.max <- data.frame(row = numeric(length = 1826), col = numeric(length = 1826))

for (i in seq_len(1826)) {
  if (all(is.na(tiefdruck.split[[i]][, 1:20]))){
    coordinates.min[i, ] <- NA
  } else {
  coordinates.min[i, ]<- which(tiefdruck.split[[i]] == min(tiefdruck.split[[i]][, 1:20], na.rm = TRUE), arr.ind = TRUE)
  }
}

for (i in seq_len(1826)) {
  if (all(is.na(hochdruck.split[[i]][, 1:20]))){
    coordinates.max[i, ] <- NA
  } else {
    coordinates.max[i, ]<- which(hochdruck.split[[i]] == max(hochdruck.split[[i]][, 1:20], na.rm = TRUE), arr.ind = TRUE)
  }
}
head(coordinates.min)
head(coordinates.max)
##   1 ...   5|6 ...  10|11 ... 15|16 ... 20
##  ________________________________________
## 1|1 1 1 1 1|2 2 2 2 2|3 3 3 3 3|4 4 4 4 4|
## 2|1 1 1 1 1|2 2 2 2 2|3 3 3 3 3|4 4 4 4 4|
## 3|1 1 1 1 1|2 2 2 2 2|3 3 3 3 3|4 4 4 4 4|
## 4|1 1 1 1 1|2 2 2 2 2|3 3 3 3 3|4 4 4 4 4|
## 5|5 5 5 5 5|6 6 6 6 6|7 7 7 7 7|8 8 8 8 8|
## 6|5 5 5 5 5|6 6 6 6 6|7 7 7 7 7|8 8 8 8 8|
## 7|5 5 5 5 5|6 6 6 6 6|7 7 7 7 7|8 8 8 8 8|
## 8|5 5 5 5 5|6 6 6 6 6|7 7 7 7 7|8 8 8 8 8|

# so teil ich das jetzt ein, falls euch was anderes einfällt, gerne bescheid sagen
# 0 wenn kein minimum oder maximum besteht

quadrant.min <- integer(length = 1826)
for (i in 1:1826) {
  if (coordinates.min[i, ][[1]] == 0) {
    quadrant.min[i] <- 0
  }
  else if (coordinates.min[i, ][[1]] < 5) {
    if (coordinates.min[i, ][[2]] < 6) {
      quadrant.min[i] <- 1
    }
    else if (coordinates.min[i, ][[2]] < 11) {
      quadrant.min[i] <- 2
    }
    else if (coordinates.min[i, ][[2]] < 16) {
      quadrant.min[i] <- 3
    }
    else {
      quadrant.min[i] <- 4
    }
  }
  else {
    if (coordinates.min[i, ][[2]] < 6) {
      quadrant.min[i] <- 5
    }
    else if (coordinates.min[i, ][[2]] < 11) {
      quadrant.min[i] <- 6
    }
    else if (coordinates.min[i, ][[2]] < 16) {
      quadrant.min[i] <- 7
    }
    else {
      quadrant.min[i] <- 8
    }
  }
}


quadrant.max <- integer(length = 1826)
for (i in 1:1826) {
  if (coordinates.max[i, ][[1]] == 0) {
    quadrant.max[i] <- 0
  }
  else if (coordinates.max[i, ][[1]] < 5) {
    if (coordinates.max[i, ][[2]] < 6) {
      quadrant.max[i] <- 1
    }
    else if (coordinates.max[i, ][[2]] < 11) {
      quadrant.max[i] <- 2
    }
    else if (coordinates.max[i, ][[2]] < 16) {
      quadrant.max[i] <- 3
    }
    else {
      quadrant.max[i] <- 4
    }
  }
  else {
    if (coordinates.max[i, ][[2]] < 6) {
      quadrant.max[i] <- 5
    }
    else if (coordinates.min[i, ][[2]] < 11) {
      quadrant.min[i] <- 6
    }
    else if (coordinates.max[i, ][[2]] < 16) {
      quadrant.max[i] <- 7
    }
    else {
      quadrant.max[i] <- 8
    }
  }
}

which(quadrant.max == quadrant.min)
## nur an 40 Tagen liegen Hoch und Tiefdruckgebiet im gleichen Quadranten. Passt also denk ich ganz gut.


# brauchen wir wahrscheinlich nicht, aber ich nehme mal das jahr, monat und tag als einzelne variablen mit 
# in den datensatz

year <- as.numeric(format(data.wide$date,"%Y"))
month <- as.numeric(format(data.wide$date, "%m"))
day <- as.numeric(format(data.wide$date, "%d"))
discrete <- data.table(date, day, month, year, minimum, intensitaet.tief, quadrant.min, tiefpunkt, 
                       maximum, intensitaet.hoch, quadrant.max, hochpunkt)
