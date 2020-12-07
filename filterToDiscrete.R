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
data_both <- dat


## das sind die tief und hochdruck gebiete... dadurch dass es eine 8x20 matrix ist, kann man gut sehen wo auf der karte sich
# die gebiete ungefähr befinden
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


tiefdruck <- as.data.table(tiefdruck)
tiefdruck[, id := rep(1:1826, each = 8)]
tiefdruck.split <- split(tiefdruck, tiefdruck[, id])

hochdruck <- as.data.table(hochdruck)
hochdruck[, id := rep(1:1826, each = 8)]
hochdruck.split <- split(hochdruck, hochdruck[, id])


class(tiefdruck.split[[1]])
which(tiefdruck.split[[992]] == min(tiefdruck.split[[992]][, 1:20], na.rm = TRUE), arr.ind = TRUE)
lapply(tiefdruck.split, function(x) min(tiefdruck.split[[x]][, 1:20], na.rm = TRUE))
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
?min
all(is.na(tiefdruck.split[[177]][, 1:20]))

for(i in seq_len(1826)) {
  if (all(is.na(hochdruck.split[[i]][, 1:20]))) {
    maximum[[i]] <- NA
  } else {
    maximum[[i]] <- max(hochdruck.split[[i]][, 1:20], na.rm = TRUE)
  }
}

plot(minimum)
hist(minimum)
plot(density(minimum, na.rm = TRUE))
plot(maximum)
hist(maximum)
plot(density(maximum))

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
sum(is.na(minimum))
table(tiefpunkt)
table(hochpunkt)
# 0 entspricht also keinem Tiefpunkt falls wirs wirklich diskret machen,
# ansonsten würde ich das minimum lassen
## tief und hochpunkt entsprich nicht der anzahl der hoch oder tiefpunkte, sondern
# eine art faktor variable
intensitaet.tief <- numeric(length = 1826)
intensitaet.hoch <- numeric(length = 1826)
for (i in seq_len(1826)) {
  intensitaet.tief[i] <- sum(sapply(tiefdruck.split[[i]][, 1:20], function(x) sum(!is.na(x))))
}

for (i in seq_len(1826)) {
  intensitaet.hoch[i] <- sum(sapply(hochdruck.split[[i]][, 1:20], function(x) sum(!is.na(x))))
}
year <- as.numeric(format(data.wide$date,"%Y"))
month <- as.numeric(format(data.wide$date, "%m"))
day <- as.numeric(format(data.wide$date, "%d"))
discrete <- data.table(date, day, month, year, minimum, intensitaet.tief, tiefpunkt, intensitaet.hoch, maximum, hochpunkt)



nonNA_counts <- sum(sapply(tiefdruck.split[[1]][, 1:20], function(x) sum(!is.na(x))))     


