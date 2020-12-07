library(data.table)
data.mslp <- readRDS("Data/cli_data_05_mslp_wide.rds")
data.geopot <- readRDS("Data/cli_data_05_geo_wide.rds")
data.wide <- readRDS("Data/cli_data_05_avgDay_wide.rds")


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

for (i in 1:20) {
  data.hoch <- dat[, i]
}
?set
set(dat, i = )
?ifelse
