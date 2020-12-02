## daten einlesen mit average werten
cli_data_05_avgDay <- readRDS("Data/cli_data_05_avgDay.rds")

# avgerage geopotential weglassen, nur noch mslp im Datenatz dabei
all_days <- copy(cli_data_05_avgDay)[, avg_geopot := NULL]
# ins wide format umändern -> 8 Zeilen pro Tag und 20 longitude Punkte
cli_data_05_avgDay_mslp_all <- dcast(copy(all_days),
                                 date + latitude ~ longitude,
                                 value.var = c("avg_mslp"))

# Dimensionen
dim(cli_data_05_avgDay_mslp_all)
dim(cli_data_05_avgDay)
292160 / 20
# ergibt 14608 = dim(cli_data_05_avgDay)[[1]]
# -> sollte passen

# Namen umändern, dass sie kürzer sind
names(cli_data_05_avgDay_mslp_all) <- c("date", "latitude", 
                                        unlist(lapply(as.numeric(names(cli_data_05_avgDay_mslp_all)[3:22]), function(x) round(x, 2))))
# speichern
saveRDS(cli_data_05_avgDay_mslp_all, "Data/cli_data_05_mslp_wide.rds")

### das gleich für geopotential
all_days_geo <- copy(cli_data_05_avgDay)[, avg_mslp := NULL]
# ins wide format umändern -> 8 Zeilen pro Tag und 20 longitude Punkte
cli_data_05_avgDay_geo_all <- dcast(copy(all_days_geo),
                                     date + latitude ~ longitude,
                                     value.var = c("avg_geopot"))

# Dimensionen
dim(cli_data_05_avgDay_geo_all)
dim(cli_data_05_avgDay)
292160 / 20
# ergibt 14608 = dim(cli_data_05_avgDay)[[1]]
# -> sollte passen

# Namen umändern, dass sie kürzer sind
names(cli_data_05_avgDay_geo_all) <- c("date", "latitude", 
                                        unlist(lapply(as.numeric(names(cli_data_05_avgDay_geo_all)[3:22]), function(x) round(x, 2))))
# speichern
saveRDS(cli_data_05_avgDay_geo_all, "Data/cli_data_05_geo_wide.rds")


### das gleiche für beide

# ins wide format umändern -> 8 Zeilen pro Tag und 20 longitude Punkte
cli_data_05_avgDay_both_all <- dcast(copy(cli_data_05_avgDay),
                                    date + latitude ~ longitude,
                                    value.var = c("avg_mslp", "avg_geopot"))

# Dimensionen
dim(cli_data_05_avgDay_both_all)
dim(cli_data_05_avgDay)
292160 / 20
# ergibt 14608 = dim(cli_data_05_avgDay)[[1]]
# -> sollte passen

# Namen umändern, dass sie kürzer sind
library(stringr)
names(cli_data_05_avgDay_both_all)[1:22] <- c("date", "latitude", 
                                        unlist(lapply(names(cli_data_05_avgDay_both_all)[3:22], 
                                                      function(x) paste0(str_sub(x, start = 10, end = 15), "_avg.mslp"))))
names(cli_data_05_avgDay_both_all)[23:42] <- unlist(lapply(names(cli_data_05_avgDay_both_all)[23:42], 
                                                            function(x) paste0(str_sub(x, start = 12, end = 17), "_avg.geo")))

# speichern
saveRDS(cli_data_05_avgDay_both_all, "Data/cli_data_05_wide.rds")


### weiß trotzdem noch nicht soooo genau wie man das dann beim clustern anwendet
