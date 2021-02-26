#diff day

#load original dataset
cli_data <- readRDS("Data/cli_data.rds")

source("dataset_mutate_funs.R")

cli_data_30 <- cli_data[format(date, "%Y") %in% seq(1971, 2000), ]

#mslp
cli_30_mslp <- copy(cli_data_30)[, geopotential := NULL]
wideTime_30_mslp <- longToWide(cli_30_mslp, id = c("date", "time"),
                          col = c("longitude", "latitude"),
                          vars = c("mslp"))
cols <- names(wideTime_30_mslp)[-c(1,2)]
maxChange_tile_day_mslp <- copy(wideTime_30_mslp)[, (cols) := lapply(.SD, function(x) max(x) - min(x)),
                                        by = date, .SDcols = cols][, .SD[1], by = date][, time := NULL] 
Change_day_mslp <- copy(maxChange_tile_day_mslp)[, .(diff = sum(.SD)), by = date, .SDcols = cols]
names(Change_day_mslp) <- c("date", "diff_mslp")
saveRDS(Change_day_mslp, "Data/change_day_mslp.rds")

#geopot
cli_30_geopot <- copy(cli_data_30)[, mslp := NULL]
wideTime_30_geopot <- longToWide(cli_30_geopot, id = c("date", "time"),
                               col = c("longitude", "latitude"),
                               vars = c("geopotential"))
cols <- names(wideTime_30_geopot)[-c(1,2)]
maxChange_tile_day_geopotential <- copy(wideTime_30_geopot)[, (cols) := lapply(.SD, function(x) max(x) - min(x)),
                                                  by = date, .SDcols = cols][, .SD[1], by = date][, time := NULL] 
Change_day_geopot <- copy(maxChange_tile_day_geopotential)[, .(diff = sum(.SD)), by = date, .SDcols = cols]
names(Change_day_geopot) <- c("date", "diff_geopot")
saveRDS(Change_day_geopot, "Data/change_day_geopot.rds")


###Variant 2

#mslp 2
cli_30_mslp <- copy(cli_data_30)[, geopotential := NULL]
wideTime_30_mslp <- longToWide(cli_30_mslp, id = c("date", "time"),
                               col = c("longitude", "latitude"),
                               vars = c("mslp"))
cols <- names(wideTime_30_mslp)[-c(1,2)]
maxChange_tile_day_mslp_var2 <- copy(wideTime_30_mslp)[, (cols) := lapply(.SD, function(x) sum(abs(x[1]- x[2]),
                                                                                               abs(x[2]- x[3]),
                                                                                               abs(x[3]- x[4]))),
                                                  by = date, .SDcols = cols][, .SD[1], by = date][, time := NULL] 
Change_day_mslp2 <- copy(maxChange_tile_day_mslp_var2)[, .(diff = sum(.SD)), by = date, .SDcols = cols]
names(Change_day_mslp2) <- c("date", "diff_mslp")
saveRDS(Change_day_mslp2, "Data/change_day_mslp_var2.rds")

#geopot
cli_30_geopot <- copy(cli_data_30)[, mslp := NULL]
wideTime_30_geopot <- longToWide(cli_30_geopot, id = c("date", "time"),
                                 col = c("longitude", "latitude"),
                                 vars = c("geopotential"))
cols <- names(wideTime_30_geopot)[-c(1,2)]
maxChange_tile_day_geopotential_var2 <- copy(wideTime_30_geopot)[, (cols) := lapply(.SD, function(x) sum(abs(x[1]- x[2]),
                                                                                                         abs(x[2]- x[3]),
                                                                                                         abs(x[3]- x[4]))),
                                                            by = date, .SDcols = cols][, .SD[1], by = date][, time := NULL] 
Change_day_geopot2 <- copy(maxChange_tile_day_geopotential_var2)[, .(diff = sum(.SD)), by = date, .SDcols = cols]
names(Change_day_geopot2) <- c("date", "diff_geopot")
saveRDS(Change_day_geopot2, "Data/change_day_geopot_var2.rds")

