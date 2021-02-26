#days drawn

#Map with dots
# world map
world_map <- readRDS("Data/world_map.rds")

#world map with measure points
Map_dots <- world_map +
  geom_point(data = one_day, aes(x = longitude, y = latitude), size = .4,
             color = "blue") +
  ggtitle("Messpunkte auf einer Weltkarte")

ggsave("documentation/plots/fplots/MapDots.png", Map_dots, device = "png",
       width = 5, height = 3)

#one day
oneDay <- readRDS("Data/cli_data_05.rds")[as.Date(date) %in% as.Date("2006-01-01"), ]

#one day mslp
oD_time0 <- copy(oneDay)[time == 0, ][, ":=" (mslp = mslp / 100,
                                        geopotential = geopotential /  9.80665)]

day_mslp <- drawFinalDay(oD_time0, "mslp", "Mslp am 01-01-2006 um 0 Uhr", "hPa")
ggsave("documentation/plots/fplots/dayMslp.png", day_mslp, device = "png",
       width = 5, height = 3)

#one day geopot
day_geopot <- drawFinalDay(oD_time0, "geopotential", "Geopot am 01-01-2006 um 0 Uhr", "gpm")
ggsave("documentation/plots/fplots/dayGeopot.png", day_geopot, device = "png",
       width = 5, height = 3)


#all 8 pics of day
fullDay <- drawFullDay(oneDay)
ggsave("documentation/plots/fplots/fullDay.png", fullDay, device = "png",
       width = 7, height = 3)

#avg day both params
cli_data_05_avgDay <- readRDS("Data/cli_data_05_avgDay.rds")

#subsetting just one day
one_day <- copy(cli_data_05_avgDay)[format(date, "%Y-%m-%d")
    %in% c("2006-01-01"), ][, ":=" (avg_mslp = avg_mslp / 100, avg_geopot = avg_geopot /  9.80665)]
mslp1 <- drawFinalDay(one_day, "avg_mslp", "Mslp", 
                      "hPa", 0.4) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
geopot1 <- drawFinalDay(one_day, "avg_geopot", "Geopot", 
                        "gpm", 0.4) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
avgDay <- grid.arrange(mslp1, geopot1, nrow = 1, top = "Mittelwerte am 01.01.2006")  

ggsave("documentation/plots/fplots/avgDay.png", avgDay, device = "png",
       width = 7, height = 3)


## one avg day
mslp2 <- drawFinalDay(one_day, "avg_mslp", "avg Mslp am 01.01.2006", 
                      "hPa", 0.5)+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())

ggsave("documentation/plots/fplots/oneDay_2.png", mslp2, device = "png",
       width = 5, height = 3)



#avg Pics of Cluster

#dataset
rawDat <- readRDS("Data/cli_data_30_avgDay_wide.rds")
#attach clusterID
rawDat[, cluster := pamfinal$clustering]
rawDatMslp <- copy(rawDat)[, c(1:161, 322)]
rawDatGeopot <- copy(rawDat)[, c(1, 162:322)]

#mslp
cols <- names(rawDatMslp)[-c(1,162)]
mslp_clustAvg <- rawDatMslp[, (cols) :=  lapply(.SD, function(x) mean(x)), by = cluster,
           .SDcols = cols][, .SD[1], by = cluster][, date := NULL]
mslp_clustAvg_long <- melt(mslp_clustAvg,
                           id.vars = c("cluster"),
                           measure.vars = patterns("^avg"),
                           variable.name = "id",
                           value.name = "value")[, id := as.numeric(gsub("\\D", "", id))]

#create id to long/latitude data.table
coords <- readRDS("Data/cli_data_05_avgDay.rds")
setorder(coords, date, longitude, latitude)
coordID <- unique(coords[, c("longitude", "latitude")])
id2Coords <- data.table(id = seq(1, 160), longitude = coordID$longitude, 
                        latitude = coordID$latitude)

test <- copy(mslp_clustAvg_long)[copy(id2Coords), on = "id"]

mslp_avgClust_1 <- drawFinalDay(test[cluster %in% 1, ], "value", "Cluster 1 Mslp", unit = "Pa")
mslp_avgClust_2 <- drawFinalDay(test[cluster %in% 2, ], "value", "Cluster 2 Mslp", unit = "Pa")
mslp_avgClust_3 <- drawFinalDay(test[cluster %in% 3, ], "value", "Cluster 3 Mslp", unit = "Pa")
mslp_avgClust_4 <- drawFinalDay(test[cluster %in% 4, ], "value", "Cluster 4 Mslp", unit = "Pa")
mslp_avgClust_5 <- drawFinalDay(test[cluster %in% 5, ], "value", "Cluster 5 Mslp", unit = "Pa")
mslp_avgClust_6 <- drawFinalDay(test[cluster %in% 6, ], "value", "Cluster 6 Mslp", unit = "Pa")

topl <- grid.arrange(mslp_avgClust_1, mslp_avgClust_2, mslp_avgClust_3, mslp_avgClust_4,
             mslp_avgClust_5, mslp_avgClust_6)

ggsave("documentation/plots/fplots/avgClustIMG_sameskala.rds", topl, width = 7, height = 5, device = "png")


#begrenzen
test2 <- copy(test)[value > 102500, value := 102500]
test2 <- test2[value < 100500, value := 100500]
test2[, value := value / 100]

library(cowplot)
legend <- get_legend(drawFinalDay(test2[cluster %in% 2, ], "value", 
                       paste("Cluster", i, "Mslp"), unit = "hPa"))

plots <- list()

for (i in 1:6) {
  plots[[i]] <- drawFinalDay(test2[cluster %in% i, ], "value", 
               paste("Cluster", i), unit = "hPa") +
                theme(axis.title.x=element_blank(),
                      axis.title.y=element_blank())
}
library(grid)
mslp_avgC <- grid.arrange(grobs = plots, nrow = 2, left = "Latitude", bottom = "Longitude",
             right = legend, top = textGrob("Mslp im Mittel über Messpunkte",gp=gpar(fontsize=20)))

ggsave("documentation/plots/fplots/avgClustIMG_mslp.rds", mslp_avgC, width = 8, height = 5, device = "png")


#now the same for geopot
cols <- names(rawDatGeopot)[-c(1,162)]
geopot_clustAvg <- rawDatGeopot[, (cols) :=  lapply(.SD, function(x) mean(x)), by = cluster,
                            .SDcols = cols][, .SD[1], by = cluster][, date := NULL]
geopot_clustAvg_long <- melt(geopot_clustAvg,
                           id.vars = c("cluster"),
                           measure.vars = patterns("^avg"),
                           variable.name = "id",
                           value.name = "value")[, id := as.numeric(gsub("\\D", "", id))]


geopot_avg_perClus <- copy(geopot_clustAvg_long)[copy(id2Coords), on = "id"]

geopot_avg_perClus2 <- copy(geopot_avg_perClus)[, value := value / 9.80665]
geopot_avg_perClus2 <- geopot_avg_perClus2[value < 5200, value := 5200]
geopot_avg_perClus2[value > 5700, value := 5700]

library(cowplot)
legend <- get_legend(drawFinalDay(geopot_avg_perClus2[cluster %in% 2, ], "value", 
                                  paste("Cluster", i, "Geopot"), unit = "gpm"))

plots <- list()

for (i in 1:6) {
  plots[[i]] <- drawFinalDay(geopot_avg_perClus2[cluster %in% i, ], "value", 
                             paste("Cluster", i), unit = "hPa") +
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank())
}

geopot_avgC <- grid.arrange(grobs = plots, nrow = 2, left = "Latitude", bottom = "Longitude",
                          right = legend, top = textGrob("Geopot im Mittel über Messpunkte",gp=gpar(fontsize=20)))

ggsave("documentation/plots/fplots/avgClustIMG_geopot.rds", geopot_avgC, width = 8, height = 5, device = "png")


