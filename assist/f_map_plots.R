#plots which show pictures of day for presentation

#Map with dots
# world map
world_map <- readRDS("Data/world_map.rds")

#one day
oneDay <- readRDS("Data/cli_data_30.rds")[as.Date(date) %in% as.Date("1980-01-01"), ]

#world map with measure points
Map_dots <- world_map +
  geom_point(data = oneDay, aes(x = longitude, y = latitude), size = .4,
             color = "blue") +
  ggtitle("Messpunkte auf einer Weltkarte")

ggsave("documentation/plots/fplots/MapDots.png", Map_dots, device = "png",
       width = 5, height = 3)



#one day mslp
oD_time0 <- copy(oneDay)[time == 0, ][, ":=" (mslp = mslp / 100,
                                        geopotential = geopotential /  9.80665)]

day_mslp <- drawFinalDay(oD_time0, "mslp", "Mslp am 01-01-1980 um 0 Uhr", "hPa")
ggsave("bericht/assets/dayMslp.png", day_mslp, device = "png",
       width = 5, height = 3)

#one day geopot
day_geopot <- drawFinalDay(oD_time0, "geopotential", "Geopot am 01-01-1980 um 0 Uhr", "gpm")
ggsave("bericht/assets/dayGeopot.png", day_geopot, device = "png",
       width = 5, height = 3)


#all 8 pics of day
fullDay <- drawFullDay(oneDay)
ggsave("documentation/plots/fplots/fullDay.png", fullDay, device = "png",
       width = 7, height = 3)

#avg day both params
cli_data_05_avgDay <- readRDS("Data/cli_data_05_avgDay.rds")
cli_data_30_avgDay <- readRDS("Data/cli_data_30_avgDay.rds")


#subsetting just one day
one_day <- copy(cli_data_30_avgDay)[format(date, "%Y-%m-%d")
    %in% c("1980-01-01"), ][, ":=" (avg_mslp = avg_mslp / 100, avg_geopot = avg_geopot /  9.80665)]
mslp1 <- drawFinalDay(one_day, "avg_mslp", "Mslp", 
                      "hPa", 0.4) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())
geopot1 <- drawFinalDay(one_day, "avg_geopot", "Geopot", 
                        "gpm", 0.4) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())
avgDay <- grid.arrange(mslp1, geopot1, nrow = 1, top = "Mittelwerte am 01.01.1980", bottom = "Longitude",
                       left = "Latitude")  

ggsave("documentation/plots/fplots/avgDay.png", avgDay, device = "png",
       width = 7, height = 3)

ggsave("bericht/assets/avgDay.png", avgDay, device = "png",
       width = 7, height = 3)

## one avg day
mslp2 <- drawFinalDay(one_day, "avg_mslp", "Gemittelter Mslp am 01.01.1980", 
                      "hPa", 0.5)

ggsave("bericht/assets/oneDay_2.png", mslp2, device = "png",
       width = 5, height = 3)

#with quadrant lines
mslp2_quadrants <- drawFinalDay(one_day, "avg_mslp", "Gemittelter Mslp am 01.01.1980", 
                      "hPa", 0.5) +
                    geom_hline(yintercept = 48.58216) +
                    geom_hline(yintercept = 59.81457) +
                    geom_vline(xintercept = -29.812660 +2.81) +
                    geom_vline(xintercept = 3.937553 + 2.81)

ggsave("bericht/assets/quadranten.png", mslp2_quadrants, device = "png",
       width = 5, height = 3)


geopot2 <- drawFinalDay(one_day, "avg_geopot", "Gemitteltes Geopot am 01.01.1980", 
                      "gpm", 0.5)

ggsave("bericht/assets/oneDay_geo_2.png", geopot2, device = "png",
       width = 5, height = 3)


#avg Pics of Cluster

#dataset
rawDat <- readRDS("Data/cli_data_30_avgDay_wide.rds")
pamfinal <- readRDS("finalDATA/PAMres.rds")
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

source("clustering/dayDrawer.R")

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
                       paste("Cluster", 2, "Mslp"), unit = "hPa") + 
                       scale_fill_gradient(low = "blue", high = "red", 
                                           breaks=c(1005, 1010, 1015, 1020, 1025),
                                           limits = c(1005, 1025),
                                           name = "in hPa"))

plots <- list()

for (i in 1:6) {
  plots[[i]] <- drawFinalDay(test2[cluster %in% i, ], "value", 
               paste("Cluster", i), unit = "hPa") +
                scale_fill_gradient(low = "blue", high = "red", 
                        breaks=c(1005, 1010, 1015, 1020, 1025), 
                        limits = c(1005, 1025), guide = FALSE) +
                theme(axis.title.x=element_blank(),
                      axis.title.y=element_blank(),
                      legend.title = element_blank()) 
}
library(grid)
mslp_avgC <- grid.arrange(grobs = plots, nrow = 2, left = "Latitude", bottom = "Longitude",
             right = legend, top = textGrob("Mslp im Mittel über Messpunkte",gp=gpar(fontsize=20)))

ggsave("documentation/plots/fplots/avgClustIMG_mslp.png", mslp_avgC, width = 8, height = 5, device = "png")

ggsave("bericht/assets/avgClustIMG_mslp.png", mslp_avgC, width = 8, height = 5, device = "png")


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
                                  paste("Cluster", i, "Geopot"), unit = "gpm") +
                       scale_fill_gradient(low = "blue", high = "red", 
                                           breaks= seq(5200, 5700, by = 100),
                                           limits = c(5200, 5700),
                                           name = "in gpm"))

plots <- list()

for (i in 1:6) {
  plots[[i]] <- drawFinalDay(geopot_avg_perClus2[cluster %in% i, ], "value", 
                             paste("Cluster", i), unit = "hPa") +
    scale_fill_gradient(low = "blue", high = "red", 
                        breaks=seq(5200, 5700, by = 100),
                        limits = c(5200, 5700), guide = F) +
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          legend.title = element_blank()) 
}

geopot_avgC <- grid.arrange(grobs = plots, nrow = 2, left = "Latitude", bottom = "Longitude",
                          right = legend, top = textGrob("Geopot im Mittel über Messpunkte",gp=gpar(fontsize=20)))

ggsave("documentation/plots/fplots/avgClustIMG_geopot.png", geopot_avgC, width = 8, height = 5, device = "png")

ggsave("bericht/assets/avgClustIMG_geopot.png", geopot_avgC, width = 8, height = 5, device = "png")


##filter method for 01-01-2006
filterDayData("2008-01-01", "custom", type = "mslp")

day <- "1980-01-01"
plots <- list()

  oneDay <- readRDS("Data/cli_data_30_avgDay.rds")[date %in% as.Date(day), ]
  plotRes <- copy(oneDay)
  
  result <- filterDay(oneDay[, date := NULL], "avg_mslp")
  
  plotDat <- data.frame(plotRes, cluster = as.factor(result$cluster))
  plots[[2]] <- drawDay(plotDat, whichFill = "cluster", showGuide = FALSE,
          discrete = TRUE) + labs(title = "Gefilterter Mslp")
  
  result2 <- filterDay(oneDay[, date := NULL], "avg_geopot")
  
  plotDat2 <- data.frame(plotRes, cluster = as.factor(result2$cluster))
  plots[[4]] <- drawDay(plotDat2, whichFill = "cluster", showGuide = FALSE,
                        discrete = TRUE) + labs(title = "Gefiltertes Geopot")

    plots[[1]] <- drawFinalDay(plotRes[, avg_mslp := avg_mslp / 100],
                               "avg_mslp", "Gemittelter Mslp",
                               "hPa") + theme(axis.title.x = element_blank(),
                                              axis.title.y = element_blank())
    
    plots[[3]] <- drawFinalDay(plotRes[, avg_geopot := avg_geopot / 9.80665],
                               "avg_geopot", "Gemitteltes Geopot",
                               "gpm") + theme(axis.title.x = element_blank(),
                                              axis.title.y = element_blank())

    filterExa <- grid.arrange(grobs = plots,
                 top = textGrob("Filtern des 01.01.1980",gp=gpar(fontsize=20)),
                 bottom = "Longitude",
                 left = "Latitude")

    ggsave("documentation/plots/fplots/filterExa.png", filterExa, device = "png",
           width = 8, height = 5)
    
    ggsave("bericht/assets/filterRes.png", filterExa, device = "png",
           width = 8, height = 5)
    
    