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

  