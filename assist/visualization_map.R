library(ggplot2)
library(data.table)

cli_data_2k_avgDay <- readRDS("Data/cli_data_2k_avgDay.rds")

#subsetting just one day
one_day <- copy(cli_data_2k_avgDay)[format(date, "%Y-%m-%d")
                                    %in% c("2006-01-01"), ]
#plot on coords
ggplot(data = one_day, 
       aes(x = longitude, y = latitude, color = avg_mslp)) +
  geom_point(size = 2)


#coordinate measurements of wide format in laongitude
cli_data_2k_avgDay_mslp <- dcast(copy(one_day),
                                 latitude ~ longitude,
                                 value.var = c("avg_mslp")
)[, latitude := NULL]

names(cli_data_2k_avgDay_mslp) <- as.character(seq(1, 20))


# world map
world_map <- readRDS("Data/world_map.rds")

#world map with measure points
world_map +
  geom_point(data = one_day, aes(x = longitude, y = latitude), size = .6) +
  ggtitle("Messpunkte auf einer Weltkarte")

#Region Map with measure points
#TODO change borders to min-5 etc
world_map +
  geom_point(data = one_day, aes(x = longitude, y = latitude)) +
  ggtitle("Measure-points on Local Map") +
  coord_sf(xlim = c(min(one_day$longitude) - 5, 
                    max(one_day$longitude) + 5), 
          ylim = c(min(one_day$latitude) - 5, 
                    max(one_day$latitude) + 5), expand = FALSE)


world_map_local <- readRDS("Data/world_map_local.rds")

#rectangles over points
world_map_local +
  ggtitle("Messpunkte") +
  geom_rect(data=one_day, 
            mapping=aes(xmin=longitude - diff_lon, xmax=longitude + diff_lon,
                        ymin=latitude - diff_lat, ymax=latitude + diff_lat),
            color="black", alpha=0.5)

#mslpon one day
world_map_local +
  ggtitle("mslp am 01-01-2006") +
  geom_rect(data=one_day, 
            mapping=aes(xmin=longitude - diff_lon, xmax=longitude + diff_lon,
                        ymin=latitude - diff_lat, ymax=latitude + diff_lat, 
                        fill = avg_mslp), alpha=0.5) +
  scale_fill_gradient(name = "mslp in Pa", low = "blue", high = "red")

#mslpon one day
world_map_local +
  ggtitle("Geopotential am 01-01-2006") +
  geom_rect(data=one_day, 
            mapping=aes(xmin=longitude - diff_lon, xmax=longitude + diff_lon,
                        ymin=latitude - diff_lat, ymax=latitude + diff_lat, 
                        fill = avg_geopot), alpha=0.5) +
  scale_fill_gradient(name = "geopot in m²/s²", low = "blue", high = "red")


# require(gridExtra)
# plot1 <- qplot(1)
# plot2 <- qplot(1)
# grid.arrange(plot1, plot2, ncol=2)


library(gstat)
library(rgeos)

# create spatial points object
oneday_mslp_sp <- as.data.frame(one_day)
# convert the data into spatial coordinates
coordinates(oneday_mslp_sp) <- ~longitude + latitude
class(oneday_mslp_sp)

plot(oneday_mslp_sp)

summary(one_day$longitude)
summary(one_day$latitude)

x_range <- c(min(one_day$longitude), max(one_day$longitude))
y_range <- c(min(one_day$latitude), max(one_day$latitude))


grd <- expand.grid(x = seq(from = x_range[1],
                           to = x_range[2], 
                           by = 5.625038),
                   y = seq(from = y_range[1],
                           to = y_range[2], 
                           by = 5.625038))  # expand points to grid  

# Convert grd object to a matrix and then turn into a spatial
# points object
coordinates(grd) <- ~x + y
# turn into a spatial pixels object
gridded(grd) <- TRUE  

plot(grd, cex = 1.5, col = "grey")
plot(oneday_mslp_sp,
     pch = 10,
     col = "red",
     cex = 1,
     add = TRUE)

# interpolate the data
idw_pow1 <- idw(formula = avg_mslp ~ 1,
                locations = oneday_mslp_sp,
                newdata = grd,
                idp = 3)

plot(idw_pow1,
     col = terrain.colors(55))

gplot_oneDay <- as.data.frame(idw_pow1)

ggplot(data = world) +
  geom_sf(fill = "gray90") +
  labs( x = "Longitude", y = "Latitude") +
  ggtitle("mslp on 01-01-2000") +
  coord_sf(xlim = c(-70.00, 50.00), ylim = c(30.00, 77.00), expand = FALSE) +
  geom_raster(data = gplot_oneDay, aes(x = x, y = y, fill = var1.pred, alpha = 0.8)) +
  scale_fill_gradient(name = "mslp in Pa", low = "blue", high = "red") +
  guides(fill = guide_colorbar()) +
  scale_alpha(guide = "none") + 
  theme_bw()

