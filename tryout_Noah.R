library(ggplot2)
library(data.table)

cli_data_2k_avgDay <- readRDS("Data/cli_data_2k_avgDay.rds")


one_day <- copy(cli_data_2k_avgDay)[format(date, "%Y-%m-%d")
                                    %in% c("2000-01-01"), ][, 
                                    avg_geopot := NULL]

ggplot(data = one_day, 
       aes(x = longitude, y = latitude, color = avg_mslp)) +
  geom_point(size = 2)


cli_data_2k_avgDay_mslp <- dcast(copy(one_day),
                                 latitude ~ longitude,
                                 value.var = c("avg_mslp")
)[, latitude := NULL]

names(cli_data_2k_avgDay_mslp) <- as.character(seq(1, 20))

# 
# 
# # Ward Hierarchical Clustering
# d <- dist(mydata, method = "euclidean") # distance matrix
# fit <- hclust(d, method="ward")
# plot(fit) # display dendogram
# 


#drawing into map
# require(rgdal)
# require(ggplot2)
# 
# fn <- file.path(tempdir(), "GBR_adm_gdb.zip", fsep = "\\")
# download.file("https://gisco-services.ec.europa.eu/distribution/v2/nuts/download/ref-nuts-2010-10m.shp.zip", fn)
# utils::unzip(fn, exdir = tempdir())
# shp <- readOGR(dsn = file.path(tempdir(), "GBR_adm1.shp"), stringsAsFactors = F)

library(ggplot2)
library(sf)
library(rgeos)
library(rnaturalearth)
library(rnaturalearthdata)
# load data
world <- ne_countries(scale = "medium", returnclass = "sf")
# gene world map
ggplot(data = world) +
  geom_sf() +
  labs( x = "Longitude", y = "Latitude") +
  ggtitle("World map", subtitle = paste0("(", length(unique(world$admin)), " countries)")) +
  geom_point(data = one_day, aes(x = longitude, y = latitude, color = avg_mslp))



library(dplyr)
library(tidyr)
library(gstat)
library(raster)
library(rgeos)
library(scales)

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
                           by = 2),
                   y = seq(from = y_range[1],
                           to = y_range[2], 
                           by = 2))  # expand points to grid  

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
  scale_fill_gradientn(name = "mslp in Pa", colors = rainbow(100)) +
  guides(fill = guide_colorbar()) +
  scale_alpha(guide = "none") + 
  theme_bw()
