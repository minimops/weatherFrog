library(ggplot2)


ggplot(data = copy(cli_data_2k_avgDay)[format(date, "%Y-%m-%d") %in% c("2000-01-01")], 
       aes(x = longitude, y = latitude, color = avg_geopot)) +
  geom_point(size = 2)


one_day <- copy(cli_data_2k_avgDay)[format(date, "%Y-%m-%d")
                                    %in% c("2000-01-01"), ][, 
                                    avg_geopot := NULL]

cli_data_2k_avgDay_mslp <- dcast(copy(one_day),
                                 latitude ~ longitude,
                                 value.var = c("avg_mslp")
)[, latitude := NULL]

names(cli_data_2k_avgDay_mslp) <- as.character(seq(1, 20))

# Ward Hierarchical Clustering
d <- dist(mydata, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward")
plot(fit) # display dendogram



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

