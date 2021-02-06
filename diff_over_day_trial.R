##difference over day trial

#load original dataset
cli_data <- readRDS("~/Uni/StatPrakt/weatherFrog/Data/cli_data.rds")

#use 30 years for now
source("dataset_mutate_funs.R")

cli_data_30 <- subsetYears(cli_data, seq(1971, 2000))


wideTime_30 <- longToWide(cli_data_30, id = c("date", "time"),
                col = c("longitude", "latitude"),
                vars = c("mslp", "geopotential"))

cols <- names(wideTime_30)[-c(1,2)]
maxChange_tile_day <- copy(wideTime_30)[, (cols) := lapply(.SD, function(x) max(x) - min(x)),
                                        by = date, .SDcols = cols][, .SD[1], by = date][, time := NULL] 

Change_day <- copy(maxChange_tile_day)[, .(diff = sum(.SD)), by = date, .SDcols = cols]
hist(Change_day$diff)

source("clustering/ClusterAssesmentHelper.R")

Change_day_gwl <- attachGwl(Change_day)

ggplot(as.data.frame(Change_day_gwl), aes(x = diff)) +
  ggtitle("Verteilung Änderung über den Tag pro GWL") +
  geom_histogram(aes(y=..density..), bins = 30) +
  facet_wrap(~ gwl)
#doesnt really seem to have any distribution differences


#per season

#function to get winter/summer
getWinSum <- function(DATES) {
  W <- as.Date("2012-10-16", format = "%Y-%m-%d") # Winter Solstice
  S <- as.Date("2012-4-16",  format = "%Y-%m-%d") # Summer Solstice
  
  # Convert dates from any year to 2012 dates
  d <- as.Date(strftime(DATES, format="2012-%m-%d"))
  
  ifelse (d >= W | d < S, "W", "S")
}

#attach season
Change_day_gwl_season <- copy(Change_day_gwl)[, season := getWinSum(date)]

ggplot(as.data.frame(Change_day_gwl_season), aes(x = diff)) +
  ggtitle("Verteilung Änderung über den Tag pro GWL pro Saison") +
  geom_histogram(aes(y=..density.., color = season, fill = season),
                 bins = 30, alpha = 0.4) +
  facet_wrap(~ gwl)



#per param


cli_data_val <- melt(cli_data_30, id.vars = c("date", "time", "longitude", "latitude"),
                     measure.vars = c("mslp", "geopotential"), variable.name = "type",
                     value.name = "value")

wideTime_val <- longToWide(cli_data_val, id = c("date", "time", "type"),
                           col = c("longitude", "latitude"),
                           vars = c("value"))

cols2 <- names(wideTime_val)[-c(1, 2, 3)]
maxChange_tile_day_val <- copy(wideTime_val)[, (cols2) := lapply(.SD, function(x) max(x) - min(x)),
                                        by = c("date", "type"), .SDcols = cols2][, .SD[1], by = c("date", "type")][, time := NULL] 

Change_day_val <- copy(maxChange_tile_day_val)[, .(diff = sum(.SD)), by = c("date", "type"), .SDcols = cols2]

ggplot(as.data.frame(Change_day_val), aes(x = diff)) +
  ggtitle("Verteilung Änderung über den Tag pro Messwert") +
  geom_histogram(aes(y=..density..),
                 bins = 30) +
  facet_wrap(~ type)


Change_day_val_gwl <- attachGwl(Change_day_val)

ggplot(as.data.frame(Change_day_val_gwl), aes(x = diff)) +
  ggtitle("Verteilung Änderung über den Tag pro GWL pro Messwert") +
  geom_histogram(aes(y=..density.., fill = type), bins = 30) +
  facet_wrap(~ gwl)


#attach season
Change_day_val_gwl_season <- copy(Change_day_val_gwl)[, season := getWinSum(date)][, seasonType := paste0(season, type)]

ggplot(as.data.frame(Change_day_val_gwl_season), aes(x = diff)) +
  ggtitle("Verteilung Änderung über den Tag pro GWL pro Saison * Messwert") +
  geom_histogram(aes(y=..density.., fill = seasonType),
                 bins = 30) +
  facet_wrap(~ gwl)
