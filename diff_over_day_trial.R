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

