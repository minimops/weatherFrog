##fuzzy testing

source("clustering/cluster_extr_var/f_spatClust_funs.R")
source("clustering/dateExtractionHelper.R")


filterDayData("2006-01-01", "fuzzy")

datestoCheck <- getDates(count = 10, timeframe = seq(2000, 2010), 
                         following = TRUE, gwltype = "SWZ")

#SWZ both
save.DayFilter.Output(datestoCheck, "SWZ", pathext = "FUZZY",
                      algo = "fuzzy")
save.DayFilter.Output(datestoCheck, "SWZ", pathext = "FUZZY",
                      algo = "fuzzy", onePage = TRUE)
#SWZ mslp
save.DayFilter.Output(datestoCheck, "SWZ", pathext = "FUZZY", 
                      algo = "fuzzy", type = "mslp")
save.DayFilter.Output(datestoCheck, "SWZ", pathext = "FUZZY", 
                      algo = "fuzzy", onePage = TRUE, type = "mslp")
#SWZ geopot
save.DayFilter.Output(datestoCheck, "SWZ", pathext = "FUZZY", 
                      algo = "fuzzy", type = "geopot")
save.DayFilter.Output(datestoCheck, "SWZ", pathext = "FUZZY", 
                      algo = "fuzzy", onePage = TRUE, type = "geopot")
#SWZ mixed
save.DayFilter.Output(datestoCheck, "SWZ",  algo = "fuzzy",
                      pathext = "FUZZY", onePage = TRUE,
                      type = "mixed")
#SWZ mixed alt weights
save.DayFilter.Output(datestoCheck, "SWZ_alt_weights",  algo = "fuzzy",
                      pathext = "FUZZY", onePage = TRUE,
                      type = "mixed", weights = c(1,1,2,2))

#BM mixed
save.DayFilter.Output(getDates(count = 10, timeframe = seq(2000, 2010), 
                               following = TRUE, gwltype = "BM"), algo = "fuzzy",
                      "BM", pathext = "FUZZY", onePage = TRUE,
                      type = "mixed")

#TRW mixed
save.DayFilter.Output(getDates(count = 10, timeframe = seq(2000, 2010), 
                               following = TRUE, gwltype = "TRW"), algo = "fuzzy",
                      "TRW", pathext = "FUZZY", onePage = TRUE,
                      type = "mixed")


#WA mixed
save.DayFilter.Output(getDates(count = 10, timeframe = seq(2000, 2010), 
                               following = TRUE, gwltype = "WA"), algo = "fuzzy",
                      "WA", pathext = "FUZZY", onePage = TRUE,
                      type = "mixed")


#TB mixed
save.DayFilter.Output(getDates(count = 10, timeframe = seq(2000, 2010), 
                               following = TRUE, gwltype = "TB"), algo = "fuzzy",
                      "TB", pathext = "FUZZY", onePage = TRUE,
                      type = "mixed")

#different gwls mixed
chosenGwls <- sample(unique(readRDS("Data/gwl.rds")$gwl), 10)
dates <- vapply(chosenGwls, function(x) as.character(getDates(count = 1, 
                              timeframe = seq(2000, 2010), gwltype = x)), 
                FUN.VALUE = character(1))
save.DayFilter.Output(dates, "different_gwls", algo = "fuzzy", 
                      pathext = "FUZZY", onePage = TRUE,
                      type = "mixed")


#12.12.2006
save.DayFilter.Output("2006-12-12", "12-12-2006", algo = "fuzzy",
                      pathext = "FUZZY", onePage = TRUE,
                      type = "mixed")

save.DayFilter.Output("2006-01-01", "01-01-2006", algo = "fuzzy",
                      pathext = "FUZZY", onePage = TRUE,
                      type = "mixed")

