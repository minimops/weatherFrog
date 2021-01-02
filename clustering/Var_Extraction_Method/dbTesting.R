##dbscan testing

datestoCheck <- getDates(count = 10, timeframe = seq(2000, 2010), 
                         following = TRUE, gwl = "SWZ")

#SWZ both
save.DBOutput(datestoCheck, "SWZ", pathext = "dbtrial")
save.DBOutput(datestoCheck, "SWZ", pathext = "dbtrial", onePage = TRUE)
#SWZ mslp
save.DBOutput(datestoCheck, "SWZ", pathext = "dbtrial", type = "mslp")
save.DBOutput(datestoCheck, "SWZ", pathext = "dbtrial", onePage = TRUE, 
              type = "mslp")
#SWZ geopot
save.DBOutput(datestoCheck, "SWZ", pathext = "dbtrial", type = "geopot")
save.DBOutput(datestoCheck, "SWZ", pathext = "dbtrial", onePage = TRUE,
              type = "geopot")
#SWZ mixed
save.DBOutput(datestoCheck, "SWZ", pathext = "dbtrial", onePage = TRUE,
              type = "mixed")

#BM mixed
save.DBOutput(getDates(count = 10, timeframe = seq(2000, 2010), 
                       following = TRUE, gwl = "BM"),
              "BM", pathext = "dbtrial", onePage = TRUE,
              type = "mixed")

#TRW mixed
save.DBOutput(getDates(count = 10, timeframe = seq(2000, 2010), 
                       following = TRUE, gwl = "TRW"),
              "TRW", pathext = "dbtrial", onePage = TRUE,
              type = "mixed")


#WA mixed
save.DBOutput(getDates(count = 10, timeframe = seq(2000, 2010), 
                       following = TRUE, gwl = "WA"),
              "WA", pathext = "dbtrial", onePage = TRUE,
              type = "mixed")

#TB mixed
save.DBOutput(getDates(count = 10, timeframe = seq(2000, 2010), 
                       following = TRUE, gwl = "TB"),
              "TB", pathext = "dbtrial", onePage = TRUE,
              type = "mixed")

#different gwls mixed
chosenGwls <- sample(unique(readRDS("Data/gwl.rds")$gwl), 10)
dates <- vapply(chosenGwls, function(x) as.character(getDates(count = 1, 
                                timeframe = seq(2000, 2010), gwl = x)), 
                FUN.VALUE = character(1))
save.DBOutput(dates, "different_gwls", pathext = "dbtrial", onePage = TRUE,
              type = "mixed")
