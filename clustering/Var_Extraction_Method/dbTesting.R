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


