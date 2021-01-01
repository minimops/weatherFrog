##dbscan testing

#SWZ both
save.DBOutput(datestoCheck, "SWZ_both", pathext = "dbtrial")
save.DBOutput(datestoCheck, "SWZ_both", pathext = "dbtrial", onePage = TRUE)
#SWZ mslp
save.DBOutput(datestoCheck, "SWZ_mslp", pathext = "dbtrial", type = "mslp")
save.DBOutput(datestoCheck, "SWZ_mslp", pathext = "dbtrial", onePage = TRUE, 
              type = "mslp")
#SWZ geopot
save.DBOutput(datestoCheck, "SWZ_geopot", pathext = "dbtrial", type = "geopot")
save.DBOutput(datestoCheck, "SWZ_geopot", pathext = "dbtrial", onePage = TRUE,
              type = "geopot")
