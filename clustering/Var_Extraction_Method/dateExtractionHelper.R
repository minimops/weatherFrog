gwls <- readRDS("Data/gwl.rds")

#subset years younger than 2000
gwls <- gwls[gwl == "TM", ][format(as.Date(date),"%Y") %in% seq(2000, 2010)]

#sample 10
(datestoCheck <- sample(gwls$date, 10))


#manually change one day10 to be successive to day9
datestoCheck[[10]] <- as.Date("2002-08-06")

(datestoCheck)
