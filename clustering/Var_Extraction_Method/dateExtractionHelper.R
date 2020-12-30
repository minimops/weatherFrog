gwls <- readRDS("Data/gwl.rds")

#subset years younger than 1971
gwls <- gwls[gwl == "TM", ][format(as.Date(date),"%Y") %in% seq(1971, 2010)]

#sample 10
(datestoCheck <- sample(gwls$date, 10))


#manually change one day10 to be successive to day9
datestoCheck[[10]] <- as.Date("1990-04-25")

(datestoCheck)
