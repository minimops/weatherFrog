library(data.table)

#this function returns a vector of dates radomly selected from a timeline
#and a selection of gwl's.
#if following is set to TRUE, the last two days will be following dates

getDates <- function(count, timeframe, following = FALSE, gwl = "NA", 
                     seed = 1234) {
  assertNumber(count)
  assertNumeric(timeframe, lower = 1900, upper = 2010)
  assertLogical(following)
  assertCharacter(gwl)
  assertNumber(seed)
  
  gwls <- readRDS("Data/gwl.rds")
  if(gwl == "NA") gwl <- unique(gwls$gwl)
  
  #subset years in timeframe and picked gwl
  gwls <- gwls[gwl %in% gwl, ][format(as.Date(date),"%Y") %in% timeframe, ]
  
  set.seed(seed)
  
  if(following){
    day <- copy(gwls)[sample(.N, 1)]
    sdate <- day$date
    sgwl <- day$gwl
    ifelse(copy(gwls)[date == as.Date(sdate) + 1, ]$gwl == sgwl, 
           fdate <- as.Date(sdate) + 1,
           fdate <- as.Date(sdate) - 1)
    datestoCheck <- c(sample(gwls$date[!gwls$date %in% c(sdate, fdate)], count - 2),
                      sdate, fdate)
  } else{
    #sample number
    datestoCheck <- sample(gwls$date, count)
  }
  
  datestoCheck
}