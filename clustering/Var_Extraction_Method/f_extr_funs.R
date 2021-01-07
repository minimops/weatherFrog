### Instert functions here used to extract variables from Dataset 

library(data.table)
library(checkmate)

#one overhead function that performs wanted date extraction for given Data
extrapolate <- function(data, vars = "all") {
  
  assertDataTable(data)
  assertSubset("date", names(data))
  #TODO insert all possible var creations
  assertSubset(vars, c("all", "season", "min", "max", "intensity", "location",
                       "range", "distance"))
  
  #run all the different var extraction methods based on the vars 
  
  #return new dataset
}

#individual extractions in indiv functions

#season 
#pretty much a hard copy from 
#https://stackoverflow.com/questions/9500114/find-which-season-a-particular-date-belongs-to
getSeason <- function(DATES) {
  WS <- as.Date("2012-12-15", format = "%Y-%m-%d") # Winter Solstice
  SE <- as.Date("2012-3-15",  format = "%Y-%m-%d") # Spring Equinox
  SS <- as.Date("2012-6-15",  format = "%Y-%m-%d") # Summer Solstice
  FE <- as.Date("2012-9-15",  format = "%Y-%m-%d") # Fall Equinox
  
  # Convert dates from any year to 2012 dates
  d <- as.Date(strftime(DATES, format="2012-%m-%d"))
  
  ifelse (d >= WS | d < SE, "Winter",
          ifelse (d >= SE & d < SS, "Spring",
                  ifelse (d >= SS & d < FE, "Summer", "Fall")))
}


