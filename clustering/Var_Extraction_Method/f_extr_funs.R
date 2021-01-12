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

#splitting up into quadrants
#this funcitons adds 4 variables to the inout data,
#a vertical and horizontal Quadrant ID, as well as those ids in char version
#Input: Long format of Dataset
#Output: data with the 4 added on variables

append.QuadrantID <- function(data) {
  assertDataTable(data)
  assertSubset(c("longitude", "latitude"), names(data))
  
  out <- copy(data)
  
  #messy, but i dont have the patience rn
  out[latitude %in% unique(latitude)[seq(1,3)], ":=" (verID = 3, verChar = "South")]
  out[latitude %in% unique(latitude)[seq(4,5)], ":=" (verID = 2, verChar = "Center")]
  out[latitude %in% unique(latitude)[seq(6,8)], ":=" (verID = 1, verChar = "North")]
  
  out[longitude %in% unique(longitude)[seq(1,7)], ":=" (horID = 1, horChar = "West")]
  out[longitude %in% unique(longitude)[seq(8,13)], ":=" (horID = 2, horChar = "Center")]
  out[longitude %in% unique(longitude)[seq(14,20)], ":=" (horID = 3, horChar = "East")]
  
  out
}
data.long
a <- append.QuadrantID(copy(data.long))
a
#get the quadrant for min/max values per day
#function returns a dt of min and max quadrant values  for all days in data
#input data needs to be in long format like the output of append.QuadrantID
#if StringID is given as True, (North, Centre, South) will be output instead
#of (1,2,3)
#now also outputs the raw coords of the max/mins

quadrantValues <- function(data, StringID = FALSE) {
  assertDataTable(data)
  assertSubset(c("verID", "horID"), names(data))
  assertLogical(StringID)
  
  ifelse(StringID, cols <- c("latitude", "longitude", "verChar", "horChar"),
         cols <- c("latitude", "longitude", "verID", "horID"))
  
  out <- copy(data)
  
  #again, mapply can fuck off, wasting too much time
  maxMslp <- out[, .SD[which(avg_mslp == max(avg_mslp))], by = date, .SDcols = 
                   cols]
  setnames(maxMslp, cols, vapply(cols, 
                    FUN = function(x) paste(
                      deparse(substitute(maxMslp)), x, sep = "."), 
                    FUN.VALUE = character(1)))
  minMslp <- out[, .SD[which(avg_mslp == min(avg_mslp))], by = date, .SDcols = 
                   cols]
  setnames(minMslp, cols, vapply(cols, 
                                 FUN = function(x) paste(
                                   deparse(substitute(minMslp)), x, sep = "."), 
                                 FUN.VALUE = character(1)))
  maxGeopot <- out[, .SD[which(avg_geopot == max(avg_geopot))], by = date, .SDcols = 
                     cols]
  setnames(maxGeopot, cols, vapply(cols, 
                                 FUN = function(x) paste(
                                   deparse(substitute(maxGeopot)), x, sep = "."), 
                                 FUN.VALUE = character(1)))
  minGeopot <- out[, .SD[which(avg_geopot == min(avg_geopot))], by = date, .SDcols = 
                     cols]
  setnames(minGeopot, cols, vapply(cols, 
                                 FUN = function(x) paste(
                                   deparse(substitute(minGeopot)), x, sep = "."), 
                                 FUN.VALUE = character(1)))
  
  
  Reduce(merge, list(maxMslp, minMslp, maxGeopot, minGeopot))
}


# this is to extract all measures of central tendency (lagemaße) such as min, max, quartiles, 
# range, mean and median for oth geopotential and mslp

# INPUT: - a data table in wide format (one row for each day) 
#        - first column has to be date, columns 2-161 mslp values amd columns 162-321 geopotential
#        - the years should have already been filtered

# OUTPUT: a data table with the above mentioned variables and the date  
#         -> dim(measures(data)) = nrow(data) x 15

measures <- function(data) {
  assertDataTable(data, ncols = 321, null.ok = FALSE)
  assertSubset("date", colnames(data)[1])
  
  date <- data[, .(date)]
  
  data.mslp <- data[, 2:161]
  data.geopot <- data[, 162:321]
  
  mean.mslp <- data.mslp[, apply(data.mslp, 1, mean)]
  mean.geopot <- data.geopot[, apply(data.geopot, 1, mean)]
  
  median.mslp <- apply(data.mslp, 1, median)
  median.geopot <- apply(data.geopot, 1, median)
  
  max.mslp <- apply(data.mslp, 1, max)
  max.geopot <- apply(data.geopot, 1, max)
  
  min.mslp <- apply(data.mslp, 1, min)
  min.geopot <- apply(data.geopot, 1, min)
  
  quartile25.mslp <- apply(data.mslp, 1, function(x) quantile(x, probs = 0.25))
  quartile25.geopot <- apply(data.geopot, 1, function(x) quantile(x, probs = 0.25))
  
  quartile75.mslp <- apply(data.mslp, 1, function(x) quantile(x, probs = 0.75))
  quartile75.geopot <- apply(data.geopot, 1, function(x) quantile(x, probs = 0.25))
  
  range.mslp <- max.mslp - min.mslp
  range.geopot <- max.geopot - min.geopot
  
  measuresCentralTen <- data.table(date, mean.mslp, mean.geopot, median.mslp, median.geopot, max.mslp, max.geopot,
                                 min.mslp, min.geopot, quartile25.mslp, quartile25.geopot, 
                                 quartile75.mslp, quartile75.geopot, range.mslp, range.geopot)
  
  if (any(is.na(measuresCentralTen))) {
    stop("at least one of the created variables was computed wrongly")
  }
  measuresCentralTen
}

# just run this function, it is needed for calculating the intensity 
# INPUT: - a data table, either mslp or geopot without date
#        - ncol = 160
#        - variable, either mslp or geopot, determining the variable which will be used
#        - quartiles, 0.25 and 0.75 quartiles for either mslp or geopot, which are computed in the function intensity
# OUTPUT: a list which has in its first position a data table only with values which are greater than the 0.75 quartile
#         and in its second position a data table with those values that are less than the 0.25 quartile

keepQuartiles <- function(data, variable = "mslp", quartiles = quartiles.mslp) {
  assertDataTable(data, ncols = 160)
  assertString(variable)
  assertSubset(variable, choices = c("mslp", "geopot"))
  assertNumeric(quartiles, len = 2)
  
  hoch <- data[, apply(data, 2, 
                       function(col) lapply(col, function(x) if (x < quartiles[2]) {x <-  NA} 
                                            else {x <- x}))]
  tief <- data[, apply(data, 2, 
                       function(col) lapply(col, function(x) if (x > quartiles[1]) {x <-  NA} 
                                            else {x <- x}))]
  list(hoch, tief)
}

# this is to calculate the intensity for high pressure and low pressure for both mslp and geopot

# INPUT: a data table in long format. By this, I mean the original one which ran through timeToWinter and
#        toDailyAverage in dataset Mutate, lines 35-50. So the column names are supposed to be 
#        c("date", "longitude", "latitude", "avg_mslp", "avg_geopot")

# OUTPUT: a data table with 5 columns, first  one is the date, the others are  the intensity for 
#         high pressure and low pressure for both mslp and geopot

intensity <- function(data) {
  assertDataTable(data, ncols = 5)
  assertSubset(colnames(data), choices = c("date", "longitude", "latitude", "avg_mslp", "avg_geopot"))
  
  quartiles.mslp <- quantile(data[, avg_mslp], probs = c(0.25, 0.75))
  quartiles.geopot <- quantile(data[, avg_geopot], probs = c(0.25, 0.75))
  
  date <- data[, .(date)]
  
  data <- toGeoIndex(data = data)
  data <- longToWide(data = data)
  data.mslp <- data[, 2:161]
  data.geopot <- data[, 162:321]
  
  mslp <- keepQuartiles(data.mslp)
  geopot <- keepQuartiles(data.geopot, variable = "geopot", quartiles = quartiles.geopot)
  
  intensitaet.hoch.mslp <- apply(mslp[[1]], 1, function(x) sum(!is.na(x)))
  intensitaet.tief.mslp <- apply(mslp[[2]], 1, function(x) sum(!is.na(x)))
  
  intensitaet.hoch.geopot <- apply(geopot[[1]], 1, function(x) sum(!is.na(x)))
  intensitaet.tief.geopot <- apply(geopot[[2]], 1, function(x) sum(!is.na(x)))
  
  intensity <- data.table(date, intensitaet.hoch.mslp, intensitaet.tief.mslp, 
                          intensitaet.hoch.geopot, intensitaet.tief.geopot)
  intensity
}


