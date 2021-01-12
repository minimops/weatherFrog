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
  out[latitude %in% unique(latitude)[seq(1,3)], ":=" (verID = 1, verChar = "North")]
  out[latitude %in% unique(latitude)[seq(4,6)], ":=" (verID = 2, verChar = "Center")]
  out[latitude %in% unique(latitude)[seq(7,8)], ":=" (verID = 3, verChar = "South")]
  
  out[longitude %in% unique(longitude)[seq(1,7)], ":=" (horID = 1, horChar = "West")]
  out[longitude %in% unique(longitude)[seq(8,13)], ":=" (horID = 2, horChar = "Center")]
  out[longitude %in% unique(longitude)[seq(14,20)], ":=" (horID = 3, horChar = "East")]
  
  out
}


#get the quadrant for min/max values per day
#function returns a dt of min and max quadrant values  for all days in data
#input data needs to be in long format like the output of append.QuadrantID
#if StringID is given as True, (North, Centre, South) will be output instead
#of (1,2,3)

quadrantValues <- function(data, StringID = FALSE) {
  assertDataTable(data)
  assertSubset(c("verID", "horID"), names(data))
  assertLogical(StringID)
  
  ifelse(StringID, cols <- c("verChar", "horChar"),
         cols <- c("verID", "horID"))
  
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
  
  if (any(c(length(mean.mslp), length(mean.geopot), length(median.mslp), length(median.geopot),
            length(max.mslp), length(max.geopot), length(min.mslp), length(min.geopot), 
            length(quartile25.mslp), length(quartile25.geopot), length(quartile75.mslp), 
            length(quartile75.geopot), length(range.mslp), length(range.geopot)) != nrow(data.wide))) {
    stop("at least one of the created variables was computed wrongly")
  }
  
  measuresLocalTen <- data.table(date, mean.mslp, mean.geopot, median.mslp, median.geopot, max.mslp, max.geopot,
                                 min.mslp, min.geopot, quartile25.mslp, quartile25.geopot, 
                                 quartile75.mslp, quartile75.geopot, range.mslp, range.geopot)
  measuresLocalTen
}
