### Insert functions here used to extract variables from Dataset 

library(data.table)
library(checkmate)
library(stringr)


#one overhead function that performs wanted date and 
#variable extraction for given Data
extrapolate <- function(yearspan, vars = "all") {
  
  assertNumeric(yearspan, lower = 1900, upper = 2010)
  #TODO insert all possible var creations
  #TODO handling
  assertSubset(vars, c("all", "all.diffDay", "all.diffDay2", "all.4qm", 
                       "all.pca", "all.fullID", "final"))
  
  #available datasets:
  #sort these smaller to larger
  avail.sets <- list("05" = seq(2006, 2010), "2k" = seq(2000, 2010), 
                     "30" = seq(1971, 2000), "full" = seq(1900, 2010))
  
  #find smallest Dataset, where yearspan is included
  for (setNum in seq_len(length(avail.sets))) {
    if(all(yearspan %in% avail.sets[[setNum]])){
      SETtoUSE <- (names(avail.sets[setNum]))
      break
    }
  }
  
  ds.Name.long <- paste0("Data/cli_data_", SETtoUSE, "_avgDay.rds")
  ds.Name.wide <- paste0("Data/cli_data_", SETtoUSE, "_avgDay_wide.rds")
  
  #read in different Data formats
  #TODO put this directly in front of where used in the future, as this is
  #possibly a memory overload issue
  
  #read long ds
  data_long_avg <- readRDS(ds.Name.long)[format(as.Date(date),"%Y") %in% yearspan, ]
  
  #read wide ds
  data_wide_avgDay <- readRDS(ds.Name.wide)[format(as.Date(date),"%Y") %in% yearspan, ]
  
  
  #run all the different var extraction methods based on the vars 
  
  #attatch quadrant info
  data_long_avg_quadrant <- append.QuadrantID(data_long_avg)
  
  #get max and min quadrant values
  ifelse(vars == "all.fullID",
         max_mins_location <- quadrantValues(diff_quadrandID(data_long_avg)), 
        max_mins_location <- quadrantValues(data_long_avg_quadrant))
 
  #distribution measures
  distMeasures <- measures(copy(data_wide_avgDay))
  
  #quadrant means
  ifelse(vars == "all.4qm",
         Qmeans <- quadrantMean(append.QuadrantID(data_long_avg, amount = 4)),
         Qmeans <- quadrantMean(data_long_avg_quadrant))
         
  
  #distance of extreme points
  distances <- Reduce(merge, list(
                      euclidean(max_mins_location),
                      euclidean(max_mins_location, 
                        x1 = "maxGeopot", x2 = "minGeopot", outputname = "geopot"),
                      euclidean(max_mins_location,
                        x1 = "maxMslp", x2 = "maxGeopot", outputname = "maxDiff"),
                      euclidean(max_mins_location,
                        x1 = "minMslp", x2 = "minGeopot", outputname = "minDiff")
                        )
                      )
  
  #intensity
  intensity <- intensity(data_wide_avgDay, data_long_avg)
  
  #pca
  if(vars == "all.pca"){
    pca <- getPCA(copy(data_wide_avgDay), 3)
    Qmeans <- Reduce(merge, list(Qmeans, pca))
  }
  
  if(vars == "all.diffDay" || vars == "all.diffDay2"){
    ifelse(vars == "all.diffDay",
           diff <- diffDay(yearspan, variant = "range"),
           diff <- diffDay(yearspan, variant = "continuous")
    )
    Qmeans <- Reduce(merge, list(Qmeans, diff))
  }

  #return new dataset
  out <- Reduce(merge, list(distMeasures, 
                     max_mins_location[, grep("latitude|longitude$", 
                                   colnames(max_mins_location)) := NULL], 
                     distances,
                     intensity,
                     Qmeans))
  
  
  setcolorder(out, c("date", "mean.mslp", "median.mslp", "max.mslp", "min.mslp",
                     "quartile25.mslp", "quartile75.mslp", "range.mslp",
                     "intensity.high.mslp", "intensity.low.mslp", "mean.geopot",
                     "median.geopot", "max.geopot", "min.geopot", "quartile25.geopot",
                     "quartile75.geopot", "range.geopot", "intensity.high.geopot",
                     "intensity.low.geopot", "maxMslp.verID", "maxMslp.horID",
                     "minMslp.verID", "minMslp.horID", "euclidean.mslp",
                     "euclidean.maxDiff", "maxGeopot.verID", "maxGeopot.horID",
                     "minGeopot.verID", "minGeopot.horID", "euclidean.geopot",
                     "euclidean.minDiff", grep("meanMslp", names(out), value = TRUE),
                     grep("meanGeopot", names(out), value = TRUE), 
                     grep("PC", names(out), value = TRUE),
                     grep("diff", names(out), value = TRUE)
  ))
  
  out
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

append.QuadrantID <- function(data, amount = 9) {
  assertDataTable(data)
  assertSubset(c("longitude", "latitude"), names(data))
  assertSubset(amount, c(9, 4))
  
  out <- copy(data)
  ifelse(amount == 9,
         {
  #messy, but i dont have the patience rn
           # sortieren 
  out[latitude %in% sort(unique(latitude))[seq(1,3)], ":=" (verID = 3, verChar = "South")]
  out[latitude %in% sort(unique(latitude))[seq(4,5)], ":=" (verID = 2, verChar = "Center")]
  out[latitude %in% sort(unique(latitude))[seq(6,8)], ":=" (verID = 1, verChar = "North")]
  
  out[longitude %in% sort(unique(longitude))[seq(1,7)], ":=" (horID = 1, horChar = "West")]
  out[longitude %in% sort(unique(longitude))[seq(8,13)], ":=" (horID = 2, horChar = "Center")]
  out[longitude %in% sort(unique(longitude))[seq(14,20)], ":=" (horID = 3, horChar = "East")]
         },
  {
  #this is so dumb, my brain hurts though
  nw <- out[which(latitude %in% unique(latitude)[seq(1,4)]), ][longitude %in% 
    unique(longitude)[seq(1, 10)], ][, ":=" (quadID = 1, quadChar = "Northwest")]
  ne <- out[which(latitude %in% unique(latitude)[seq(1,4)]), ][longitude %in% 
    unique(longitude)[seq(11, 20)], ][, ":=" (quadID = 2, quadChar = "Northeast")]
  sw <- out[which(latitude %in% unique(latitude)[seq(5,8)]), ][longitude %in% 
    unique(longitude)[seq(1, 10)], ][, ":=" (quadID = 3, quadChar = "Southwest")]
  se <- out[which(latitude %in% unique(latitude)[seq(5,8)]), ][longitude %in% 
    unique(longitude)[seq(11, 20)], ][, ":=" (quadID = 4, quadChar = "Southeast")]
  
  out <- rbind(nw, ne, sw, se)
  
  })
  
  out
}

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
  #TODO the uniquw is a dirty fix to it creating duplicate rows
  maxMslp <- unique(out[, .SD[which(avg_mslp == max(avg_mslp))], by = date, .SDcols = 
                   cols], by = "date")
  setnames(maxMslp, cols, vapply(cols, 
                    FUN = function(x) paste(
                      deparse(substitute(maxMslp)), x, sep = "."), 
                    FUN.VALUE = character(1)))
  minMslp <- unique(out[, .SD[which(avg_mslp == min(avg_mslp))], by = date, .SDcols = 
                   cols], by = "date")
  setnames(minMslp, cols, vapply(cols, 
                                 FUN = function(x) paste(
                                   deparse(substitute(minMslp)), x, sep = "."), 
                                 FUN.VALUE = character(1)))
  maxGeopot <- unique(out[, .SD[which(avg_geopot == max(avg_geopot))], by = date, .SDcols = 
                     cols], by ="date")
  setnames(maxGeopot, cols, vapply(cols, 
                                 FUN = function(x) paste(
                                   deparse(substitute(maxGeopot)), x, sep = "."), 
                                 FUN.VALUE = character(1)))
  minGeopot <- unique(out[, .SD[which(avg_geopot == min(avg_geopot))], by = date, .SDcols = 
                     cols], by ="date")
  setnames(minGeopot, cols, vapply(cols, 
                                 FUN = function(x) paste(
                                   deparse(substitute(minGeopot)), x, sep = "."), 
                                 FUN.VALUE = character(1)))
  
  
  Reduce(merge, list(maxMslp, minMslp, maxGeopot, minGeopot))
}

# this is a function to calculate the euclidean distance between two points which are the points x1 and x2.
# INPUT: - data, a data table in format of output of the function quadrantValues
#        - x1: first point (either maxMslp, minMslp, maxGeopot, minGeopot)
#        - x2: second point
#        - outputname: name of the output, so f.e. distance between minGeopot and minMslp "geopot.mslp.min"
#                      or "mslp" for minMslp and maxMslp

# OUTPUT: a data table with two columns, date and euclidean distance

euclidean <- function(data, x1 = "maxMslp", x2 = "minMslp", outputname = "mslp") {
  assertDataTable(data)
  assertString(x1)
  assertString(x2)
  assertSubset(x1, choices = c("maxMslp", "minMslp", "maxGeopot", "minGeopot"))
  assertSubset(x2, choices = c("maxMslp", "minMslp", "maxGeopot", "minGeopot"))
  assertString(outputname)
  
  cols <- str_split(paste("date", paste0(x1, ".latitude"), paste0(x1, ".longitude"),
                          paste0(x2, ".latitude"), paste0(x2, ".longitude")), pattern = " ")[[1]]
  
  data.euc <- data[, .SD, .SDcols = cols]
  colnames(data.euc) <- c("date", "lat1", "long1", "lat2", "long2")
  
  data.euc <- data.euc[, euclidean := sqrt((lat1 - lat2)^2 + (long1 - long2)^2)][, 
                                 ":=" (lat1 = NULL, lat2 = NULL, long1 = NULL, long2 = NULL)]
  colnames(data.euc) <- c("date", paste0("euclidean.", outputname))
  data.euc
}

# this is to extract all measures of central tendency (lagemaße) such as min, max, quartiles, 
# range, mean and median for oth geopotential and mslp

# INPUT: - a data table in wide format (one row for each day) 
#        - first column has to be date, columns 2-161 mslp values amd columns 162-321 geopotential
#        - the years should have already been filtered

# OUTPUT: a data table with the above mentioned variables and the date  
#         -> dim(measures(data)) = nrow(data) x 15

measures <- function(data) {
  assertDataTable(data, null.ok = FALSE)
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
  quartile75.geopot <- apply(data.geopot, 1, function(x) quantile(x, probs = 0.75))
  
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

keepQuartiles <- function(data, variable = "mslp", quartiles) {
  assertDataTable(data, ncols = 160)
  assertString(variable)
  assertSubset(variable, choices = c("mslp", "geopot"))
  assertNumeric(quartiles, len = 2)
  
  # hoch <- data[, apply(data, 2, 
  #                      function(col) lapply(col, function(x) if (x < quartiles[2]) {x <-  NA} 
  #                                           else {x <- x}))]
  hoch <- copy(data)
  hoch[hoch < quartiles[2]] <- NA
  # tief <- data[, apply(data, 2, 
  #                      function(col) lapply(col, function(x) if (x > quartiles[1]) {x <-  NA} 
  #                                           else {x <- x}))]
  tief <- copy(data)
  tief[tief > quartiles[1]] <- NA
  
  list(hoch, tief)
}

# this is to calculate the intensity for high pressure and low pressure for both mslp and geopot

# INPUT: - a data table data.long in long format. By this, I mean the original one which ran through timeToWinter and
#          toDailyAverage in dataset Mutate, lines 35-50. So the column names are supposed to be 
#          c("date", "longitude", "latitude", "avg_mslp", "avg_geopot")
#        - a data table data.wide in wide format ( 321 cols)

# OUTPUT: a data table with 5 columns, first  one is the date, the others are  the intensity for 
#         high pressure and low pressure for both mslp and geopot

intensity <- function(data.wide, data.long) {
  assertDataTable(data.long, ncols = 5)
  assertDataTable(data.wide)
  assertSubset(colnames(data.long), choices = c("date", "longitude", "latitude", "avg_mslp", "avg_geopot"))
  
  quartiles.mslp <- quantile(data.long[, avg_mslp], probs = c(0.25, 0.75))
  quartiles.geopot <- quantile(data.long[, avg_geopot], probs = c(0.25, 0.75))
  
  date <- data.wide[, .(date)]
  
  #data <- toGeoIndex(data = data)
  #data <- longToWide(data = data)
  data.mslp <- data.wide[, 2:161]
  data.geopot <- data.wide[, 162:321]
  
  mslp <- keepQuartiles(data.mslp, variable = "mslp", quartiles = quartiles.mslp)
  geopot <- keepQuartiles(data.geopot, variable = "geopot", quartiles = quartiles.geopot)
  
  intensity.high.mslp <- apply(mslp[[1]], 1, function(x) sum(!is.na(x)))
  intensity.low.mslp <- apply(mslp[[2]], 1, function(x) sum(!is.na(x)))
  
  intensity.high.geopot <- apply(geopot[[1]], 1, function(x) sum(!is.na(x)))
  intensity.low.geopot <- apply(geopot[[2]], 1, function(x) sum(!is.na(x)))
  
  intensity <- data.table(date, intensity.high.mslp, intensity.low.mslp, 
                          intensity.high.geopot, intensity.low.geopot)
  intensity
}


#function for the mean of the quadrants per day
#Input log format datatable from append.QuadrantID
#outputs the date and means of the quadrants

quadrantMean <- function(data) {
  assertDataTable(data)
  out <- copy(data)
  out <- tryCatch(
    {
      assertSubset(c("date", "verID", "horID", "avg_mslp", "avg_geopot"),
                   names(data))
      
      out <- out[,  .(meanMslp = mean(avg_mslp), 
                      meanGeopot = mean(avg_geopot)), 
          by = .(date, verID, horID)]
      out[, ID := paste(verID, horID, sep = ".")][, ":=" (verID = NULL, horID = NULL)]
      
    }, error = function(cond) {
      assertSubset(c("date", "quadID", "avg_mslp", "avg_geopot"),
                   names(data))
      
      out <- out[,  .(meanMslp = mean(avg_mslp), 
                      meanGeopot = mean(avg_geopot)), 
                 by = .(date, quadID)]
      out[, ID := paste(quadID, sep = ".")][, ":=" (quadID = NULL)]
    }
  )
  out <- dcast(out, date ~ ID, value.var = c("meanMslp", "meanGeopot"))
  out
}


##function that return a certain number of principle components
getPCA <- function(data, number = 3) {
  assertDataTable(data)
  assertSubset("date", names(data))
  assertNumber(number, lower = 1, upper = ncol(data))
  
  data.table(date = data$date, 
        prcomp(as.data.frame(copy(data)[, date := NULL]))[["x"]][, seq(1, number)])
}

##create quadrant ids of 8x20
##finer min/max value location
diff_quadrandID <- function(data){
  assertDataTable(data)
  assertSubset(c("longitude", "latitude"), names(data))
  
  out <- copy(data)
  
  lats <- unique(sort(out$latitude))
  longs <- unique(sort(out$longitude))
  
  out[, ":=" (verID = vapply(latitude, function(x) which(lats == x), c(1)),
              horID = vapply(longitude, function(x) which(longs == x), c(1)))]
  
  out
}


##this function calculates the absolute difference over a day
## "range" :=
##for each measure location, it computes the absolute of the min measured at that 
##point on that day - the max value

## "coninuous" :=
## sums up change for every timeinstance to its next per location
## So the total change over a day

##sums up difference over all 160 locations per day

diffDay <- function(yearspan, variant = "range") {
  assertNumeric(yearspan, lower = 1900, upper = 2010)
  assertSubset(variant, c("range", "continuous"))
    
  source("dataset_mutate_funs.R")
  
  #load original dataset
  cli_data <- readRDS("Data/cli_data.rds")
  
  cli_data_30 <- cli_data[format(date, "%Y") %in% yearspan, ]
  
  #a bit dirty and repetetiv but ya know
  ifelse(variant == "range",
         {
  #mslp
  cli_30_mslp <- copy(cli_data_30)[, geopotential := NULL]
  wideTime_30_mslp <- longToWide(cli_30_mslp, id = c("date", "time"),
                                 col = c("longitude", "latitude"),
                                 vars = c("mslp"))
  cols <- names(wideTime_30_mslp)[-c(1,2)]
  maxChange_tile_day_mslp <- copy(wideTime_30_mslp)[, (cols) := lapply(.SD, function(x) max(x) - min(x)),
                                                    by = date, .SDcols = cols][, .SD[1], by = date][, time := NULL] 
  Change_day_mslp <- copy(maxChange_tile_day_mslp)[, .(diff = sum(.SD)), by = date, .SDcols = cols]
  names(Change_day_mslp) <- c("date", "diff_mslp")
  
  #geopot
  cli_30_geopot <- copy(cli_data_30)[, mslp := NULL]
  wideTime_30_geopot <- longToWide(cli_30_geopot, id = c("date", "time"),
                                   col = c("longitude", "latitude"),
                                   vars = c("geopotential"))
  cols <- names(wideTime_30_geopot)[-c(1,2)]
  maxChange_tile_day_geopotential <- copy(wideTime_30_geopot)[, (cols) := lapply(.SD, function(x) max(x) - min(x)),
                                                              by = date, .SDcols = cols][, .SD[1], by = date][, time := NULL] 
  Change_day_geopot <- copy(maxChange_tile_day_geopotential)[, .(diff = sum(.SD)), by = date, .SDcols = cols]
  names(Change_day_geopot) <- c("date", "diff_geopot")
         },
  {
    #mslp 2
    cli_30_mslp <- copy(cli_data_30)[, geopotential := NULL]
    wideTime_30_mslp <- longToWide(cli_30_mslp, id = c("date", "time"),
                                   col = c("longitude", "latitude"),
                                   vars = c("mslp"))
    cols <- names(wideTime_30_mslp)[-c(1,2)]
    maxChange_tile_day_mslp_var <- copy(wideTime_30_mslp)[, (cols) := lapply(.SD, function(x) sum(abs(x[1]- x[2]),
                                                                                                   abs(x[2]- x[3]),
                                                                                                   abs(x[3]- x[4]))),
                                                           by = date, .SDcols = cols][, .SD[1], by = date][, time := NULL] 
    Change_day_mslp <- copy(maxChange_tile_day_mslp_var)[, .(diff = sum(.SD)), by = date, .SDcols = cols]
    names(Change_day_mslp) <- c("date", "diff_mslp")
    
    #geopot
    cli_30_geopot <- copy(cli_data_30)[, mslp := NULL]
    wideTime_30_geopot <- longToWide(cli_30_geopot, id = c("date", "time"),
                                     col = c("longitude", "latitude"),
                                     vars = c("geopotential"))
    cols <- names(wideTime_30_geopot)[-c(1,2)]
    maxChange_tile_day_geopotential_var <- copy(wideTime_30_geopot)[, (cols) := lapply(.SD, function(x) sum(abs(x[1]- x[2]),
                                                                                                             abs(x[2]- x[3]),
                                                                                                             abs(x[3]- x[4]))),
                                                                     by = date, .SDcols = cols][, .SD[1], by = date][, time := NULL] 
    Change_day_geopot <- copy(maxChange_tile_day_geopotential_var)[, .(diff = sum(.SD)), by = date, .SDcols = cols]
    names(Change_day_geopot) <- c("date", "diff_geopot")
  })
  
  cbind(Change_day_mslp, diff_geopot = Change_day_geopot$diff_geopot)
}
