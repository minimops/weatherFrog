#funs for dataset mutation

require(data.table)
require(checkmate)
require(stringr)


#subsets certain yearspan
#TODO this is such a mess
subsetYears <- function(data, years) {
  assertDataTable(as.data.table(data))
  assert_vector(years)
  tryCatch(assertSubset(years, seq(1900, 2010)),
           error = function(cond) {
             assertSubset(years, as.character(seq(1900, 2010)))
           })
  tryCatch({
    assertSubset("time", names(data))
    data[format(as.Date(substring(data$time, 1, 10)),"%Y") %in% years, ]
  },
  error = function(cond) {
    tryCatch(data[format(as.Date(date),"%Y") %in% years, ],
             error = function(cond) {
               data[year %in% years, ]
             })
  })
}

#sets all measuretimes to winter time
timeToWinter <- function(data) {
  assertDataTable(data)
  
  warning("WARNING: This function only subtracts 1 from odd measuring times. This is not okay for the entire dataset, as we have heard that the times are irregular!!!")
  data[time %in% seq(1, 23, by = 2), time := time - 1]
}

#averages mslp and geopot for each day
toDailyAverage <- function(data) {
  assertDataTable(data)
  
  data[, .(avg_mslp = mean(mslp), 
           avg_geopot = mean(geopotential)), 
       by = .(date, longitude, latitude)]
}

#removes longitude and latitude data for and replaces with index (1 to 160)
toGeoIndex <- function(data) {
  assertDataTable(data)
  out <- copy(data)
  setorder(data, date, longitude, latitude)
  out[, geoIndex := 1:.N, by = date][, ":=" (longitude = NULL, latitude = NULL)]
}

#take cli_data from long to wide format
longToWide <- function(data, id = "date", col = "geoIndex", vars = c("avg_mslp", "avg_geopot")) {
  assertDataTable(data)
  assertCharacter(id)
  assertCharacter(col)
  assertCharacter(vars)
  
  dcast(data, 
        paste(paste(id, collapse = "+"), "~", paste(col, collapse = "+")), 
        value.var = vars)
}