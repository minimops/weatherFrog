#### differences in values of extrapolate() for summer and winter season

datagwl <- attachGwl(copy(data))
datagwl

getSeason2 <- function(dates) {
  WS <- as.Date("2012-10-16", format = "%Y-%m-%d")
  SS <- as.Date("2012-04-16", format = "%Y-%m-%d")
  
  d <- as.Date(strftime(dates, format = "2012-%m-%d"))
  
  ifelse(d >= SS & d < WS, "Summer", "Winter")
}

season <- getSeason2(datagwl$date)
datagwl <- datagwl[, season := season]

getBoxplot <- function(data, gwl = "all", variable = "mean.mslp") {
  assertDataTable(data)
  assertSubset(c("season", "gwl", "date"), names(data))
  assertCharacter(gwl)
  
  if (gwl == "all") {
    gwl <- unique(datagwl$gwl)
  }
  
  index <- which(names(datagwl) == variable)
  par(mfrow = c(3, 5))
  
  for (i in seq_along(gwl)) {
    datagwlTry <- copy(datagwl)[gwl == gwl[i]]
    # datagwlSummer <- copy(datagwl)[gwl == i & season == "Summer"]
    boxplot(datagwlTry[[index]] ~ season, 
            data = datagwlTry, 
            xlab = paste0(gwl[i], " per Season"),
            ylab = variable)
  }
  par(mfrow = c(1, 1))
}

getBoxplot(copy(datagwl), variable = "median.mslp")

