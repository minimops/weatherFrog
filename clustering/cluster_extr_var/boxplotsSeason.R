#### differences in values of extrapolate() for summer and winter season
library(data.table)

source("clustering/cluster_extr_var/f_extr_funs.R")
source("clustering/ClusterAssesmentHelper.R")

data <- extrapolate(seq(1971, 2000))

datagwl <- attachGwl(copy(data))
datagwl

separateBySeason <- function(data, Season = "Summer") {
  assertDataTable(data)
  assertSubset("date", names(data))
  assertString(Season)
  assertSubset(Season, choices = c("Summer", "Winter"))
  
  WS <- as.Date("2012-10-16", format = "%Y-%m-%d")
  SS <- as.Date("2012-04-16", format = "%Y-%m-%d")
  
  d <- as.Date(strftime(data$date, format = "2012-%m-%d"))
  d <- ifelse(d >= SS & d < WS, "Summer", "Winter")
  
  dataSeason <- copy(data)[, season := d]
  
  ifelse(Season == "Summer", return(dataSeason[season == "Summer"][, season := NULL]), 
                             return(dataSeason[season == "Winter"][, season := NULL]))
}

season <- getSeason2(datagwl$date)
datagwl <- datagwl[, season := season]
dataSummer <- separateBySeason(copy(data))
dataWinter <- separateBySeason(copy(data), "Winter")

################# just for saving the plots!!! ####################

getBoxplotSaved <- function(data, gwl = "all", variable = "mean.mslp") {
  assertDataTable(data)
  assertSubset(c("season", "gwl", "date"), names(data))
  assertCharacter(gwl)
  
  if (gwl == "all") {
    gwl <- unique(datagwl$gwl)
  }
  
  index <- which(names(datagwl) == variable)
  
  jpeg(paste0("seasons_", variable, ".jpeg"))
  par(mfrow = c(3, 5))
  for (i in seq_len(15)) {
    datagwlTry <- copy(datagwl)[gwl == gwl[i]]
    # datagwlSummer <- copy(datagwl)[gwl == i & season == "Summer"]
    boxplot(datagwlTry[[index]] ~ season, 
            data = datagwlTry, 
            xlab = paste0(gwl[i], " per Season"),
            ylab = variable)
    
  }
  dev.off()
  
  jpeg(paste0("seasons_", variable, "2.jpeg"))
  par(mfrow = c(3, 5))
  for (i in 16:30) {
    datagwlTry <- copy(datagwl)[gwl == gwl[i]]
    # datagwlSummer <- copy(datagwl)[gwl == i & season == "Summer"]
    boxplot(datagwlTry[[index]] ~ season, 
            data = datagwlTry, 
            xlab = paste0(gwl[i], " per Season"),
            ylab = variable)
    
  }
  dev.off()
  par(mfrow = c(1, 1))
}

for (var in names(datagwl)[2:49]) {
  getBoxplotSaved(copy(datagwl), variable = var)
}

############################## PLOTS SAVED #################################

# this function is to extract certain variables as boxplots per season

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
    boxplot(datagwlTry[[index]] ~ season, 
            data = datagwlTry, 
            xlab = paste0(gwl[i], " per Season"),
            ylab = variable)
  }
  
  par(mfrow = c(1, 1))
}

getBoxplot(copy(datagwl), variable = "median.mslp")

