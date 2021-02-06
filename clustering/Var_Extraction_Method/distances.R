##### Distanzmatrizen ############

### please source f_extr_funs.R for this
# function that gives output as in extrapolate() but it has categorial values for the 
# quadrants of min/max MSlp/geopot
# INPUT: - a data table dataLong which is in log format
#        - a data table with output of extrapolate
# OUTPUT: data table with categories for hor/verID

getCategorical <- function(dataLong, data) {
  assertDataTable(dataLong)
  assertDataTable(data)
  assertSubset("date", names(data))
  assertSubset(c("latitude", "longitude", "date"), names(dataLong))
  
  dataQuadrant <- append.QuadrantID(copy(dataLong))
  dataQuadrant <- quadrantValues(copy(dataQuadrant), StringID = TRUE)
  dataQuadrant <- dataQuadrant[, ":=" ("maxMslp" = paste0(maxMslp.verChar, maxMslp.horChar),
                                       "minMslp" = paste0(minMslp.verChar, minMslp.horChar),
                                       "maxGeopot" = paste0(maxGeopot.verChar, maxGeopot.horChar),
                                       "minGeopot" = paste0(minGeopot.verChar, minGeopot.horChar))]
  cols <- c("date", "maxMslp", "minMslp", "maxGeopot", "minGeopot")
  dataQuadrant <- dataQuadrant[, .SD, .SDcols = cols]
  
  dataCate <- merge(data, dataQuadrant, by = "date")
  cols <- grep("ID", names(dataCate), value = TRUE)
  dataCate <- dataCate[, (cols) := NULL]
  dataCate
}

### 1. Gower with categorial quadrants without PCA
data <- extrapolate(seq(1971, 2000))
dataLong <- readRDS("Data/cli_data_30_avgDay.rds")

dataCategorial <- getCategorical(copy(dataLong), copy(data))
ncol(dataCategorial)
# should be 49 - 8 + 4 = 45

#### !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ####
diss.pam.gower.cat <- dissimilarityPAM(scaleNweight(copy(dataCategorial)),
                                       weights = c(rep(c(1/9, 1/9, 1/6, 1/6, 1/18, 1/18, 1/9, 1/9, 1/9), 2), 
                                                   rep(1/8, 4), rep(1/18, 18), rep(1/8, 4)),
                                       metric = "gower",
                                       dist = FALSE)
## WEIGHTS: die Gruppen mit Verteilung, mean in Quadranten so gelassen, euclidean und quadranten zusammengefasst
#### !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ####


### 2. GOWER with categorial quadrants and PCA
dataPCA <- extrapolate(seq(1971, 2000), "all.pca")

dataCategorialPCA <- getCategorical(copy(dataLong), copy(dataPCA))
ncol(dataCategorialPCA)
# should be 45 + 3 = 48

diss.pam.gower.cat.pca <- dissimilarityPAM(scaleNweight(copy(dataCategorialPCA)),
                                           weights = c(rep(c(1/9, 1/9, 1/6, 1/6, 1/18, 1/18, 1/9, 1/9, 1/9), 2), 
                                                       rep(1/8, 4), rep(1/21, 21), rep(1/8, 4)),
                                           metric = "gower",
                                           dist = FALSE)


### 3. try EUCLIDEAN 
diss.pam.euc <- dissimilarityPAM(scaleNweight(copy(data)), 
                                 weights = NULL, 
                                 metric = "euclidean", 
                                 dist = FALSE)

### 4. EUCLIDEAN weighted in scaleNweight()
diss.pam.euc.weighted <- dissimilarityPAM(scaleNweight(copy(data), weight = TRUE), 
                                          metric = "euclidean", 
                                          dist = FALSE)

### 5. EUCLIDEAN weighted in daisy()
diss.pam.euc.weighted2 <- dissimilarityPAM(scaleNweight(copy(data)), 
                                           weights = c(rep(c(1/9, 1/9, 1/6, 1/6, 1/18, 1/18, 1/9, 1/9, 1/9), 2), 
                                                       rep(1/6, 12), rep(1/18, 18)),
                                           metric = "euclidean",
                                           dist = FALSE)
