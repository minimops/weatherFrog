##### Distanzmatrizen ############

library(cluster)
library(data.table)


### please source f_extr_funs.R for this
source("clustering/Var_Extraction_Method/f_extr_funs.R")
source("clustering/ClusterAssesmentHelper.R")
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
  dataCate$maxMslp <- as.factor(dataCate$maxMslp)
  dataCate$minMslp <- as.factor(dataCate$minMslp)
  dataCate$maxGeopot <- as.factor(dataCate$minGeopot)
  dataCate$minGeopot <- as.factor(dataCate$maxGeopot)
  dataCate
}

### 1. Gower with categorial quadrants without PCA
data <- extrapolate(seq(1971, 2000))
#data <- extrapolate(seq(2000, 2010))
dataLong <- readRDS("Data/cli_data_30_avgDay.rds")

dataCategorial <- getCategorical(copy(dataLong), copy(data))
ncol(dataCategorial)
# should be 49 - 8 + 4 = 45
sapply(dataCategorial, class)


diss.pam.gower.cat <- dissimilarityPAM(copy(dataCategorial),
                                       weights = c(rep(c(1/9, 1/9, 1/6, 1/6, 1/18, 1/18, 1/9, 1/9, 1/9), 2), 
                                                   rep(1/8, 4), rep(1/18, 18), rep(1/8, 4)),
                                       metric = "gower",
                                       dist = FALSE)

saveRDS(PAMhelper(copy(dataCategorial),
                  weights = c(rep(c(1/9, 1/9, 1/6, 1/6, 1/18, 1/18, 1/9, 1/9, 1/9), 2), 
                              rep(1/8, 4), rep(1/18, 18), rep(1/8, 4)),
                  metric = "gower",
                  dist = FALSE,
                  fname = "PAM_gower_categorical_9-12"), 
        "diss.pam.gower.cat9-12.rds")


## WEIGHTS: die Gruppen mit Verteilung, mean in Quadranten so gelassen, euclidean und quadranten zusammengefasst

?daisy

### 2. GOWER with categorial quadrants and PCA
dataPCA <- extrapolate(seq(1971, 2000), "all.pca")

dataCategorialPCA <- getCategorical(copy(dataLong), copy(dataPCA))
ncol(dataCategorialPCA)
# should be 45 + 3 = 48

diss.pam.gower.cat.pca <- dissimilarityPAM(copy(dataCategorialPCA),
                                           weights = c(rep(c(1/9, 1/9, 1/6, 1/6, 1/18, 1/18, 1/9, 1/9, 1/9), 2), 
                                                       rep(1/8, 4), rep(1/21, 21), rep(1/8, 4)),
                                           metric = "gower",
                                           dist = FALSE)


saveRDS(PAMhelper(copy(dataCategorialPCA),
                  weights = c(rep(c(1/9, 1/9, 1/6, 1/6, 1/18, 1/18, 1/9, 1/9, 1/9), 2), 
                              rep(1/8, 4), rep(1/21, 21), rep(1/8, 4)),
                  metric = "gower",
                  dist = FALSE,
                  fname = "PAM_gower_categorical_pca_9-12"), 
        "diss.pam.gower.cat.pca9-12.rds")


### 3. MANHATTAN weighted in scaleNweight
diss.pam.manhat.weighted <- dissimilarityPAM(scaleNweight(copy(data), weight = TRUE),
                                             weights = NULL,
                                             metric = "manhattan",
                                             dist = FALSE)

saveRDS(PAMhelper(scaleNweight(copy(data), weight = TRUE),
          weights = NULL,
          metric = "manhattan",
          dist = FALSE,
          fname = "PAM_Manhattan_preweighted_default"), 
        "diss.pam.manhat.weighted.rds")



### 4. MANHATTAN weighted in scaleNweight, different weights (mean und min,max höher gewichtet als restliche verteilungssachen)
diss.pam.manhat.weighted2 <- dissimilarityPAM(scaleNweight(copy(data), 
                                                           weight = TRUE,
                                                           weights = c(rep(c(1/3, 1/6, rep(1/3, 2), rep(1/6, 5)), 2),
                                                                       rep(1/6, 12), rep(1/18,18))),
                                              weights = NULL,
                                              metric = "manhattan",
                                              dist = FALSE)

saveRDS(PAMhelper(scaleNweight(copy(data), 
                               weight = TRUE,
                               weights = c(rep(c(1/3, 1/6, rep(1/3, 2), rep(1/6, 5)), 2),
                                           rep(1/6, 12), rep(1/18,18))),
                  weights = NULL,
                  metric = "manhattan",
                  dist = FALSE,
                  fname = "PAM_Manhattan_preweighted_diff"), 
        "diss.pam.manhat.weighted2.rds")


### 5. MANHATTAN weighted in scaleNweight with PCA
diss.pam.manhat.30.allpca.weighted <- dissimilarityPAM(scaleNweight(copy(dataPCA), 
                                                                    weight = TRUE,
                                                                    weights = c(rep(c(1/9, 1/9, 1/6, 1/6, 
                                                                         1/18, 1/18, 1/9, 1/9, 1/9), 2), 
                                                                         rep(1/6, 12), rep(1/21, 21))),
                                                       weights = NULL,
                                                       dist = FALSE, 
                                                       metric = "manhattan")


### 6. EUCLIDEAN weighted in scaleNweight()
diss.pam.euc.weighted <- dissimilarityPAM(scaleNweight(copy(data), weight = TRUE), 
                                          weights = NULL,
                                          metric = "euclidean", 
                                          dist = FALSE)

saveRDS(PAMhelper(scaleNweight(copy(data), weight = TRUE), 
                  weights = NULL,
                  metric = "euclidean", 
                  dist = FALSE,
                  fname = "PAM_Euclid_preweighted_default"), 
        "diss.pam.euc.weighted.rds")

### 7. EUCLIDEAN weighted in daisy()
#TODO dont think this makes sense
diss.pam.euc.weighted2 <- dissimilarityPAM(scaleNweight(copy(data)), 
                                           weights = c(rep(c(1/9, 1/9, 1/6, 1/6, 1/18, 1/18, 1/9, 1/9, 1/9), 2), 
                                                       rep(1/6, 12), rep(1/18, 18)),
                                           metric = "euclidean",
                                           dist = FALSE)

saveRDS(PAMhelper(scaleNweight(copy(data)), 
                  weights = c(rep(c(1/9, 1/9, 1/6, 1/6, 1/18, 1/18, 1/9, 1/9, 1/9), 2), 
                              rep(1/6, 12), rep(1/18, 18)),
                  metric = "euclidean",
                  dist = FALSE,
                  fname = "PAM_Euclid_preweighted_within"), 
        "diss.pam.euc.weighted2.rds")


s <- daisy(scaleNweight(copy(data))[, date := NULL], 
           weights = c(rep(c(1/9, 1/9, 1/6, 1/6, 1/18, 1/18, 1/9, 1/9, 1/9), 2), 
                       rep(1/6, 12), rep(1/18, 18)),
           metric = "euclidean")

useDat <- scaleNweight(copy(data))[, date := NULL][, Map("*", .SD, weights)]
dissimilarity <- parallelDist(as.matrix(useDat), method = "euclidean",
                              threads = detectCores() - 2)

useDat <- scaleNweight(copy(data))[, date := NULL]
dissimilarity <- parallelDist(as.matrix(useDat), method = "euclidean",
                              threads = detectCores() - 2)

###8  EUCLIDEAN with the different weights

saveRDS(PAMhelper(scaleNweight(copy(data), 
                               weight = TRUE,
                               weights = c(rep(c(1/3, 1/6, rep(1/3, 2), rep(1/6, 5)), 2),
                                           rep(1/6, 12), rep(1/18,18))),
                  metric = "euclidean",
                  dist = FALSE,
                  fname = "PAM_Euclid_preweighted_diff"), 
        "diss.pam.euc.weighted2.rds")
