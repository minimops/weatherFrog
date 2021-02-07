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


getEvaluation <- function(clustering, distance, data, title = "PAM", on = "sil") {
  assertDataTable(data)
  assertString(title)
  assertString(on)
  assertSubset(on, choices = c("sil", "timeline", "mosaic"))
  clusterVector <- clustering$clustering
  
  dataTimeline <- copy(data)[, cluster := clusterVector]
  
  ifelse(on == "sil", sil(clustering, clusterVector, distance, "pam"),
         ifelse(on == "timeline",  Cl.timeline(copy(dataTimeline)),
         mosaic(copy(data), clusterVector, title = title)))
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

diss.pam.gower.cat <- readRDS("Data/diss_pam_gower_cat.rds")
pam.gower.cat <- pam(diss.pam.gower.cat, diss = TRUE, k = 7)
# saveRDS(pam.gower.cat, "Data/pam.gower.cat.rds")
getEvaluation(pam.gower.cat, diss.pam.gower.cat, dataCategorial, "GOWER with CATEGORIAL QUADRANTS")


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

diss.pam.gower.cat.pca <- readRDS("Data/diss_pam_gower_cat_pca.rds")
pam.gower.cat.pca <- pam(diss.pam.gower.cat.pca, diss = TRUE, k = 8)
saveRDS(pam.gower.cat.pca, "Data/pam.gower.cat.pca.rds")


sil(pam.gower.cat.pca, pam.gower.cat.pca$clustering, diss.pam.gower.cat.pca, "pam")
data.gower.cat.pca <- copy(dataCategorialPCA)[, cluster := pam.gower.cat.pca$clustering]
Cl.timeline(copy(data.gower.cat.pca))
mosaic(copy(dataCategorialPCA), pam.gower.cat.pca$clustering, "Gower, PCA, Quadrant Categories")


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

diss.pam.manhat.weighted <- readRDS("Data/diss_pam_manhat_weighted.rds")
pam.manhat.preweighted <- pam(diss.pam.manhat.weighted, diss = TRUE, k = 8)
saveRDS(pam.manhat.preweighted, "Data/pam.manhat.preweighted.rds")

sil(pam.manhat.preweighted, pam.manhat.preweighted$clustering, diss.pam.manhat.weighted, "pam")
data.manhat.preweighted <- copy(data)[, cluster := pam.manhat.preweighted$clustering]
Cl.timeline(copy(data.manhat.preweighted))
mosaic(copy(data), pam.manhat.preweighted$clustering, "Manhattan, preweighted")


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

diss.pam.manhat.preweighted2 <- readRDS("Data/diss_pam_manhat_weighted2.rds")
pam.manhat.preweighted.diff <- pam(diss.pam.manhat.preweighted2, diss = TRUE, k = 7)
saveRDS(pam.manhat.preweighted.diff, "Data/pam.manhat.preweighted.diff.rds")

sil(pam.manhat.preweighted.diff, pam.manhat.preweighted.diff$clustering, diss.pam.manhat.preweighted2, "pam")
data.manhat.preweighted.diff <- copy(data)[, cluster := pam.manhat.preweighted.diff$clustering]
Cl.timeline(copy(data.manhat.preweighted.diff))
mosaic(copy(data), pam.manhat.preweighted.diff$clustering, "Manhattan, preweighted, diff")

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


diss.pam.euc.preweighted.diff <- readRDS("Data/diss_pam_euc_weighted2.rds")
pam.euc.preweighted.diff <- pam(diss.pam.euc.preweighted.diff, diss = TRUE, k = 5)
saveRDS(pam.euc.preweighted.diff, "Data/pam.euc.preweighted.diff.rds")

sil(pam.euc.preweighted.diff, pam.euc.preweighted.diff$clustering, diss.pam.euc.preweighted.diff, "pam")
data.euc.preweighted.diff <- copy(data)[, cluster := pam.euc.preweighted.diff$clustering]
Cl.timeline(copy(data.euc.preweighted.diff))
mosaic(copy(data), pam.euc.preweighted.diff$clustering, "Euclidean, preweighted, diff")


