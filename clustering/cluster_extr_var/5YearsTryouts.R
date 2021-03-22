# start with data 3 x 5 years

bestClustNumber <- function(distMat, metric, fname, range) {
  assert_class(distMat, "dist")
  assertNumeric(range)
  if(length(range) > 5) stop("A range greater than 5 will likely result 
                             in your pc crashing.")
  
  cl <- makeCluster(min(c(detectCores() - 1, length(range))))
  
  PamSilFun <- function(i, distM) {
    library(cluster)
    
    pam_fit <- pam(distM,
                   diss = TRUE,
                   k = i)
    pam_fit$silinfo$avg.width
  }
  
  sil_width <- unlist(clusterApply(cl, range, PamSilFun, distM = distMat))
  stopCluster(cl)
  
  jpeg(file= paste0("documentation/plots/PAMtrial/5YearsEuc/PCA/"
                    , fname, ".jpeg"))
  
  plot(range, sil_width,
       xlab = "Number of clusters",
       ylab = "Silhouette Width",
       main = paste("PAM", metric))
  lines(range, sil_width)
  
  dev.off()
  
  return(range[which(sil_width == max(sil_width))])
}

# from dataset mutate
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

## this is to save the datasets for the years 1971-1975, 1984-1988 and 1996-2000
# as they are needed in extrapolate()
dataLong <- readRDS("Data/cli_data_full_avgDay.rds")
dataWide <- readRDS("Data/cli_data_full_avgDay_wide.rds")

yearsList <- list(seq(1971, 1975), seq(1984, 1988), seq(1996, 2000), seq(2006, 2010))
names <- c("71", "84", "96", "05")

for (i in seq_len(3)) {
  dataYearsLong <- subsetYears(copy(dataLong), yearsList[[i]])
  saveRDS(dataYearsLong, paste0("Data/cli_data_", names[i], "_avgDay.rds"))
  
  dataYearsWide <- subsetYears(copy(dataWide), yearsList[[i]])
  saveRDS(dataYearsWide, paste0("Data/cli_data_", names[i], "_avgDay_wide.rds"))
}

# get 4 different 5 year datasets
# extraplotae mit all (also ganz normal)
dataList <- list()

for (i in seq_along(yearsList)) {
  dataList[[i]] <- extrapolate(yearsList[[i]], vars = "all")
}
names(dataList) <- names

# output: named list with four different datasets (different years)

dataListSummer <- list()
dataListWinter <- list()

for (i in seq_along(yearsList)) {
  dataListSummer[[i]] <- separateBySeason(copy(dataList[[i]]), "Summer")
  dataListWinter[[i]] <- separateBySeason(copy(dataList[[i]]), "Winter")
}


### for example like this
# save number of clusters for winter, summer and together

for(i in seq_along(dataListSummer)) {
  bestClustNumber(daisy(scaleNweight(copy(dataListSummer[[i]]))[, 2:ncol(dataListSummer[[i]])], metric = "manhattan"),
                  "manhattan", 
                  paste0(names[i], "Summer"), 
                  range = seq(5, 9))
}

for(i in seq_along(dataListWinter)) {
  bestClustNumber(daisy(scaleNweight(copy(dataListWinter[[i]]))[, 2:ncol(dataListWinter[[i]])], metric = "manhattan"),
                  "manhattan", 
                  paste0(names[i], "Winter"), 
                  range = seq(5, 9))
}

for(i in seq_along(dataList)) {
  bestClustNumber(daisy(scaleNweight(copy(dataList[[i]]))[, 2:ncol(dataList[[i]])], metric = "manhattan"),
                  "manhattan", 
                  names[i], 
                  range = seq(5, 9))
}


# and weighted:


for(i in seq_along(dataListSummer)) {
  bestClustNumber(daisy(scaleNweight(copy(dataListSummer[[i]]),
                                     weight = TRUE)[, 2:ncol(dataListSummer[[i]])], metric = "manhattan"),
                  "manhattan", 
                  paste0(names[i], "Summer_preweighted"), 
                  range = seq(5, 9))
}

for(i in seq_along(dataListWinter)) {
  bestClustNumber(daisy(scaleNweight(copy(dataListWinter[[i]]),
                                     weight = TRUE)[, 2:ncol(dataListWinter[[i]])], metric = "manhattan"),
                  "manhattan", 
                  paste0(names[i], "Winter_preweighted"), 
                  range = seq(5, 9))
}

for(i in seq_along(dataList)) {
  bestClustNumber(daisy(scaleNweight(copy(dataList[[i]]),
                                     weight = TRUE)[, 2:ncol(dataList[[i]])], metric = "manhattan"),
                  "manhattan", 
                  paste0(names[i], "_preweighted"), 
                  range = seq(5, 9))
}

## make clusters - unweighted


doPAM <- function(dataList, names = c("71", "84", "96", "05"), k = 5, weight = FALSE,
                  weights = c(rep(c(1/9, 1/9, 1/6, 1/6, 1/18, 1/18, 1/9, 
                                    1/9, 1/9), 2), 
                              rep(1/6, 12), rep(1/18, 18))) {
  assertList(dataList)
  
  pamList <- list()
  for(i in seq_along(dataList)) {
    pamList[[i]] <- pam(daisy(scaleNweight(copy(dataList[[i]]), 
                                           weight = weight,
                                           weights = weights)[, 2:ncol(dataList[[i]])], metric = "euclidean"), 
                        diss = TRUE, k = k)
  }
  names(pamList) <- names
  pamList
}


together_unweighted <- doPAM(dataList)
summer_unweighted <- doPAM(dataListSummer, names = c("71Summer", "84Summer", "96Summer", "05Summer"))
winter_unweighted <- doPAM(dataListWinter, names = c("71Winter", "84Winter", "96Winter", "05Winter"))

together_preweighted <- doPAM(dataList, weight = TRUE)
summer_preweighted <- doPAM(dataListSummer, names = c("71Summer", "84Summer", "96Summer", "05Summer"), weight = TRUE)
winter_preweighted <- doPAM(dataListWinter, names = c("71Winter", "84Winter", "96Winter", "05Winter"), weight = TRUE)


clusterAssesment <- function(data, clusterRes, metric, distance, fname) {
  assert_class(distance, "dist")
  assertCharacter(fname)
  assertString(metric)
  
  path <- "documentation/plots/PAMtrial/5YearsEuc/PCA/"
  
  jpeg(file= paste0(path
                    , paste("mosaic", metric, fname, sep = "_"), ".jpeg"))
  par(mfrow=c(2,1))
  mosaic(data, clusterRes$clustering, title = paste(metric, fname))
  dev.off()
  
  jpeg(file= paste0(path
                    , paste("timeline", metric, fname, sep = "_"), ".jpeg"))
  
  capture.output(Cl.timeline(cbind(data, cluster = clusterRes$clustering),
                             titleAdd = paste(metric, fname, sep = "_")), 
                 file = paste0(path, paste("output", metric, fname, sep = "_")),
                 append = TRUE)
  dev.off()
  
  jpeg(file= paste0(path
                    , paste("sil", metric, fname, sep = "_"), ".jpeg"))
  capture.output(sil(clusterRes, clusterRes$clustering, distance, "pam"),
                 file = paste0(path, paste("output", metric, fname, sep = "_")),
                 append = TRUE)
  dev.off()
}


ClusterAssessmentList <- function(pamList, dataList, metric, fname, weight = FALSE, 
                                  weights = c(rep(c(1/9, 1/9, 1/6, 1/6, 1/18, 1/18, 1/9, 1/9, 1/9), 2), 
                                              rep(1/6, 12), rep(1/18, 18))) {
  names <-  c("71", "84", "96", "05")
  for (i in seq_along(pamList)) {
    clusterAssesment(dataList[[i]], pamList[[i]], metric, 
                     distance = daisy(scaleNweight(copy(dataList[[i]]),
                                                   weight = weight, 
                                                   weights = weights)[, 2:ncol(dataList[[i]])], metric = "euclidean"),
                     paste0(names[i], fname))
  }
}

ClusterAssessmentList(together_unweighted, dataList, "euclidean", "_unweighted")
ClusterAssessmentList(summer_unweighted, dataListSummer, "euclidean", "Summer_unweighted")
ClusterAssessmentList(winter_unweighted, dataListWinter, "euclidean", "Winter_unweighted")

ClusterAssessmentList(together_preweighted, dataList, "euclidean", "_preweighted", weight = TRUE)
ClusterAssessmentList(summer_preweighted, dataListSummer, "euclidean", "Summer_preweighted", weight = TRUE)
ClusterAssessmentList(winter_preweighted, dataListWinter, "euclidean", "Winter_preweighted", weight = TRUE)

### Bewertungskriterien
# Summer
HBdiffSum <- c()
for (i in seq_len(4)) {
  HBdiffSum[i] <- mosaic(dataListSummer[[i]], summer_preweighted[[i]]$clustering)
}

mean(HBdiffSum)

for (i in seq_len(4)){
  Cl.timeline(data.table(date = dataListSummer[[i]]$date, cluster = summer_preweighted[[i]]$clustering))
}

tlSum <- c(0.1071803, -0.1440625, 0.08827791, 0.02821392)
mean(tlSum)

# Winter
HBdiffWin <- c()
for (i in seq_len(4)) {
  HBdiffWin[i] <- mosaic(dataListWinter[[i]], winter_preweighted[[i]]$clustering)
}

mean(HBdiffWin)

for (i in seq_len(4)){
  Cl.timeline(data.table(date = dataListWinter[[i]]$date, cluster = winter_preweighted[[i]]$clustering))
}

tlWin <- c(-0.03716875, 0.05666324, -0.133882, -0.02000162)
mean(tlWin)


# together
HBdiff <- c()
for (i in seq_len(4)) {
  HBdiff[i] <- mosaic(dataList[[i]], together_preweighted[[i]]$clustering)
}

mean(HBdiff)

for (i in seq_len(4)){
  Cl.timeline(data.table(date = dataList[[i]]$date, cluster = together_preweighted[[i]]$clustering))
}

tl <- c(0.2672315, 0.1813983, 0.2745528, 0.2976635)
mean(tl)

################## PCA ######################

dataListPCA <- list()

for (i in seq_along(yearsList)) {
  dataListPCA[[i]] <- extrapolate(yearsList[[i]], vars = "all.pca")
}
names(dataListPCA) <- names


names <- c("71PCA", "84PCA", "96PCA", "05PCA")
for(i in seq_along(dataListPCA)) {
  bestClustNumber(daisy(scaleNweight(copy(dataListPCA[[i]]))[, 2:ncol(dataListPCA[[i]])], metric = "euclidean"),
                  "euclidean", 
                  names[i], 
                  range = seq(5, 9))
}

for(i in seq_along(dataListPCA)) {
  bestClustNumber(daisy(scaleNweight(copy(dataListPCA[[i]]),
                                     weight = TRUE,
                                     weights = c(rep(c(1/9, 1/9, 1/6, 1/6, 1/18, 1/18, 1/9, 1/9, 1/9), 2), 
                                                 rep(1/6, 12), rep(1/21, 21)))[, 2:ncol(dataListPCA[[i]])], 
                  metric = "euclidean"),
                  "euclidean", 
                  paste0(names[i], "preweighted"), 
                  range = seq(5, 9))
}


togetherPCA_unweighted <- doPAM(dataListPCA, names = c("71PCA", "84PCA", "96PCA", "05PCA"))
ClusterAssessmentList(togetherPCA_unweighted, dataListPCA, "euclidean", "PCA_unweighted")

togetherPCA_preweighted <- doPAM(dataListPCA, names = c("71PCA", "84PCA", "96PCA", "05PCA"), weight = TRUE,
                                 weights = c(rep(c(1/9, 1/9, 1/6, 1/6, 1/18, 1/18, 1/9, 
                                                   1/9, 1/9), 2), 
                                             rep(1/6, 12), rep(1/21, 21)))
ClusterAssessmentList(togetherPCA_preweighted, dataListPCA, "euclidean", "PCA_preweighted", weight = TRUE,
                      weights = c(rep(c(1/9, 1/9, 1/6, 1/6, 1/18, 1/18, 1/9, 
                                        1/9, 1/9), 2), 
                                  rep(1/6, 12), rep(1/21, 21)))


######


pams <- c("together_unweighted", "together_preweighted", "summer_unweighted", "summer_preweighted",
          "winter_unweighted", "winter_preweighted", "togetherPCA_unweighted", "togetherPCA_preweighted")




source("clustering/Var_Extraction_Method/f_extr_funs.R")
#load change over day
change_day <- readRDS("Data/change_day.rds")

extr <- extrapolate(seq(1971, 2010))


dataChange <- copy(extr)[copy(change_day), ]


yearspans <- list(seq(1971, 1975), seq(1984, 1988), seq(1996, 2000), seq(2006, 2010))

source("clustering/PAM_NumCL_finder.R")
source("dataset_mutate_funs.R")

#plus diffday
for (span in yearspans) {
  years <- unlist(span)
  data <- subsetYears(dataChange, years)
  data <- cbind(date = data[, date], as.data.table(scale(copy(data)[, date := NULL])))
  PAMhelper(data, weights = NULL, dist = FALSE, metric = "manhattan",
            range = 5:9, fname = paste0("extr+change", years[1]))
  }

#just plain extr
for (span in yearspans) {
  years <- unlist(span)
  data <- subsetYears(extr, years)
  data <- cbind(date = data[, date], as.data.table(scale(copy(data)[, date := NULL])))
  PAMhelper(data, weights = NULL, dist = FALSE, metric = "manhattan",
            range = 5:9, fname = paste0("extr", years[1]))
}

#diffday plus pca
extrpca <- extrapolate(seq(1971, 2010), "all.pca")

dataChangepca <- copy(extrpca)[copy(change_day), ]

for (span in yearspans) {
  years <- unlist(span)
  data <- subsetYears(dataChangepca, years)
  data <- cbind(date = data[, date], as.data.table(scale(copy(data)[, date := NULL])))
  PAMhelper(data, weights = NULL, dist = FALSE, metric = "manhattan",
            range = 5:9, fname = paste0("extrPCA+change", years[1]))
}



#justfilter
distMat_30_date <- readRDS("Data/filter/distMat_30_date.rds")

library(zoo)

x <- as.Date(distMat_30_date[, ncol(distMat_30_date)])

for (span in yearspans[1:3]) {
  years <- unlist(span)
  index <- which(format(as.Date(x), "%Y") %in% years)
  data <- distMat_30_date[index, index]
  data <- cbind(date = x[index], data)
  PAMhelper(data, weights = NULL, dist = FALSE, metric = "manhattan",
            range = 5:9, fname = paste0("justfilter", years[1]), diss = TRUE)
}

distMat_05_date <- readRDS("clusData/filter/distMat_05_date.rds")
x2 <- as.Date(distMat_05_date[, ncol(distMat_05_date)])

years <- unlist(yearspans[4])
index <- which(format(as.Date(x2), "%Y") %in% years)
data <- distMat_05_date[index, index]
data <- cbind(date = x[index], data)
PAMhelper(data, weights = NULL, dist = FALSE, metric = "manhattan",
          range = 5:9, fname = paste0("justfilter", years[1]), diss = TRUE)



#filterattached simple addition
for (span in yearspans[1:3]) {
  years <- unlist(span)
  data <- subsetYears(extr, years)
  data <- cbind(date = data[, date], as.data.table(scale(copy(data)[, date := NULL])))
  dist1 <- as.matrix(PAMhelper(data, weights = NULL, dist = TRUE, metric = "manhattan",
            range = 5:9, fname = paste0("extr", years[1])))
  index <- which(format(as.Date(x), "%Y") %in% years)
  dist2 <- distMat_30_date[index, index]
  dist <- dist1 + dist2
  distIn <- cbind(date  = x[index], dist)
  PAMhelper(distIn, weights = NULL, dist = FALSE, metric = "manhattan",
            range = 5:9, fname = paste0("filterattached", years[1]), diss = TRUE)
}

years <- unlist(yearspans[4])
data <- subsetYears(extr, years)
data <- cbind(date = data[, date], as.data.table(scale(copy(data)[, date := NULL])))
dist1 <- as.matrix(PAMhelper(data, weights = NULL, dist = TRUE, metric = "manhattan",
                             range = 5:9, fname = paste0("extr", years[1])))
index <- which(format(as.Date(x2), "%Y") %in% years)
dist2 <- distMat_05_date[index, index]
dist <- dist1 + dist2
distIn <- cbind(date  = x[index], dist)
PAMhelper(distIn, weights = NULL, dist = FALSE, metric = "manhattan",
          range = 5:9, fname = paste0("filterattached", years[1]), diss = TRUE)




#all


extrpca <- extrapolate(seq(1971, 2010), vars = "all.pca")

dataChange2 <- copy(extrpca)[copy(change_day), ]

#filterattached simple addition
for (span in yearspans[1:3]) {
  years <- unlist(span)
  data <- subsetYears(dataChange2, years)
  data <- cbind(date = data[, date], as.data.table(scale(copy(data)[, date := NULL])))
  dist1 <- as.matrix(PAMhelper(data, weights = NULL, dist = TRUE, metric = "manhattan",
                               range = 5:9, fname = paste0("all", years[1])))
  index <- which(format(as.Date(x), "%Y") %in% years)
  dist2 <- distMat_30_date[index, index]
  dist <- dist1 + dist2
  distIn <- cbind(date  = x[index], dist)
  PAMhelper(distIn, weights = NULL, dist = FALSE, metric = "manhattan",
            range = 5:9, fname = paste0("allofit", years[1]), diss = TRUE)
}

years <- unlist(yearspans[4])
data <- subsetYears(dataChange2, years)
data <- cbind(date = data[, date], as.data.table(scale(copy(data)[, date := NULL])))
dist1 <- as.matrix(PAMhelper(data, weights = NULL, dist = TRUE, metric = "manhattan",
                             range = 5:9, fname = paste0("all", years[1])))
index <- which(format(as.Date(x2), "%Y") %in% years)
dist2 <- distMat_05_date[index, index]
dist <- dist1 + dist2
distIn <- cbind(date  = x[index], dist)
PAMhelper(distIn, weights = NULL, dist = FALSE, metric = "manhattan",
          range = 5:9, fname = paste0("allofit", years[1]), diss = TRUE)

