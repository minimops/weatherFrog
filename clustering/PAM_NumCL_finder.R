#function to find best cluster count for pam clustering

library(checkmate)
library(data.table)
library(ggplot2)
library(parallel)
library(parallelDist)

source("clustering/ClusterAssesmentHelper.R")

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
  
  jpeg(file= paste0("documentation/plots/PAMfinal/"
                    , fname, ".jpeg"))
  
  plot(range, sil_width,
       xlab = "Number of clusters",
       ylab = "Silhouette Width",
       main = paste("PAM", metric))
  lines(range, sil_width)
  
  dev.off()

  return(range[which(sil_width == max(sil_width))])
}


PAMhelper <- function(data, weights = c(rep(c(1/9, 1/9, 1/6, 1/6, 1/18, 1/18, 1/9, 
                                              1/9, 1/9), 2), 
                                        rep(1/6, 12), rep(1/18, 18)), 
                      metric = "euclidean", dist = TRUE, fname, range = 5:9, diss = FALSE) {
  
  ifelse(diss, assertMultiClass(data, c("dist", "matrix")),
         assertDataTable(data))
  assertNumeric(weights, null.ok = TRUE)
  assertSubset("date", colnames(data)[1])
  assertSubset(metric, choices = c("euclidean", "gower", "manhattan"))
  assertLogical(dist)
  assertString(fname)
  assertNumeric(range)
  
  ifelse(diss, 
         dissimilarity <- as.dist(data[, 2:ncol(data)], method = metric),
         {
  ifelse(metric == "gower",
    dissimilarity <- daisy(data[, 2:ncol(data)], metric = metric, weights = weights)
    ,
   {
      ifelse(is.null(weights),
          useDat <- data[, 2:ncol(data)],
          useDat <- data[, 2:ncol(data)][, Map("*", .SD, weights)])
      dissimilarity <- parallelDist(as.matrix(useDat), method = metric,
                                    threads = detectCores() - 2)
    }
  )
         })

  
  if (dist) {
    return(dissimilarity)
  }
  
  bestCNum <- bestClustNumber(dissimilarity, metric, fname, range)
  
  clusterAssesment(data, pam(dissimilarity, bestCNum, diss = TRUE), metric,
                   dissimilarity, fname)
}


clusterAssesment <- function(data, clusterRes, metric, distance, fname) {
  assert_class(distance, "dist")
  assertCharacter(fname)
  assertString(metric)
  
  path <- "documentation/plots/PAMfinal/"
  
  jpeg(file= paste0(path
                    , paste("mosaic", metric, fname, sep = "_"), ".jpeg"))
  par(mfrow=c(2,1))
  mosaic(as.data.table(data), clusterRes$clustering, title = paste(metric, fname))
  dev.off()
  
  jpeg(file= paste0(path
                    , paste("timeline", metric, fname, sep = "_"), ".jpeg"))
  
  capture.output(Cl.timeline(cbind(as.data.table(data), cluster = clusterRes$clustering),
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


weightDistMat <- function(weights, dist) {
  dist / sum(weights)
}




