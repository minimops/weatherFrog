#function to find best cluster count for pam clustering

library(checkmate)
library(data.table)
library(ggplot2)
library(parallel)
library(parallelDist)

bestClustNumber <- function(distMat, metric, fname) {
  assert_class(distMat, "dist")

  cl <- makeCluster(4)
  
  PamSilFun <- function(i, distM) {
    library(cluster)
    
    pam_fit <- pam(distM,
                   diss = TRUE,
                   k = i)
    pam_fit$silinfo$avg.width
  }
  
  sil_width <- unlist(clusterApply(cl, 9:12, PamSilFun, distM = distMat))
  stopCluster(cl)
  
  jpeg(file= paste0("documentation/plots/PAMtrial/"
                    , fname, ".jpeg"))
  
  plot(9:12, sil_width,
       xlab = "Number of clusters",
       ylab = "Silhouette Width",
       main = paste("PAM", metric))
  lines(9:12, sil_width)
  
  dev.off()

}


PAMhelper <- function(data, weights = c(rep(c(1/9, 1/9, 1/6, 1/6, 1/18, 1/18, 1/9, 
                                              1/9, 1/9), 2), 
                                        rep(1/6, 12), rep(1/18, 18)), 
                      metric = "euclidean", dist = TRUE, fname) {
  
  assertDataTable(data)
  assertNumeric(weights, null.ok = TRUE)
  assertSubset("date", colnames(data)[1])
  assertSubset(metric, choices = c("euclidean", "gower", "manhattan"))
  assertLogical(dist)
  assertString(fname)
  
  
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
  
  if (dist) {
    return(dissimilarity)
  }
  
  bestClustNumber(dissimilarity, metric, fname)
  
  return(dissimilarity)
}










