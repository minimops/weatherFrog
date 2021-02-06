#function to find best cluster count for pam clustering

library(checkmate)
library(ggplot2)
library(parallel)

bestClustNumber <- function(distMat) {
  assert_class(distMat, "dist")

  cl <- makeCluster(detectCores() - 1)
  
  PamSilFun <- function(i, distM) {
    library(cluster)
    
    pam_fit <- pam(distM,
                   diss = TRUE,
                   k = i)
    pam_fit$silinfo$avg.width
  }
  
  sil_width <- unlist(clusterApply(cl, 4:15, PamSilFun, distM = distMat))
  stopCluster(cl)
  
  plot(4:15, sil_width,
       xlab = "Number of clusters",
       ylab = "Silhouette Width",)
  lines(4:15, sil_width)
  print(sil_width)  
}










