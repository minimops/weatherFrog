#boosting

library(data.table)
library(checkmate)
library(parallel)
library(cluster)

clusterBoosting <- function(stepW, mstop, data, sampling = c(2, 3), 
                            start.weights = rep(0, ncol(data) -1 )) {
  assertNumber(stepW, lower = 10^-20, upper = 1)
  assertNumber(mstop, lower = 1, finite = TRUE)
  assertDataTable(data)
  assertSubset("date", names(data))
  assertNumeric(sampling, len = 2)
  
  if(length(start.weights) != ncol(data) -1) stop("start.weights must have same 
                                                  length as data has columns that aren't date")
  
  #year period
  timeframe <- as.numeric(unique(copy(data)[,
                          year := format(as.Date(date), "%Y")][, year]))
  #save for output
  iterations <- mstop
  #set up weight vector
  weights <- start.weights
  #scores over time
  scoreIT <- data.table(iteration = 0, score = 0, avgSil = 0, avgTLS = 0, stab = 0,
                        weights = list(weights))
  #run through iterations
  while(mstop > 0){
    years <- selectYears(timeframe, number = sampling[1], length = sampling[2])
    scores <- data.frame()
    cl <- makeCluster(detectCores())
    for (i in years) {
      #getData
      useDat <- scale(copy(data)[format(date, "%Y") %in% i, ][, date := NULL])
      scores <- rbind(scores, rbindlist(clusterApply(cl, x = seq(1, ncol(useDat)), 
                                           fun = clusterScorer, dat = useDat,
                                           w = weights, stepW = stepW)))
    }
    #clusterMap(cl, "+", t.y = c(1,1,1), w.i = c(3,4,5))
    stopCluster(cl)
    
    
    #attatch stability metric
    scores$V4 <- as.data.table(scores)[, V4 := (1/3) * (1 - (abs(range(V2)[1] - range(V2)[2]) +
                                                        abs(range(V3)[1] - range(V3)[2]))), by 
                                         = V1]$V4
    
    #calculate averages
    scores <- as.data.table(scores)[, .(V2 = mean(V2), V3 = mean(V3), V4 = mean(V4)), by = V1]
    
    
    #sum up scoreParams
    #names(scores) <- c("param", "avgSil")
    indexBest <- which(rowSums(scores[, 2:ncol(scores)]) == 
                         max(rowSums(scores[, 2:ncol(scores)])))
    scores$fscore <- rowSums(scores[, 2:ncol(scores)])
    #best scoring increment
    para2Weight <- as.numeric(scores[fscore == max(fscore), ][, 1])
    
    #increment parameter weight
    weights[para2Weight] <- weights[para2Weight] + stepW
    
    #record in timeline
    scoreIT <- rbind(scoreIT, data.table(iterations - mstop + 1,
                                         scores[fscore == max(fscore),
                                                          fscore],
                                         scores[indexBest, 2],
                                         scores[indexBest, 3],
                                         scores[indexBest, 4],
                                         list(weights)), use.names = F)
    
    print(paste("Iteration", iterations - mstop + 1, "with score:", 
                scores[fscore == max(fscore), fscore]))
    
    mstop <- mstop - 1
  }
  scoreIT
}


clusterScorer <- function(x, dat, w, stepW) {
  library(cluster)
  source("clustering/ClusterAssesmentHelper.R")
  #increment one column weight my c
  w[x] <- w[x] + stepW
  #weight matrix
  dat <- mapply("*", as.data.frame(dat), w)
  dist <- daisy(dat, metric = "manhattan")
  cluster <- pam(dist, 5, diss = TRUE)
  avgSil <- mean(silhouette(x = cluster$clustering, dist = dist)[, 3])
  
  runLengths <- rle(cluster$clustering)
  data <- data.table(table(runLengths$lengths))
  colnames(data) <- c("length", "count")
  data[, ":=" (length = as.numeric(length))]
  avgTLS <- TLS(data[, count := length * count])
  
  
  list(x, avgSil, avgTLS)
}

#timeperiod selection
#this function randomly selects "number" amount of timeperiods of "length" years
#out of the "timeframe" period
selectYears <- function(timeframe, number, length) {
  assertNumeric(timeframe, lower = 1901, upper = 2010)
  lapply(list(number, length), assertNumber)
  if(length(timeframe) < number * length) stop("timeframe too small to extract those samples")
  
  yearList <- list()
  tosample <- timeframe[-c(length(timeframe)-length+1:length(timeframe))]
  #sample first
  for (i in seq_len(number)) {
    year <- sample(tosample, 1)
    years <- seq(year, year + length - 1)
    yearList[[i]] <- years
    tosample <- tosample[-which(tosample %in% years)]
  }
  yearList
}

