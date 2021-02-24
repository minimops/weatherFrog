#boosting

library(data.table)
library(checkmate)
library(parallel)
library(cluster)

clusterBoosting <- function(stepW, mstop, data, sampling = c(2, 3)) {
  assertNumber(stepW, lower = 10^-20, upper = 1)
  assertNumber(mstop, lower = 1, finite = TRUE)
  assertDataTable(data)
  assertSubset("date", names(data))
  assertNumeric(sampling, len = 2)
  
  #year period
  timeframe <- as.numeric(unique(copy(data)[,
                          year := format(as.Date(date), "%Y")][, year]))
  #save for output
  iterations <- mstop
  #set up weight vector
  weights <- rep(0.01, ncol(data) - 1)
  #scores over time
  scoreIT <- data.table(iteration = 0, score = 0, weights = list(weights))
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
    
    #sum up scoreParams
    #names(scores) <- c("param", "avgSil")
    scores$fscore <- rowSums(scores[, 2:ncol(scores)])
    #best scoring increment
    para2Weight <- as.numeric(scores[fscore == max(fscore), ][, 1])
    
    #increment parameter weight
    weights[para2Weight] <- weights[para2Weight] + stepW
    
    #record in timeline
    scoreIT <- rbind(scoreIT, data.table(iteration = iterations - mstop + 1,
                                         score = scores[fscore == max(fscore),
                                                          fscore],
                                         weights = list(weights)))
    
    print(paste("iteration", iterations - mstop + 1, "with score:", 
                scores[fscore == max(fscore), fscore]))
    
    mstop <- mstop - 1
  }
  scoreIT
}


clusterScorer <- function(x, dat, w, stepW) {
  library(cluster)
  #increment one column weight my c
  w[x] <- w[x] + stepW
  #weight matrix
  dat <- mapply("*", as.data.frame(dat), w)
  dist <- daisy(dat, metric = "manhattan")
  cluster <- pam(dist, 5, diss = TRUE)
  avgSil <- mean(silhouette(x = cluster$clustering, dist = dist)[, 3])
  list(x, avgSil)
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

