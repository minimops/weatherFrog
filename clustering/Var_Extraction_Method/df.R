#### DF zusammenfassung



df <- matrix(NA, nrow = 32, ncol = 6)

year <- c("71", "84", "96", "05")
rnames <- list()

for (i in seq_along(year)){
  vec <- c(year[i], 
           paste0(year[i], "_preweighted"),
           paste0(year[i], "Summer"),
           paste0(year[i], "Summer_preweighted"),
           paste0(year[i], "Winter"),
           paste0(year[i], "Winter_preweighted"),
           paste0(year[i], "PCA"),
           paste0(year[i], "PCA_preweighted"))
  rnames[[i]] <- vec
}

rnamesVec <- unlist(rnames)
rownames(df) <- unlist(rnames)
colnames(df) <- c("cluster", "silhouette", "TlDay1", "TlDay2", "TlDay3", "TlLastDay")
df[, 1] <- 5

silIntoDF <- function(pamList, df, a = 1) {
  names <-  c("71", "84", "96", "05")
  
  a <- a
  for (i in seq_along(names)) {
   
    df[a, 2] <- pamList[[i]]$silinfo$avg.width
    a <- a + 8
  }
  df
}
pams <- c(together_unweighted, together_preweighted, summer_unweighted, summer_preweighted,
          winter_unweighted, winter_preweighted, togetherPCA_unweighted, togetherPCA_preweighted)

df <- silIntoDF(together_unweighted, df, 1)
df <- silIntoDF(together_preweighted, df, 2)
df <- silIntoDF(summer_unweighted, df, 3)
df <- silIntoDF(summer_preweighted, df, 4)
df <- silIntoDF(winter_unweighted, df, 5)
df <- silIntoDF(winter_preweighted, df, 6)
df <- silIntoDF(togetherPCA_unweighted, df, 7)
df <- silIntoDF(togetherPCA_preweighted, df, 8)


timeline <- function(dataList, pamList, df, a) {
  output <- list()
  a <- a
  for (i in seq_along(dataList)) {
    data <- dataList[[i]][, cluster := pamList[[i]]$clustering]
    use <- data.table(ClustID = copy(data)[[as.character("cluster")]],
                      date = copy(data)[["date"]])
    setorder(use, date)
    
    runLengths <- rle(use[["ClustID"]])
    
    output[[i]] <- c(table(runLengths$lengths)["1"], table(runLengths$lengths)["2"], 
                     table(runLengths$lengths)["3"], max(runLengths$lengths))
    
    df[a, 3] <- output[[i]][1]
    df[a, 4] <- output[[i]][2]
    df[a, 5] <- output[[i]][3]
    df[a, 6] <- output[[i]][4]
    a <- a + 8
  }
  df
}
df <- timeline(copy(dataList), together_unweighted, df, 1)
df <- timeline(copy(dataList), together_preweighted, df, 2)
df <- timeline(copy(dataListSummer), summer_unweighted, df, 3)
df <- timeline(copy(dataListSummer), summer_preweighted, df, 4)
df <- timeline(copy(dataListWinter), winter_unweighted, df, 5)
df <- timeline(copy(dataListWinter), winter_preweighted, df, 6)
df <- timeline(copy(dataListPCA), togetherPCA_unweighted, df, 7)
df <- timeline(copy(dataListPCA), togetherPCA_preweighted, df, 8)
df



dt <- as.data.table(df)
dt <- dt[, names := rnamesVec][, .(names, cluster, silhouette, TlDay1, TlDay2, TlDay3, TlLastDay)]
dt <- dt[, id := rep(1:8, 4)]

cols <- c("silhouette", "TlDay1", "TlDay2", "TlDay3", "TlLastDay")
dt2 <- unique(dt[, ":=" (avg.sil = mean(silhouette),
                 avg.day1 = mean(TlDay1),
                 avg.day2 = mean(TlDay2),
                 avg.day3 = mean(TlDay3),
                 avg.Last = mean(TlLastDay)), by = id][, c(2, 9:13)])
dt2 <- dt2[, names := c("together unweighted", "together preweighted", "summer unweighted", "summer preweighted",
                        "winter unweighted", "winter preweighted", "togetherPCA unweighted", "togetherPCA preweighted")]
dt2
saveRDS(dt2, "Data/manhattanSummaryAvg.rds")
saveRDS(df, "Data/manhattanSummary.rds")
