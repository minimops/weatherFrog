
library(checkmate)
library(data.table)
library(ggplot2)
library(cluster)
library(factoextra)
library(ggmosaic)

#function to attach gwl to a dataset with date column
attachGwl <- function(data) {
  assertDataTable(data)
  assertSubset("date", names(data))
  
  gwls <- readRDS("Data/gwl.rds")
  
  merge(data, gwls)
}

#function that prints table of occurence freq of lengths and plots them
#data input has to be a dt with date and id column
#cluster input is used to identify the id column
#title add input can be used to add smth to the plot title
Cl.timeline <- function(data, cluster = "cluster", titleAdd = "", seperated = FALSE,
                        cut = 90, multiplied = FALSE, showOpt = FALSE) {
  assertDataTable(data)
  assertString(cluster)
  assertString(titleAdd)
  assertSubset(c("date"), names(data))
  assertLogical(seperated)
  assertLogical(multiplied)
  
  if(showOpt && !multiplied) stop("Optimal Distribution only works in multiplied mode.")
  
  #this is next level stupid,i cant figure out a different way to extract the
  #cluster column while leaving it a variable
  use <- data.table(ClustID = copy(data)[[as.character(cluster)]],
               date = copy(data)[["date"]])
  setorder(use, date)
  print("Table of Cluster frequencies:")
  print(table(use$ClustID))
  if(seperated){
           runLengths <- rle(use[["ClustID"]])
           plots <- list()
           for (i in unique(use[["ClustID"]])) {
             
             length.runLengths.part <- runLengths$lengths[which(runLengths$values == i)]
             print(paste("distribution of runLengths", "Cluster:", i))
             print(table(length.runLengths.part))
             
             data <- data.table(table(length.runLengths.part))
             colnames(data) <- c("length", "count")
             
             # print(Tl.weight.fun(data))
             
           
             #data2 <- data[order(as.numeric(length))]
             dataOver30 <- copy(data)[as.numeric(length) > 30]
             cutoffs <- sum(as.numeric(dataOver30$length) * dataOver30$count)
             colVector <- RColorBrewer::brewer.pal(8, "Set1")
             
             p <- ggplot(as.data.frame(data), 
                    aes(x= as.numeric(length), y = count)) +
               geom_col(fill = colVector[i], col = "black") +
               labs(x = "Länge", 
                    title = paste("Cluster ", i),
                    y = "Anzahl") +
               ylim(0, 150) +
               scale_x_continuous(limits = c(0, cut + 1), breaks = seq(0, cut, by = ifelse(cut == 90, 5, 2))) +
               theme_bw() +
               geom_text(x = ifelse(cut == 90, cut - 15, cut - 10), y = 125, 
                         label= ifelse(cut == 90, "", paste0(cutoffs, " Tage abgeschnitten")),
                         size = 4) +
               theme(axis.title.x = element_blank(),
                     axis.title.y = element_blank()
                     )
             plots[[i]] <- p
           }
           grid.arrange(grobs = plots, left = "Anzahl", bottom = "Länge")
         } else{
          runLengths <- rle(use[["ClustID"]])
          
          print("distribution of runLengths:")
          print(table(runLengths$lengths))
          
          data <- data.table(table(runLengths$lengths))
          colnames(data) <- c("length", "count")
          data[, ":=" (length = as.numeric(length))]
          
          # print("timeline Value:")
          # print(Tl.weight.fun(data))
          
          print("New Timeline Value:")
          print(TLS(copy(data)[, count := length * count]))
          
          if (cluster == "cluster") {
            mainAdd <- "Cluster"
          }
          else {
            mainAdd <- "GWL"
          }
          ylab <- "Anzahl"
          if(multiplied){
            data[, count := count * length]
            ylab <- "Anzahl Tage"
          }
          ifelse(max(data$count) > 700, upperYlim <- 1000,
                 upperYlim <- 700)
          
          ifelse(showOpt,
                 {
                   ##timeline Verteilung
                   dec_fun <- function(x) {
                     if(x < 3 || x >= 40) {
                       return(0)
                     } else{
                       if(x < 13){
                         return(1)
                       }
                     }
                     return((23 / x) - (44 / x^2) - 0.55)
                   }
                   count <-  seq(1, cut)
                   TL.distr <- data.table(Anteil = vapply(count, dec_fun, FUN.VALUE = numeric(1)),
                                          count)
                   
                   TL.distr <- data.frame(TL.distr[, Anteil := nrow(use) * (Anteil / sum(TL.distr$Anteil))])
                   
                   return(ggplot(as.data.frame(data), 
                          aes(x= as.numeric(length), y = count)) +
                     geom_col(col = "black", fill = "gray77") +
                     geom_line(data = TL.distr, aes(x = count, y = Anteil), 
                               color = "red") + 
                     labs(x = "Länge", 
                          title = paste("Timeline"),
                          y = ylab) +
                     ylim(0, upperYlim) +
                       theme(legend.title = element_text(size=8)) +
                     
                     scale_x_continuous(breaks = c(1, seq(5, cut, by = 5)),
                                        limits = c(0, cut)) +
                     
                     theme_bw()
                   )
                 },
                 {
                   return(ggplot(as.data.frame(data), 
                          aes(x= as.numeric(length), y = count)) +
                     geom_col(col = "black", fill = "gray77") +
                     labs(x = "Länge", 
                          title = paste("Timeline"),
                          y = ylab) +
                     ylim(0, upperYlim) +
                     
                     scale_x_continuous(breaks = c(1, seq(5, cut, by = 5)),
                                        limits = c(0, cut)) +
                     
                     theme_bw()
                   )
                 }
                 )
          
         }
}


# this function is to get the silhouette coefficient. 
# INPUT: - cluster.fittet: Result of a clustering
#        - cluster.vector: the clustering vector of the fittet cluster, normally it is either
#                          cluster.fittet$cluster or cluster.fittet$clustering
#        - distance: an object of class "dist", so for example with dist(...) or daisy(...)
#        - algorithm: method you have chosen. Fuzzy is a bit different and I dont know what the density based
#                     clustering will be like

# OUTPUT: exact mean value of silhouette width and output of fviz_silhouette which is the silhouette width 
#         and a plot


sil <- function(cluster.fitted, cluster.vector, distance, algorithm) {
  assertNumeric(cluster.vector)
  assertString(algorithm)
  assertSubset(algorithm, choices = c("pam", "kmeans", "fuzzy", "distribution"))
  
  if (algorithm == "fuzzy") {
    fviz_silhouette(cluster.fitted)
  }
  sil <- silhouette(x = cluster.vector, dist = distance)
  print(mean(sil[, 3]))
  output <- fviz_silhouette(sil.obj = sil, print.summary = FALSE, palette = "Set1",
                  main = "Silhouettenplot", 
                  submain = paste0("Silhouettenkoeffizient: ", round(mean(sil[, 3]), 3)),
                  legend.title = "Cluster") + theme_classic() +
                  labs(y = "Silhouette S(o)") +
                  theme(axis.text.x = element_blank(),
                        axis.text.x.bottom = element_blank(),
                        axis.ticks.x = element_blank())
   
  output$layers[[2]]$aes_params$colour <- "black"
  output
}


# an example:
#sil(pam_fit, pam_fit$clustering, dissimilarity, "pam")



# function to scale and weight the data.
# weighting is a bit difficult because normally it should be done in a clustering or dissimilarity function
# and not by multiplying the weight and the variable.
# INPUT: - data in format of extrapolate(seq(x-y)), or with selected variables
#        - weight: logical whether oe wants the output to be weighted or not
#        - weights: the weights for the variables, right now for data input of extrapolate(seq(x, y), vars = "all)
#                   if the input data has less variables, the weights vector must be adjusted
#                   date is not included in weighting, so length(weights) == ncol(data)-1

# OUTPUT: a data table with same dimensions as input data, either scaled and weighted or just scaled

scaleNweight <- function(data, weight = FALSE, weights = c(rep(c(1/9, 1/9, 1/6, 1/6, 1/18, 1/18, 1/9, 1/9, 1/9), 2), 
                                                           rep(1/6, 12), rep(1/18, 18))) {
  assertDataTable(data)
  assertSubset("date", colnames(data)[1])
  assertLogical(weight)
  assertNumeric(weights, null.ok = TRUE)
  
  date <- data[, .(date)]
  cols <- colnames(data)[2:ncol(data)]
  
  data.scaled <- copy(data)[, (cols) := lapply(.SD, scale), .SDcols = cols]
  
  if (!weight) {
    return(data.scaled)
  }  
  
  data.weighted <- as.data.frame(matrix(0, ncol = ncol(data)-1, nrow = nrow(data)))
  for (i in 1:(ncol(data) - 1)) {
    data.weighted[, i] <- data.scaled[, .SD, .SDcols = cols[i]] * weights[i]
  }
  
  data.weighted <- data.table(date, data.weighted)
  colnames(data.weighted) <- c("date", cols)
  data.weighted
}


# function that print mosaicplots
# INPUT: - data with date 
#        - clustering vector of clusters
#        - title of plots, input is the used method

mosaic <- function(data, cluster_vector, title = "PAM") {
  assertDataTable(data)
  assertInteger(cluster_vector)
  assertString(title)
  assertSubset("date", colnames(data))
  
  gwl <- readRDS("Data/gwl.rds")
  data.gwl <- gwl[data, on = .(date)]
  data.gwl.cluster <- data.gwl[, cluster := cluster_vector]
  
  print(HB.diff.index(data.gwl.cluster))
  
  # mosaicplot(table(data.gwl.cluster$cluster, data.gwl.cluster$gwl), color = TRUE,
  #            xlab = "Cluster", ylab = "GWL", cex.axis = 0.6, las = 2,
  #            main = paste0(title, " Cluster - GWL"))
  # mosaicplot(table(data.gwl.cluster$gwl, data.gwl.cluster$cluster), color = TRUE,
  #            ylab = "Cluster", xlab = "GWL", cex.axis = 0.6, las = 2,
  #            main = paste0(title, " Cluster - GWL"))
  
  ggplot(data = data.gwl.cluster) +
    geom_mosaic(aes(x = product(cluster, gwl), fill = cluster), offset = 0.005) +
    theme_classic() +
    ggtitle("Mosaikplot für Cluster ~ GWL") +
    labs(x = "GWL") +
    scale_fill_brewer(palette = "Set1") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    guides(fill=guide_legend(title = "Cluster", reverse = TRUE)) +
    theme(axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          axis.line.y = element_blank())
  
}


#function that classifies noise for soft clustering methods
#observations with to low probability or ultiple high probabilities
#are classified as noise
#inputs are the clusterid vector of a cluster result 
#and a cluster probability matrix
#outputs new clusterid vector
noiseAllocation <- function(cluster.id, cluster.prob) {
  assertMatrix(cluster.prob)
  assertNumeric(cluster.id, len = nrow(cluster.prob))
  
  #rows where no probability is greater than 35%
  low.Prob.rows <- which(!apply(cluster.prob, 1, function(r) any(r > 0.35)))
  #rows where probility greater than 50percent to more than one cluster
  mult.high.Prob.rows <- which(apply(cluster.prob, 1, function(r) sum(r > 0.5)) > 1)
  
  #give these observations the cluster id 99 
  #TODO maybe NA instead of 99?
  cluster.id[unique(c(low.Prob.rows, mult.high.Prob.rows))] <- 99
  
  cluster.id
}


#MANOVA

# data: data frame/data table, that are used for clustering, for example our extract_varaibles_data
#       only contains the variable "date" and variables, that have been clustered
# cluster_vector: cluster vector of the cluster solution, usually cluster_object$cluster or something like that
# data_variable: column name of the date variable, default is "date"

manova.fun <- function(data,cluster_vector,date_variable = date){
  
  # data preparation
  cluster <- cluster_vector
  data_for_manova <- cbind(cluster,data)
  data_for_manova <- subset(data_for_manova, select = -date)
  
  # calculating manova
  man <- manova(as.matrix(data_for_manova[,-1]) ~ data_for_manova$cluster)
  print(man)
  #print(summary(man, tol = 0,fit = "Wilks"))
  man.aov <- summary.aov(man)
  
  
  # gather the output og summary.aov in one data.frame
  res_mat <- as.data.frame(matrix(ncol = 5))
  res_mat_residuals <- as.data.frame(matrix(ncol = 5))
  for ( i in seq_len(ncol(data_for_manova)-1)){
    res_mat[i,] <- man.aov[[i]][1,]
    res_mat_residuals[i,] <- man.aov[[i]][2,]
  }
  colnames(res_mat) <- colnames(man.aov[[1]])
  res_mat_residuals <- res_mat_residuals[,-c(4,5)]
  colnames(res_mat_residuals) <- c("residuals_df","residuals_sum_sq","residuals_mean_sq")
  rownames(res_mat) <- seq(1,ncol(data_for_manova)-1,by = 1)
  rownames(res_mat_residuals) <- seq(1,ncol(data_for_manova)-1,by = 1)
  variable <- colnames(data_for_manova[,-1])
  res_mat <- cbind(variable,res_mat,res_mat_residuals)
  
  # creating a colum, that shows, if a single variable contributes to the cluster solution/is significant
  # if p < alpha = 0.05 : yes, significant if p > alpha: no / not significant
  
  res_mat$significance <- "NA"
  for(i in seq_len(nrow(res_mat))) {
    if(res_mat[i,6] < 0.05){
      res_mat[i,10] = "yes"
    }
    else{
      res_mat[i,10] = "no"
    }
  }
  
  return(res_mat)
}


# function to separate the data by season summer or winter
# INPUT: - data table with cloumn date 
#        - string saying either Summer or Winter
# OUTPUT: - a data table which is separated by season

separateBySeason <- function(data, Season = "Summer") {
  assertDataTable(data)
  assertSubset("date", names(data))
  assertString(Season)
  assertSubset(Season, choices = c("Summer", "Winter"))
  
  WS <- as.Date("2012-10-16", format = "%Y-%m-%d")
  SS <- as.Date("2012-04-16", format = "%Y-%m-%d")
  
  d <- as.Date(strftime(data$date, format = "2012-%m-%d"))
  d <- ifelse(d >= SS & d < WS, "Summer", "Winter")
  
  dataSeason <- copy(data)[, season := d]
  
  ifelse(Season == "Summer", return(dataSeason[season == "Summer"][, season := NULL]), 
         return(dataSeason[season == "Winter"][, season := NULL]))
}

separateBy4Season <- function(data) {
  assertDataTable(data)
  assertSubset("date", names(data))
  
  WS <- as.Date("2012-12-01", format = "%Y-%m-%d")
  SpS <- as.Date("2012-03-01", format = "%Y-%m-%d")
  SS <- as.Date("2012-06-01", format = "%Y-%m-%d")
  AS <- as.Date("2012-09-01", format = "%Y-%m-%d")
  
  d <- as.Date(strftime(data$date, format = "2012-%m-%d"))
  d <- ifelse(d >= SS & d < AS, "Sommer", 
              ifelse(d >= AS & d < WS, "Herbst", 
                     ifelse(d >= SpS & d < SS, "Frühling", "Winter")))
  
  dataSeason <- copy(data)[, season := d]
  
  return(dataSeason)
}


Tl.weight.fun <- function(timeline){
  assertDataTable(timeline)
  
  #weights
  x <- data.table(length = seq(1, 30), 
                  weight = c(0,0, seq(3, 9), seq(9, 0, by = -9/20)))
  #plot(x)
  
  data <- timeline[length <= 30, ]
  
  joined <- x[data, on = "length"]
  sum(joined$weight * joined$count)
}

TLS <- function(timelineMultiplied) {
  assertDataTable(timelineMultiplied)
  
  ##timeline Verteilung
  dec_fun <- function(x) {
    if(x < 3 || x >= 40) {
      return(0)
    } else{
      if(x < 13){
        return(1)
      }
    }
    return((23 / x) - (44 / x^2) - 0.55)
  }
  x <-  seq(1, max(40, timelineMultiplied$length))
  TL.distr <- data.table(Anteil = vapply(x, dec_fun, FUN.VALUE = numeric(1)),
                         length = x)
  TL.distr[, Anteil := Anteil / sum(TL.distr$Anteil)]
  
  data <- timelineMultiplied[, count := count / 
                                               sum(timelineMultiplied$count)]
  
  joined <- merge(TL.distr, data, all.x=TRUE)
  joined$count[is.na(joined$count)] <- 0
  
  joined[, diff := abs(Anteil - count)]
  
  return(1 - sum(joined$diff))
}

#input is a datatable of at least dates and cluster ids

HB.diff.index <- function(data) {
  assertDataTable(data)
  assertSubset(c("cluster", "date"), names(data))
  
  #attach gwl, but remove U
  useDat <- attachGwl(data)[gwl != "U"]
  
  tabled <- copy(useDat)[, .N, by = .(gwl, cluster)]
  
  maxTab <- copy(tabled)[, .(num = max(N) / sum(N), size = sum(N)), by = .(gwl)]
  
  sum(maxTab$num) / nrow(maxTab)
}

# this is a function to compute the dissimilarity matrix for pam clustering and the silhouette which
# speciefies k (clusters)

# INPUT: - data: a data.table which is already scaled
#        - weights: the weights for the variables (only if the dt hasnt already been weighted)
#        - metric: either euclidean, manhatten or gower
#        - dist: logical, indicating whether just the dissimilarity matrix has to be computed

dissimilarityPAM <- function(data, weights = c(rep(c(1/3, 1/6, rep(1/3, 2), rep(1/6, 5)), 2),
                                               rep(1/6, 12), rep(1/18,18)), 
                             metric = "euclidean", dist = TRUE) {
  assertDataTable(data)
  assertNumeric(weights, null.ok = TRUE)
  assertSubset("date", colnames(data)[1])
  assertSubset(metric, choices = c("euclidean", "gower", "manhattan"))
  assertLogical(dist)
  dissimilarity <- daisy(data[, 2:ncol(data)], metric = metric, weights = weights)
  
  if (dist) {
    return(dissimilarity)
  }
  
  sil_width <- c(NA)
  for(i in 5:9){
    pam_fit <- pam(dissimilarity,
                   diss = TRUE,
                   k = i)
    
    sil_width[i-4] <- pam_fit$silinfo$avg.width
  }
  plot(5:9, sil_width,
       xlab = "Number of clusters",
       ylab = "Silhouette Width",)
  lines(5:9, sil_width)
  print(sil_width)
  return(dissimilarity)
}