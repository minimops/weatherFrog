# testing daily filtering with kmeans

library(e1071)
library(data.table)
library(factoextra)

trialData <- readRDS("Data/cli_data_2k_avgDay.rds")
set.seed(124)
#get one Day
trialDay <- copy(trialData)[date == sample(unique(trialData$date), 1), ]
trialDay[, date := NULL]

#todo scale this first
trialDay_scale <- data.table(scale(trialDay))

maxGeo <- copy(trialDay_scale)[avg_geopot == max(avg_geopot), ]
minGeo <- copy(trialDay_scale)[avg_geopot == min(avg_geopot), ]
maxMslp <- copy(trialDay_scale)[avg_mslp == max(avg_mslp), ]
minMslp <- copy(trialDay_scale)[avg_mslp == min(avg_mslp), ]

clusts <- cmeans(trialDay_scale, 
                 centers = unique(rbind(maxGeo, minGeo, maxMslp, minMslp)))
#this form of weighting weights roiws and not columns i think, so this is wrong
clusts <- cmeans(trialDay_scale, rbind(maxGeo, minGeo, maxMslp, minMslp), 
                 weights = c(1.5, 1.5, 1, 1))
clusts <- cmeans(trialDay_scale, 5)

#plot cluster result
fviz_cluster(list(data = trialDay_scale, cluster=clusts$cluster), 
             ellipse.type = "norm",
             ellipse.level = 0.68,
             palette = "jco",
             ggtheme = theme_minimal())

# library(corrplot)
# corrplot(clusts$membership, is.corr = FALSE)

#plot on map
#attach cluster
toPlot <- data.frame(trialDay, cluster = as.factor(clusts$cluster))

drawDay(toPlot, "cluster", showGuide = FALSE)
drawDay(toPlot, "avg_mslp", showGuide = FALSE, discrete = FALSE)
drawDay(toPlot, "avg_geopot", showGuide = FALSE, discrete = FALSE)
