## trying out PCA

library(ggfortify)
library(cluster)
library(data.table)

#import Datasets
wide_05 <- readRDS("Data/cli_data_05_avgDay_wide.rds") 
gwl <- readRDS("Data/gwl.rds")

#remove geopot for now
drop.cols <- grep("avg_geopot.*", colnames(wide_05))
wide_05 <- wide_05[, (drop.cols) := NULL]

#attach gwl per Day and transform to df
wide_05 <- as.data.frame(gwl[wide_05, on = .(date)])

#pca
pca_05 <- prcomp(wide_05[, 3:162])
summary(pca_05)

screeplot(pca_05, npcs = 10, type = "lines")
cumvar <- cumsum(pca_05$sdev^2 / sum(pca_05$sdev^2))
lines(cumvar[1:10])
(pc.index<-min(which(cumvar>0.85)))

#plot
pca.plot <- autoplot(pca_05, data = wide_05, colour = "gwl")
pca.plot


autoplot(clara(wide_05[3:162], 4), 
         frame = TRUE, frame.type = "norm")



# First for principal components
comp <- data.frame(pca_05$x[,1:9])

# Determine number of clusters
wss <- (nrow(comp)-1)*sum(apply(comp,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(comp,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")


fit <- kmeans(comp, 8, nstart=25, iter.max=1000)
aggregate(comp,by=list(fit$cluster),FUN=mean)
# append cluster assignment
mydata <- data.frame(comp, fit$cluster)
mydata <- data.frame(mydata, wide_05$gwl)


#which proportion of GWL x is in which cluster
(clust_table_1 <- round(
  prop.table(table(mydata$wide_05.gwl, mydata$fit.cluster), margin = 1), 2))

(clust_table_2 <- round(
  prop.table(table(mydata$fit.cluster, mydata$wide_05.gwl), margin = 1), 2))


plot(clust_table_1)



library(tsne)
tsne(wide_05)

colors = rainbow(length(unique(wide_05$gwl)))
names(colors) = unique(wide_05$gwl)
ecb = function(x,y){ plot(x,t='n'); text(x,labels=wide_05$gwl, col=colors[wide_05$gwl]) }
tsne_iris = tsne(wide_05[,-c(1,2)], epoch_callback = ecb, perplexity=50)




##Attempt #2
#at PCA

library(ggplot2)

#load appropriate dataset
#use long format to normalize values first
cli_data_pca <- readRDS("Data/cli_data_05_avgDay_index.rds")
gwl <- readRDS("Data/gwl.rds")
#TODO wrong scaling?
cli_data_pca[, ":=" (avg_mslp = scale(avg_mslp), avg_geopot = scale(avg_geopot))]
#to wide
cli_data_pca <- dcast(cli_data_pca,
                date ~ geoIndex,
                value.var = c("avg_mslp", "avg_geopot"))

#attach gwl per Day and transform to df
cli_data_pca <- as.data.frame(gwl[cli_data_pca, on = .(date)])

#pca
cli_pca_2 <- prcomp(as.data.frame(cli_data_pca)[3:322])

#plotting first two pca
cli_pca_2Scores <- data.frame(cli_pca_2$x[, 1:2])
ggplot(cli_pca_2Scores, aes(y = PC1, x = PC2)) + 
  geom_point(alpha = 0.3)

#clustering
plot(cli_pca_2) #elbow point at 4

cumvar <- cumsum(cli_pca_2$sdev^2 / sum(cli_pca_2$sdev^2))
(pc.index<-min(which(cumvar>0.85)))
#but 85percent of variance with 10 components

#continuing with 4 for now
pca2_cluster <- cli_pca_2$x[, 1:4]

library(factoextra)
#optimal number of clusters
fviz_nbclust(pca2_cluster, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)
#clustering
k2 <- kmeans(pca2_cluster, centers = 4, nstart = 25)


fviz_cluster(k2, data = cli_data_pca[, - c(1, 2)])

k2_clustered <- data.frame(cli_data_pca, k2$cluster)

(clust_table_pca <- round(
  prop.table(table(k2_clustered$gwl, k2_clustered$k2.cluster), margin = 1), 2))
plot(clust_table_pca)

