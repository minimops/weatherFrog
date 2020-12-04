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


