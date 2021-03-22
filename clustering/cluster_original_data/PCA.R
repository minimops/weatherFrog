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



## Measurement 
# 1.
sil(fit, fit$cluster, dist(comp), "kmeans")
?manova
dat.pca <- copy(as.data.table(wide_05))[, cluster := fit$cluster]
# 2.
Cl.timeline(copy(dat.pca))
# 3.
model.kmeans.euc <- manova(as.matrix(dat.kmeans[, 2:49]) ~ dat.kmeans$cluster)
summary(as.matrix(dat.kmeans[, 2:49]) ~ dat.kmeans$cluster, test = "Wilks")
summary.aov(model.kmeans.euc)
# 4.
mosaic(copy(as.data.table(wide_05)), fit$cluster, title = "PAM WITH MANHAT")


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
library(data.table)


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
autoplot(cli_pca_2, alpha = 0.3)
#plotting first two pca
cli_pca_2Scores <- data.frame(cli_pca_2$x[, 1:2])
ggplot(cli_pca_2Scores, aes(y = PC1, x = PC2)) + 
  geom_point(alpha = 0.3) +
  labs(x = "PC1 (28.31%)", y = "PC2 (16.05%)") +
  ggtitle("Ersten zwei PC (skaliert)")

library(cluster)
library(ggfortify)
autoplot(clara(as.data.frame(cli_data_pca)[3:322], 4), 
         frame = TRUE, frame.type = "norm", 
         title = "clara with 4 clusters")


#clustering
plot(cli_pca_2, main = "Screeplot PCA") #elbow point at 4

cumvar <- cumsum(cli_pca_2$sdev^2 / sum(cli_pca_2$sdev^2))
(pc.index<-min(which(cumvar>0.85)))
#but 85percent of variance with 10 components

#continuing with 10 for now
pca2_cluster <- cli_pca_2$x[, 1:10]

library(factoextra)
#optimal number of clusters
fviz_nbclust(pca2_cluster, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)
#clustering
k2 <- kmeans(pca2_cluster, centers = 4, nstart = 25)

## Measurement 
# 1.
sil(k2, k2$cluster, dist(pca2_cluster), "kmeans")
?manova
dat.pca2 <- copy(as.data.table(wide_05))[, cluster := k2$cluster]
# 2.
Cl.timeline(copy(dat.pca2))
# 3.
model.kmeans.euc <- manova(as.matrix(dat.kmeans[, 2:49]) ~ dat.kmeans$cluster)
summary(as.matrix(dat.kmeans[, 2:49]) ~ dat.kmeans$cluster, test = "Wilks")
summary.aov(model.kmeans.euc)
# 4.
mosaic(copy(as.data.table(wide_05)), k2$cluster, title = "PAM WITH MANHAT")


#cluster plot
fviz_cluster(k2, data = cli_data_pca[, - c(1, 2)], labelsize = 0)
#cant add labels and this plot is a mess so:
library(ggalt)
clustervis <- data.frame(gwl = cli_data_pca$gwl, cluster = factor(k2$cluster), cli_pca_2$x)

ggplot(data = clustervis, aes(x = PC1, y = PC2)) +
  geom_point(aes(color = gwl)) +
  geom_encircle(aes(group = cluster))
#still a bit too messy, so lets splitt gwls into two
cl_vis <- as.data.table(clustervis)
#subsetting most often occuring
cl_vis_mp <- cl_vis[gwl %in%
                      names(which(table(cl_vis$gwl) > median(table(cl_vis$gwl)))), ]
#and least often
cl_vis_lp <- cl_vis[gwl %in% names(which(table(cl_vis$gwl) <= median(table(cl_vis$gwl)))), ]


#plot lp
#color as cluster
ggplot(data = data.frame(cl_vis_lp), aes(x = PC1, y = PC2)) +
  geom_point(aes(color = cluster)) +
  geom_text(aes(label = gwl), size = 3, hjust = 0, vjust = 0)
#color as gwl
ggplot(data = data.frame(cl_vis_lp), aes(x = PC1, y = PC2)) +
  geom_point(aes(color = gwl)) +
  geom_text(aes(label = cluster), size = 3, hjust = 0, vjust = 0) +
  geom_encircle(aes(group = cluster))


#only labeling one gwl without subsetting
ggplot(data = data.frame(cl_vis), aes(x = PC1, y = PC2)) +
  geom_point(aes(color = cluster)) +
  geom_text(data = cl_vis[gwl == unique(cl_vis$gwl)[[4]]], 
            aes(label = gwl), size = 3, hjust = 0, vjust = 0) +
  ggtitle("kmeans 10pc")




k2_clustered <- data.frame(cli_data_pca, k2$cluster)

#table
(clust_table_pca <- 
  table(k2_clustered$gwl, k2_clustered$k2.cluster))
#prop table
(clust_proptable_pca <- round(
  prop.table(table(k2_clustered$gwl, k2_clustered$k2.cluster), margin = 1), 2))
#Moaik of table
plot(clust_table_pca, main = "Mosaikplot GWL zu Cluster durch PCA&kmeans")
#mosaik of prop table
plot(clust_proptable_pca)


#with pam
k3 <- pam(pca2_cluster, 4)
clustervis2 <- data.frame(gwl = cli_data_pca$gwl, cluster = factor(k3$cluster), cli_pca_2$x)
(clust_table_pca_pam <- 
    table(clustervis2$gwl, clustervis2$cluster))
plot(clust_table_pca_pam)
#doesnt look better...




#kurz westlich von spanien abschneiden
pca_noWest <- readRDS("Data/cli_data_05_avgDay.rds")
gwl <- readRDS("Data/gwl.rds")
#eliminate longitude smaller than -30, latitude greater than 70
pca_noWest <- pca_noWest[longitude >= -30, ][latitude <= 70]
#to wide
pca_noWest <- dcast(pca_noWest,
                      date ~ longitude + latitude,
                      value.var = c("avg_mslp", "avg_geopot"))
#so we are down to 196 dimensions

#attach gwl per Day and transform to df
pca_noWest <- as.data.frame(gwl[pca_noWest, on = .(date)])
names(pca_noWest) <- c("date", "gwl", seq(1:196))
#pca
cli_pca_noWest <- prcomp(as.data.frame(pca_noWest)[, -c(1, 2)], scale. = TRUE)
plot(cli_pca_noWest)
#screeplot looks a lot better
#plotting first two pca
cli_pca_noWest_Scores <- data.frame(cli_pca_noWest$x[, 1:2])
ggplot(cli_pca_2Scores, aes(y = PC1, x = PC2)) + 
  geom_point(alpha = 0.3) +
  ggtitle("first two PCA with scaled parameters")
#nuber of clust
fviz_nbclust(cli_pca_noWest$x, kmeans, method = "wss") +
  geom_vline(xintercept = 8, linetype = 2)
#clustering
k3 <- kmeans(cli_pca_noWest$x, centers = 8, nstart = 25)

clusterdata_noWest <- data.frame(cluster = k3$cluster, gwl = pca_noWest$gwl)

#table
(clust_table_pca_noWest <- 
    table(clusterdata_noWest$gwl, clusterdata_noWest$cluster))
plot(clust_table_pca_noWest)


autoplot(clara(as.data.frame(pca_noWest)[, -c(1, 2)], 8), 
         frame = TRUE, frame.type = "norm")
