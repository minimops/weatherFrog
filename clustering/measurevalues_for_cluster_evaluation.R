
#### Trying measure values for the evaluation for clustersolutions


############
######1. Gamma Koeffizient / Kophenetischer Korrelationskoeffizient

######### ACHTUNG: Nur geeignet für hierarchisches Clustering, da in 
#die Berechnung des Gamma Koeffizienten die Dendrigramm-Matrix eingeht ( die es nur bei
#hierarchisches Clustering gibt?)
 
# Bestimmt Korrelation zwischen Distanzen der Distanzmatrix ( hier Mahalanobis Matrix)
# und kophenetischer Matrix = Distanzmatrix, die sich aus dem Dendrogramm ergibt

# Idee: Bestimme konkordanten und diskordanten Paare der Distanzen zwischen 
# Distanzatrix und kophenetischer Matrix: gamma = (C - D) / (c + D)

#Entscheiung für das Verfahren mit dem groeßten Korrelationskoeffizienten


######## Anwendung
## Bsp "erster Clusterversuch" aus clustermahalanobis.R: cpmplete_linkage mit mahalanobis
# Berechnung der kophenet. Matrix = Matrix aus dem Dendrogramm

###Fragen:
#Wie exrahiere ich Matrix des Dendrogramms?
# Wie bestimme ich die Distanzen zwischen den beiden Matrizen?
# Im Internet gibt es dazu nichts, nur allgemein zu Goodmann und Kruskals Gamma 



###########################################
#2. MANOVA

# Multivariate Varianzanalyse
# Gegeben: Grundgesamtheit, die in g Gruppen partioniert ist
#Untersuchr Varianz innerhalb einer Gruppe (W) und Varianzen zwischen den Gruppen (B)
# und setzt beide in Beziehung
#Testgroeße: Wilks Lambda: |W| / |W + B|
#Variabilität zwischen Gruppen sehr stark: Nenner > Zaehler
# p- Wer > alpha: Variation in Gruppen > Variation zwischen Gruppen

#### Durchfuehrung


## Bsp 1 aus Internet:
library(rattle.data)
data(wine, package="rattle.data")
wine_subset <- scale(wine[ , c(2:4)])
wine_cluster <- kmeans(wine_subset, centers = 3,
                       iter.max = 10,
                       nstart = 25)
cluster <- wine_cluster$cluster
wine1 <- cbind(cluster,wine_subset)
wine1 <- as.data.frame(wine1)

# MANOVA
#manova(as.matrix(dat[,c("read","math")]) ~ dat$income_cut)
model <-  manova(as.matrix(wine1[,2:4]) ~ wine1$cluster)
 summary(model, test = "Wilks")

##Bsp 2 aus R file clustermahalanobis K- means

# benoetigt ( attributes of kmeans output)
  #withness = sum of squares of within clusters
  #betweeness   = sum of squares of between clusters

labels(clusterkmeans)
clusterkmeans$withinss
clusterkmeans$tot.withinss
clusterkmeans$betweenss
clusterkmeans$size
clusterzuordnung <- clusterkmeans$cluster
 a <- clusterkmeans$centers
 
 clusterframe <- cbind(clusterzuordnung,dist_mahal_scaled)
 

# Syntax von MANOVA
# manova(as.matrix(dat[,c("read","math")]) ~ dat$income_cut)
# Variablen innerhalb eines Clusters: hier: geopot und mslp, und Einteilung der Cluster

# Benötigt: dataframe, der angibt, welche Werte in welchem Cluster sind, um eine ANOVA/MANOVA
 #durchzuführen
 
# Problem: k means wurde anstatt mti den "Originaldaten" mit der Distanzmatrix durchgeführt
 
 
 

 
 ########
 #3. Silouhettenkoeffizient
 
 # gibr fuer eine Beobachtung an, wie gut die Zuordnung zu den beiden
 #nächstgelegenen Clustern ist
 
 #Silhouttenkoeffi gibt eine von der Clusteranzahl unabhaengige Maßzahl für Qualitaet
 #eines Clusters an
 
#####Durchfuehrung mit Weinebsp. von oben
 
 ### Silhouette-Plot
 # Silhouette: Pro Punkt i wird ein Balken gezeichnet:
 # s(i) = (b - a) / max(b, a)
 # mit b der Wahrscheinlichkeit, dass Punkt i dem eigenen Cluster zugeordnet wird, und
 # mit a der Wahrscheinlichkeit, dass Punkt i dem am naechsten liegenden Cluster zugeordnet wird
 # -> Es sollten alle Werte positiv und moeglichst nahe 1 sein.
 #    Werte < 0 weisen auf Probleme in der Clusterung hin.
 library(factoextra)
 sil <- cluster::silhouette(wine_cluster$cluster, dist(wine))
 fviz_silhouette(sil)

 
 #4. Vergleich von Clustern mit Rand Index
 
 # vergleicht die Aehnlichkeit zwischen Cluster
 
 # 1. Clusterversuch: kmeans: wine_cluster
 #2.Clusterversuch: complete linkage
 
 complete_linkage <- hcut(dist(wine), k = 3, hc_func = "hclust", hc_method = "complete")

 c1 <- wine_cluster$cluster
 c2 <- complete_linkage$cluster
 library(mclust)
adjustedRandIndex(c1,c2) 
table(c1,c2)

