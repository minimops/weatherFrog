
#### Trying measure values for the evaluation for clustersolutions
library(rattle.data)
data(wine, package="rattle.data")
wine_subset <- scale(wine[ , c(2:4)])
wine_cluster <- kmeans(wine_subset, centers = 3,
                       iter.max = 10,
                       nstart = 25)
cluster <- wine_cluster$cluster
wine1 <- cbind(cluster,wine_subset)
wine1 <- as.data.frame(wine1)

############
######1. Gamma Koeffizient / Kophenetischer Korrelationskoeffizient
# ist eine Anpassung von GoodmannKruskalGamma an Clusteranalyse

######### ACHTUNG: Nur geeignet für hierarchisches Clustering, da in 
#die Berechnung des Gamma Koeffizienten die Dendrigramm-Matrix eingeht ( die es nur bei
#hierarchisches Clustering gibt?)
 
# Bestimmt Korrelation zwischen Distanzen der Distanzmatrix ( hier Mahalanobis Matrix)
# und kophenetischer Matrix = Distanzmatrix, die sich aus dem Dendrogramm ergibt

# Idee: Bestimme konkordanten und diskordanten Paare der Distanzen zwischen 
# Distanzatrix und kophenetischer Matrix: gamma = (C - D) / (c + D)

#Entscheiung für das Verfahren mit dem groeßten Korrelationskoeffizienten


######## Anwendung

# cophenetic(x) : erzeugen der kophenetischen Matrix 
# cor_cophenetic(x1 = distance_matric, x2 = cophenetische_Matrix)


# Frage: goodman/Kruskals Gamma eventuell auch bei nicht hierarchischen Cluster anwendbar?:
# Antwort: scheint zu gehen laut dem Buch Clusteranalyse von Bacher, leider steht nicht drin, 
# wie man es berechnet bzw. in R implementiert
# gamma gibt an, in welchem Ausmaß die Distanzen (Unähnlichkeiten) innerhalb des 
#Clusters kleiner sind als zwischen den Clustern (hört sich irgendwie nach MANOVA an) ( Buch S. 248)

###########################################
#2. MANOVA

# MANOVA: mehrere Zielvariablen
#ANOVA: eine Zielvariable
# unabhängige Variable = Einflussvariable ,ist immer factor, ist die Gruppierungsvariable
# abhängige Variable = Zielvariable, enthält die Messwerte, ist metrisch
  
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
#cluster <- wine_cluster$cluster
wine1 <- cbind(wine_cluster$cluster,wine_subset)
wine1 <- as.data.frame(wine1)
colnames(wine1)[colnames(wine1) == "V1"] <- "cluster_group"

# MANOVA
#manova(as.matrix(dat[,c("read","math")]) ~ dat$income_cut)
(model <- manova(as.matrix(wine1[,2:4]) ~ wine1$cluster))
 summary(model, test = "Wilks")
 summary.aov(model)
 

 boxplot(wine1$Alcohol ~ wine1$cluster_group, ylab = colnames(wine1[,2]))
 colnames(wine1)[1]
 boxplot(wine1$Malic ~ wine1$cluster_group, ylab = colnames(wine1[1]))
 boxplot(wine1$Ash ~ wine1$cluster_group)
 
for ( i in seq_len(ncol(wine1)) - 1){
  boxplot(wine1[, i + 1] ~ wine1$cluster_group, ylab = colnames(wine1[i + 1]))
  
}
 
 
#Bsp 2 aus R file clustermahalanobis K- means

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
 

 ####mit checkManova
 #scheint nur mit clusterobjekt von fuzzy zu funktionieren, kmeans hat andere
 #Struktur im cluster output
 
 library(RcmdrPlugin.FuzzyClust)
 fuzzy.CM(X=wine[,2:5],K = 3,m = 2,RandomNumber = 1234)->cl
 checkManova(cl)


 

 
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


# Weitere Möglichkeiten, ein Cluster zu bewerten:

#Diskriminanzanalyse
# allgemeines, lineares Modell / multiples, lineares Regressionsmodell

#


# Anmerkung: einen blick auf fuzzy means werfen?