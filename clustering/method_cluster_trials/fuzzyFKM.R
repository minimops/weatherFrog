# Fuzzy c means clustering with FKM


##number of clusters s no longer required as an input argument

#optional number of clusters is automatically chosen by optimizing one of 
#the available fuzzy clusteri validy indices
# has the option scale

# only question: is it possible to implement this fuction with mahalanobis? 
# with distance do this function use? did not found any information about that


library(ppclust)
library(factoextra)
library(cluster)
library(fclust)
library(e1071) #for cmeans


# generate extracted dataset
source("clustering/Var_Extraction_Method/f_extr_funs.R")

# choose 30 years
extract_data_30 <- extrapolate(seq(1971, 2010))



# fuzzy c-means clustering 
x <- as.data.frame(extract_data_30)

# with standardisation, no weights
res.fkm <- FKM(x[,-1],stand = 1, RS = 10, seed = 123,index = "SIL.F", k = 7:15)
res.fkm$k
# rechenleistung reicht nicht aus

