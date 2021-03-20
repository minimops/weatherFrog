
library(cluster)
library(parallelDist)
library(parallel)
library(factoextra)
library(data.table)
source("clustering/Var_Extraction_Method/f_extr_funs.R")

#check if diff overall weights equal diff results
dat1 <- extrapolate(seq(1971, 1981))
dat1s <- scale(copy(dat1)[, date := NULL])
dist_dat1s <- daisy(dat1s, metric = "manhattan")
res_dat1s <- pam(dist_dat1s, 5, diss = TRUE)
mean(silhouette(x = res_dat1s$clustering, dist = dist_dat1s)[, 3])

dat1sw <- mapply("*", as.data.frame(dat1s), rep(0.5, ncol(dat1s)))
dist_dat1sw <- daisy(dat1sw, metric = "manhattan")
res_dat1sw <- pam(dist_dat1sw, 5, diss = TRUE)
mean(silhouette(x = res_dat1sw$clustering, dist = dist_dat1sw)[, 3])

#well acc seems to be the same
#so we dont have to regularize any dist matricies

#baseline
datextr <- extrapolate(seq(1971, 2000))

dist0 <- parallelDist(scale(copy(datextr)[, date := NULL]), method = "manhattan",
                      threads = detectCores() - 2)

clust0 <- pam(dist0, 5, diss = TRUE)

fviz_silhouette(silhouette(clust0$clustering, dist0))


#new finer id values better?

#split up diff over day values
#before split
diffDay_old <- readRDS("Data/change_day.rds")[format(date, "%Y") %in% seq(1971, 2000)]

datcompl2 <- datextr[diffDay_old, ]

dist2 <- parallelDist(scale(copy(datcompl2)[, date := NULL]), method = "manhattan",
                      threads = detectCores() - 2)

clust2 <- pam(dist2, 5, diss = TRUE)

fviz_silhouette(silhouette(clust2$clustering, dist2))



#after split
diffDay <- readRDS("Data/change_day_mslp.rds")[readRDS("Data/change_day_geopot.rds"), ]


datcompl <- datextr[diffDay, ]

dist1 <- parallelDist(scale(copy(datcompl)[, date := NULL]), method = "manhattan",
             threads = detectCores() - 2)

clust1 <- pam(dist1, 5, diss = TRUE)

fviz_silhouette(silhouette(clust1$clustering, dist1))

#which diff over day calc is better?

#try different weights

#ungewichtet
#annes gewichte
#trial

#run final version
## add diff over day to datextr
d <- copy(datextr)[readRDS("Data/change_day_mslp.rds"), on = "date"]
datadiff <- copy(d)[readRDS("Data/change_day_geopot.rds"), on = "date"]
## 1. scaled but unweighted
PAMhelper(scaleNweight(copy(datadiff)), 
          weights = rep(1, 50), 
          metric = "manhattan", 
          fname = "scaled"
          )

#### skalierst du da??

## 2.scaled and weighted
PAMhelper(scaleNweight(copy(datadiff)),
          weights = c(rep(c(1/4, 1/6, rep(1/4, 2), rep(1/6, 5)), 2),
                      rep(1/6, 12), rep(1/9, 18), rep(1/4, 2)),
          metric = "manhattan",
          dist = FALSE,
          fname = "scaledNweighted"
          )

## 3. without range
dataNoRange <- copy(datadiff)[, ":=" (range.mslp = NULL, range.geopot = NULL)]

PAMhelper(scaleNweight(copy(dataNoRange)),
          weights = c(rep(c(1/3, 1/6, rep(1/3, 2), rep(1/6, 4)), 2),
                      rep(1/6, 12), rep(1/9, 18), rep(1/6, 2)),
          metric = "manhattan",
          dist = FALSE,
          fname = "scaledNweighted.noRange.final"
)
#create final plots


##without mindiff/maxdiff
dataNoRnD <- copy(dataNoRange)[, ":=" (euclidean.minDiff = NULL, euclidean.maxDiff = NULL)]

PAMhelper(scaleNweight(copy(dataNoRnD)),
          weights = c(rep(c(1/3, 1/6, rep(1/3, 2), rep(1/6, 4)), 2),
                      rep(1/5, 10), rep(1/9, 18), rep(1/6, 2)),
          metric = "manhattan",
          dist = FALSE,
          fname = "scaledNweighted.noRange.noDiff"
)

PAMhelper(scaleNweight(copy(dataNoRnD)),
          weights = NULL,
          metric = "manhattan",
          dist = FALSE,
          fname = "scaled.noRange.noDiff"
)



## scaled and weighted different diff calc
diff2 <- readRDS("Data/change_day_mslp_var2.rds")[readRDS("Data/change_day_geopot_var2.rds"), ]
datfull <- datextr[diff2, ]


PAMhelper(scaleNweight(copy(datfull)),
          weights = c(rep(c(1/4, 1/6, rep(1/4, 2), rep(1/6, 5)), 2),
                      rep(1/6, 12), rep(1/9, 18), rep(1/4, 2)),
          metric = "manhattan",
          dist = FALSE,
          fname = "scaledNweighted_diff2"
)


## different id vals scaled and weighted with first diff
extr_diffID <- extrapolate(seq(1971, 2000), "all.fullID")
temp2 <- copy(extr_diffID)[readRDS("Data/change_day_mslp.rds"), on = "date"]
datadiff_newID <- temp2[readRDS("Data/change_day_geopot.rds"), on = "date"]


PAMhelper(scaleNweight(copy(datadiff_newID)),
          weights = c(rep(c(1/4, 1/6, rep(1/4, 2), rep(1/6, 5)), 2),
                      rep(1/6, 12), rep(1/9, 18), rep(1/4, 2)),
          metric = "manhattan",
          dist = FALSE,
          fname = "scaledNweighted_newIDs"
)


## scaled and weighted no range no diff new diff calc, new ids
extr_diffID <- extrapolate(seq(1971, 2000), "all.fullID")
temp3 <- copy(extr_diffID)[readRDS("Data/change_day_mslp_var2.rds"), on = "date"]
datadiff_newID_var2 <- temp3[readRDS("Data/change_day_geopot_var2.rds"), on = "date"]

datadiff_newID_var2[, ":=" (euclidean.minDiff = NULL, euclidean.maxDiff = NULL,
                       range.mslp = NULL, range.geopot = NULL)]

PAMhelper(scaleNweight(copy(datadiff_newID_var2)),
          weights = c(rep(c(1/3, 1/6, rep(1/3, 2), rep(1/6, 4)), 2),
                      rep(1/5, 10), rep(1/9, 18), rep(1/6, 2)),
          metric = "manhattan",
          dist = FALSE,
          fname = "scaledNweighted.noRange.noDiff.newID.newDiff"
)


# final decision
diff2 <- readRDS("Data/change_day_mslp_var2.rds")[readRDS("Data/change_day_geopot_var2.rds"), ]
datfull_f <- extrapolate(seq(1971, 2000))[diff2, ]
dataNoRange_f <- datfull_f[, ":=" (range.mslp = NULL, range.geopot = NULL)]


PAMhelper(scaleNweight(copy(dataNoRange_f)),
          weights = c(rep(c(1/3, 1/6, rep(1/3, 2), rep(1/6, 4)), 2),
                      rep(1/6, 12), rep(1/9, 18), rep(1/6, 2)),
          metric = "manhattan",
          dist = FALSE,
          fname = "scaledNweighted.noRange.newDiff"
)



####final clust

weights <- c(rep(c(1/3, 1/6, rep(1/3, 2), rep(1/6, 4)), 2),
             rep(1/6, 12), rep(1/9, 18), rep(1/6, 2))
d <- copy(datextr)[readRDS("Data/change_day_mslp.rds"), on = "date"]
datadiff <- copy(d)[readRDS("Data/change_day_geopot.rds"), on = "date"]
dataNoRange <- copy(datadiff)[, ":=" (range.mslp = NULL, range.geopot = NULL)]

useDat <- as.data.table(scale(copy(dataNoRange)[, date := NULL]))[, Map("*", .SD, weights)]
dissimilarity <- parallelDist(as.matrix(useDat), method = "manhattan",
                              threads = detectCores() - 2)

finalClust <- pam(dissimilarity, 6, diss = TRUE)

clusterAssesment(dataNoRange, clusterRes = finalClust, metric = "manhattan", 
                 distance = dissimilarity, fname = "scaleNweight.noRange.diff1.6clust")

saveRDS(finalClust, "finalDATA/PAMres.rds")
saveRDS(dataNoRange, "finalDATA/f_data.rds")
saveRDS(dissimilarity, "finalDATA/f_dist.rds")


#full set split up into seasons:
library(parallelDist)
library(parallel)

source("clustering/ClusterAssesmentHelper.R")
source("clustering/PAM_NumCL_finder.R")
#final data, equals "dataNoRange" from above
f_data <- readRDS("finalDATA/f_data.rds")

weights <- c(rep(c(1/3, 1/6, rep(1/3, 2), rep(1/6, 4)), 2),
             rep(1/6, 12), rep(1/9, 18), rep(1/6, 2))

#summer
dat_Summer <- separateBySeason(f_data)

useDat_summer <- as.data.table(scale(copy(dat_Summer)[, date := NULL]))[, Map("*", .SD, weights)]
dissimilarity_summer <- parallelDist(as.matrix(useDat_summer), method = "manhattan",
                              threads = detectCores() - 2)

bestClustNumber(dissimilarity_summer, "manhattan", "justSummer", range = 5:9)

pam_summer <- pam(dissimilarity_summer, diss = TRUE, k = 5)

sil(pam_summer, pam_summer$clustering, dissimilarity_summer, "pam")
#sil = 0.1126
data_pam_summer <- data.table(date = dat_Summer$date, cluster = pam_summer$clustering)
tl <- Cl.timeline(data_pam_summer, multiplied = T, showOpt = T)
#TLS = 0.4767
mos <- mosaic(data_pam_summer, data_pam_summer$cluster)
#HBdiff = 0.3448

#winter
dat_Winter <- separateBySeason(f_data, Season = "Winter")

useDat_winter <- as.data.table(scale(copy(dat_Winter)[, date := NULL]))[, Map("*", .SD, weights)]
dissimilarity_winter <- parallelDist(as.matrix(useDat_winter), method = "manhattan",
                                     threads = detectCores() - 2)

bestClustNumber(dissimilarity_winter, "manhattan", "justWinter", range = 5:9)

pam_winter <- pam(dissimilarity_winter, diss = TRUE, k = 5)

sil(pam_winter, pam_winter$clustering, dissimilarity_winter, "pam")
#sil = 0.0909
data_pam_winter <- data.table(date = dat_Summer$date, cluster = pam_winter$clustering)
tl <- Cl.timeline(data_pam_winter, multiplied = T, showOpt = T)
#TLS = 0.2303
mos <- mosaic(data_pam_winter, data_pam_winter$cluster)
#HBdiff = 0.2970

