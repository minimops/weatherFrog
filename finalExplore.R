
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
          dist = FALSE,
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
          fname = "scaledNweighted.noRange"
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
