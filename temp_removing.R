dat <- attachGwl(data.table(date = f_data$date, cluster = PAMres$clustering))

mosaic(dat, dat$cluster)

s <- rle(copy(dat[["cluster"]]))

lol <- c()
for (i in 1:2096) {
  lol <- c(lol, rep(s$lengths[i], s$lengths[i]))
}

datN <- cbind(dat, lol)


#remove rows where lol < 3
datNN <- datN[lol > 2]

mosaic(datNN, datNN$cluster)


##analysis remove gwl<4 before clustering

datto <- attachGwl(f_data)

dor <- rle(copy(datto[["gwl"]]))
loldo <- c()
for (i in 1:length(dor$lengths)) {
  loldo <- c(loldo, rep(dor$lengths[i], dor$lengths[i]))
}

datto <- cbind(datto, loldo)

datTODO_org <- datto[loldo > 3, ]
datTODO <- copy(datTODO_org)[, ":=" (date = NULL, gwl = NULL, loldo = NULL)]

library(parallelDist)

dist_datTODO <- parallelDist(as.matrix(datTODO), method = "manhattan",
                                     threads = detectCores() - 2)

bestClustNumber(dist_datTODO, metric = "manhattan", fname = "trial_gwl_4daysup", range = 5:9)


pam_lim <- pam(dist_datTODO, diss = TRUE, k = 5)

sil(pam_lim, pam_lim$clustering, dist_datTODO, "pam")
#sil = 0.2557
data_pam_lim <- data.table(date = datTODO_org$date, cluster = pam_lim$clustering)
tl <- Cl.timeline(data_pam_lim, multiplied = T, showOpt = T)
#TLS = -0.1083
mos <- mosaic(data_pam_lim, data_pam_lim$cluster)
#HBdiff = 0.3499
