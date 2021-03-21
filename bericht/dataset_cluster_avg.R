
datrrr <- cbind(copy(f_data), cluster = PAMres$clustering)[, c("cluster", 
                        "mean.mslp", "max.mslp", "min.mslp", "mean.geopot",
                           "max.geopot", "min.geopot")]

cols <- c("mean.mslp", "max.mslp", "min.mslp", "mean.geopot",
          "max.geopot", "min.geopot")

resMean <- copy(datrrr)[, (cols) := lapply(.SD, function(x) mean(x)), 
            by = cluster, .SDcols = cols][, .SD[1], by = cluster]

resSd <- copy(datrrr)[, (cols) := lapply(.SD, function(x) sd(x)), 
                        by = cluster, .SDcols = cols][, .SD[1], by = cluster]

res <- resMean[resSd, on = "cluster"]

res[, c(2:4,  8:10)] <- res[, c(2:4, 8:10)] / 100
res[, c(5:7,  11:13)] <- res[, c(5:7,  11:13)] / 9.80665

cols2 <- names(res)[-1]
res[,(cols2) := round(.SD,1), .SDcols=cols2]

setnames(res, names(res)[8:13], vapply(names(res)[2:7], function(x) paste0("sd_", x), ""))

saveRDS(res, "bericht/assets/dataset_cluster_avg.rds")

cols3 <- names(res)[2:7]
test <- copy(res)[, (cols) := lapply(.SD, function(x) paste(x, paste0("sd_", names(.SD)) )), .SDcols = cols3]


resSd2 <- copy(resSd)

resSd2[, c(2:4)] <- resSd2[, c(2:4)] / 100
resSd2[, c(5:7)] <- resSd2[, c(5:7)] / 9.80665

cols5 <- names(resSd)[-1]
resSd2[,(cols5) := lapply(.SD, function(x) sprintf("%.1f",x)), .SDcols=cols5]
resSd2[,(cols5) := lapply(.SD, function(x) paste("\u00B1", x)), .SDcols=cols5]


resMean2 <- copy(resMean)

resMean2[, c(2:4)] <- resMean2[, c(2:4)] / 100
resMean2[, c(5:7)] <- resMean2[, c(5:7)] / 9.80665

resMean2[,(cols5) := lapply(.SD, function(x) sprintf("%.1f",x)), .SDcols=cols5]

res2 <- rbind(resMean2, resSd2)
setorder(res2, cluster)
saveRDS(res2, "bericht/assets/dataset_cluster_avg_2.rds")
