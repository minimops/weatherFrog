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
