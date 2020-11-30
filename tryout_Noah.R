library(ggplot2)


ggplot(data = copy(cli_data_2k_avgDay)[format(date, "%Y-%m-%d") %in% c("2000-01-01")], 
       aes(x = longitude, y = latitude, color = avg_geopot)) +
  geom_point(size = 2)


one_day <- copy(cli_data_2k_avgDay)[format(date, "%Y-%m-%d")
                                    %in% c("2000-01-01"), ][, 
                                    avg_geopot := NULL]

cli_data_2k_avgDay_mslp <- dcast(copy(one_day),
                                 latitude ~ longitude,
                                 value.var = c("avg_mslp")
)[, latitude := NULL]

names(cli_data_2k_avgDay_mslp) <- as.character(seq(1, 20))

# Ward Hierarchical Clustering
d <- dist(mydata, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward")
plot(fit) # display dendogram


