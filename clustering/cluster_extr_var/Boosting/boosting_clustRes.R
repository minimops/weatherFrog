#boosting results

##boosting results:
library(parallelDist)
library(parallel)
library(cluster)

source("clustering/ClusterAssesmentHelper.R")


#run2
run2 <- readRDS("Data/boosting/run2.rds")
weights <- unlist(run2$weights[101])

useDat_run2 <- as.data.table(scale(copy(f_data)[, date := NULL]))[, Map("*", .SD, weights)]
dissimilarity_run2 <- parallelDist(as.matrix(useDat_run2), method = "manhattan",
                                   threads = detectCores() - 2)

pam_run2 <- pam(dissimilarity_run2, diss = TRUE, k = 6)

sil(pam_run2, pam_run2$clustering, dissimilarity_run2, "pam")
#sil = 0.1257
data_pam_run2 <- data.table(date = f_data$date, cluster = pam_run2$clustering)
tl <- Cl.timeline(data_pam_run2, multiplied = T, showOpt = T)
#TLS = 0.3390
mos <- mosaic(data_pam_run2, data_pam_run2$cluster)
#HBdiff = 0.3702


#run4
run4 <- readRDS("Data/boosting/run4.rds")
weights <- unlist(run4$weights[101])

useDat_run4 <- as.data.table(scale(copy(f_data)[, date := NULL]))[, Map("*", .SD, weights)]
dissimilarity_run4 <- parallelDist(as.matrix(useDat_run4), method = "manhattan",
                                   threads = detectCores() - 2)

pam_run4 <- pam(dissimilarity_run4, diss = TRUE, k = 6)

sil(pam_run4, pam_run4$clustering, dissimilarity_run4, "pam")
#sil = 0.1148
data_pam_run4 <- data.table(date = f_data$date, cluster = pam_run4$clustering)
tl <- Cl.timeline(data_pam_run4, multiplied = T, showOpt = T)
#TLS = 0.4777
mos <- mosaic(data_pam_run4, data_pam_run4$cluster)
#HBdiff = 0.3640


#run1
run1 <- readRDS("Data/boosting/run1.rds")
weights <- unlist(run1$weights[101])

useDat_run1 <- as.data.table(scale(copy(f_data)[, date := NULL]))[, Map("*", .SD, weights)]
dissimilarity_run1 <- parallelDist(as.matrix(useDat_run1), method = "manhattan",
                                   threads = detectCores() - 2)

pam_run1 <- pam(dissimilarity_run1, diss = TRUE, k = 6)

sil(pam_run1, pam_run1$clustering, dissimilarity_run1, "pam")
#sil = 0.1116
data_pam_run1 <- data.table(date = f_data$date, cluster = pam_run1$clustering)
tl <- Cl.timeline(data_pam_run1, multiplied = T, showOpt = T)
#TLS = 0.3875
mos <- mosaic(data_pam_run1, data_pam_run1$cluster)
#HBdiff = 0.3680


#run5
run5 <- readRDS("Data/boosting/run5.rds")
weights <- unlist(run5$weights[101])

useDat_run5 <- as.data.table(scale(copy(f_data)[, date := NULL]))[, Map("*", .SD, weights)]
dissimilarity_run5 <- parallelDist(as.matrix(useDat_run5), method = "manhattan",
                                   threads = detectCores() - 2)

pam_run5 <- pam(dissimilarity_run5, diss = TRUE, k = 6)

sil(pam_run5, pam_run5$clustering, dissimilarity_run5, "pam")
#sil = 0.1052
data_pam_run5 <- data.table(date = f_data$date, cluster = pam_run5$clustering)
tl <- Cl.timeline(data_pam_run5, multiplied = T, showOpt = T)
#TLS = 0.3840
mos <- mosaic(data_pam_run5, data_pam_run5$cluster)
#HBdiff = 0.3624
