pam.manhat.allpca <- readRDS("Data/pam.manhat.30.allpca.rds")
pam.manhat.allpca.weighted <- readRDS("Data/pam.manhat.30.allpca.weighted.rds")
pam.manhat <- readRDS("Data/pam.manhat.30.rds")
pam.manhat.preweighted.diff <- readRDS("Data/pam.manhat.preweighted.diff.rds")
pam.manhat.preweighted <- readRDS("Data/pam.manhat.preweighted.rds")

data.pca <- extrapolate(seq(1971, 2000), "all.pca")
data <- extrapolate(seq(1971, 2000), "all")

manova.fun(copy(data), pam.manhat$clustering)
manova.fun(copy(data.pca), pam.manhat.allpca$clustering)
manova.fun(copy(data), pam.manhat.preweighted$clustering)
manova.fun(copy(data), pam.manhat.preweighted.diff$clustering)
manova.fun(copy(data.pca), pam.manhat.allpca.weighted$clustering)


significance <- list(list("not.significant" = c("euclidean.mslp", "minGeopot.verID",
                                                "euclidean.minDiff", "meanMslp_1.2"),
                           "very.significant" = c("alle Verteilungsvariablen", "alle meanGeopot")),
                     list("not.significant" = c("euclidean.mslp", "minGeopot.verID",
                                               "euclidean.minDiff", "meanMslp_1.2"),
                          "very.significant" = c("alle Verteilungsvariablen", 
                                                 "alle meanGeopot", "PCA1")),
                     list("not.significant" = c("minGeopot.verID", "minGeopot.horID", "meanMslp_1.2"),
                          "very.significant" = c("alle Verteilungsvariablen", "alle meanGeopot")),
                     list("not.significant" = c("meanMslp_1.2", "meanMslp_2.3"),
                          "very.significant" = c("alle Verteilungsvariablen", "alle meanGeopot")),
                     list("not.significant" = c("maxGeopot.verID", "minGeopot.verID", "minGeopot.horID",
                                                "euclidean.minDiff", "meanMslp_1.2"),
                          "very.significant" = c("alle Verteilungsvariablen", "alle meanGeopot")
                     ))

names(significance) <- c("pam.manhat", "pam.manhat.pca", "pam.manhat.preweighted",
  "pam.manhat.preweighted.diff" ,"pam.manhat.allpca.weighted") 
 
significance 




gwl <- readRDS("Data/gwl.rds") 
gwl <- gwl[order(date)]
gwl
gwl30 <- subsetYears(copy(gwl), seq(1971, 2000))
any(is.na(gwl30$gwl))
Cl.timeline(copy(gwl30), cluster = "gwl")
Cl.timeline()

data.cluster <- copy(data)[, cluster := pam.manhat$clustering]
Cl.timeline(copy(data.cluster), seperated = TRUE)




