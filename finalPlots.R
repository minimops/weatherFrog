
pamfinal <- readRDS("wetransfer-ed1938/PAMres.rds")
distfinal <- readRDS("wetransfer-ed1938/f_dist.rds")
datafinal <- readRDS("wetransfer-ed1938/f_data.rds")

datafinalID <- copy(datafinal)[, cluster := pamfinal$clustering]


# 1. Timeline ganz
tl <- Cl.timeline(copy(datafinalID))

ggsave(plot = tl, "documentation/plots/PAMfinal/timeline.png", device = "png",
       width = 5, height = 3)

#1.1 Timeline multiplied
tlMult <- Cl.timeline(copy(datafinalID), multiplied = TRUE)
ggsave(plot = tlMult, "documentation/plots/PAMfinal/timelineMult.png", device = "png",
       width = 5, height = 3)

# 2. Timeline cut
timelineCut <- Cl.timeline(copy(datafinalID), cut = 41)

ggsave("documentation/plots/PAMfinal/timelineCut.png", timelineCut, device = "png",
       width = 5, height = 3)


# 3. Timeline nach Cluster ganz
timelineSep <- Cl.timeline(copy(datafinalID), seperated = TRUE)
ggsave("documentation/plots/PAMfinal/timelineSep.png", timelineSep, device = "png",
       width = 10, height = 6)

# 4. Timeline nach Cluster cut
timelineSepCut <- Cl.timeline(copy(datafinalID), seperated = TRUE, cut = 40)


ggsave("TimelineClusterCut", path = "Documentation/plots/PAMfinal/", device = "jpeg",
       width = 5, height = 3)


#silhouette plot

silli <- silhouette(PAMres$clustering, f_dist)

jpeg("documentation/plots/PAMfinal/f_sil.jpeg", width = 5, height = 3, res = 1000, units = "in")
output <- fviz_silhouette(sil.obj = silli,
                print.summary = FALSE, palette = "Set1",
                main = "Silhouettenplot", 
                submain = paste0("Silhouettenkoeffizient: ", round(mean(silli[, 3]), 3)),
                legend.title = "Cluster") + theme_classic() +
        scale_y_continuous(name = "Silhouette S(o)", limits = c(-0.3, 1),
                           breaks = seq(-0.25, 1, by = 0.25)) +
        theme(axis.text.x = element_blank(),
              axis.text.x.bottom = element_blank(),
              axis.ticks.x = element_blank())
output$layers[[2]]$aes_params$colour <- "black"
output
dev.off()


reanalyse <- readRDS("Data/data_reanalysis_20201109.rds")
a <- head(reanalyse, 4)
time <- c(rep("1900-01-01 00:00:00", 4), "1900-01-01 18:00:00", "1900-01-02 00:00:00", 
          rep("2010-12-31 18:00:00", 2))
a$time <- time
mytable <- tableGrob(a, rows = NULL, theme = ttheme_default(core = list(bg_params = list(fill = "grey99"))))
drw <- grid.draw(mytable)
?ggsave
ggsave(path = "Documentation/plots/PAMfinal/Data1.jpeg", grid.table(drw), device = "jpeg",
       width = 5, height = 3)

png("Data1.png", height = 800, width = 1600)
grid.arrange(mytable)
grid.table(a)
dev.off()



?grid.table
class(grid.table(a))


d$time <- time
d


ggsave("documentation/plots/PAMfinal/timelineSepCut.png",timelineSepCut, device = "png",
       width = 10, height = 6)

##timeline gwl multiplied

gwl <- readRDS("Data/gwl.rds")[format(date, "%Y") %in% seq(1971, 2000), ]

gwlTLm <- Cl.timeline(gwl, "gwl", multiplied = TRUE) + 
        labs(y = "Anzahl Tage", title = "Timeline GWL") +
        scale_x_continuous(breaks = c(seq(1, 25, by = 2)),
                           limits = c(0, 25))

ggsave("bericht/assets/timeline_GWL_mult.png", gwlTLm, device = "png",
       width = 5, height = 3)

##timeline Verteilung
dec_fun <- function(x) {
        if(x < 3 || x >= 40) {
                return(0)
        } else{
                if(x < 13){
                        return(1)
                }
        }
        return((23 / x) - (44 / x^2) - 0.55)
}
count <-  seq(1, 42)
TL.distr <- data.frame(Anteil = vapply(count, dec_fun, FUN.VALUE = numeric(1)),
                      count)

Tl.weight <- ggplot(TL.distr, aes(x = count, y = weights)) +
        geom_line() +
        geom_point() +
        scale_x_continuous(breaks = c(seq(1, 42, by = 2)),
                           limits = c(0, 42)) +
        labs(x = "Länge", y = "Gewicht", title = "Timeline - Gewichtungsfunktion") +
        theme_bw()

ggsave("bericht/assets/timeline_weights.png", Tl.weight, device = "png",
       width = 5, height = 3)


Tl.vtlg <- ggplot(TL.distr, aes(x = count, y = Anteil / sum(TL.distr$Anteil))) +
        geom_line() +
        geom_point() +
        scale_x_continuous(breaks = c(seq(1, 42, by = 2)),
                           limits = c(0, 42)) +
        labs(x = "Länge", y = "Anteil", title = "Optimale Timeline Verteilung") +
        theme_bw()

ggsave("bericht/assets/timeline_vtlg.png", Tl.vtlg, device = "png",
       width = 5, height = 3)



#kNN-dist plot
library(KneeArrower)
library(dbscan)
data <- readRDS("Data/cli_data_30_avgDay.rds")[date %in% 
                                as.Date("1980-01-01"),][, ":=" (date = NULL,
                                        avg_mslp = NULL)]
sc_oneDay <- as.data.table(scale(data))

#get density threshold
eps <- findCutoff(seq(1, nrow(sc_oneDay)),
                  sort(kNNdist(sc_oneDay, k = 10))
                  , method = "curvature")$y 

kNNdistplot(sc_oneDay, k = 10)

dist <- sort(kNNdist(sc_oneDay, k = 10))

kNN.dist <- ggplot(data = data.frame(x =seq(1, nrow(sc_oneDay)),
                         y = sort(kNNdist(sc_oneDay, k = 10))), 
       aes(x = x, y = y)) +
        geom_line()+
        geom_hline(aes(yintercept = findCutoff(seq(1, nrow(sc_oneDay)),
                                           sort(kNNdist(sc_oneDay, k = 10))
                                           , method = "curvature")$y, 
                       linetype = "eps = 0.877"), 
                   colour = "red", show.legend = T)+
        scale_linetype_manual(name="max. Wölbung", values = c(1))+
        theme_bw() +
        labs(title = "10-NN Distanz Geopot am 01.01.1980", x = "Beobachtung", y = "Distanz")
        


ggsave("bericht/assets/kNN_dist.png", kNN.dist, device = "png",
       width = 5, height = 3)


library(akmedoids)

ggplot(data = data.frame(x =seq(1, nrow(sc_oneDay)),
                         y = sort(kNNdist(sc_oneDay, k = 10))), 
       aes(x = x, y = y)) +
        geom_line()+
        geom_hline(yintercept = elbowPoint(seq(1, nrow(sc_oneDay)),
                                           sort(kNNdist(sc_oneDay, k = 10)))$y, 
                   colour = "red", linetype = "dashed")+
        theme_bw() +
        labs(title = "10-NN Distanz", x = "Beobachtung", y = "Distanz")


#geopot id plot

source("clustering/Var_Extraction_Method/f_extr_funs.R")

dat <- extrapolate(seq(1971, 2000))

dat_long <- copy(dat)[, c("date", "minGeopot.verID", "maxGeopot.verID")] %>%
        gather("Stat", "Value", -date) 

dat_long$Value <- factor(dat_long$Value, c(1,2,3))
levels(dat_long$Value) <-  c("Nord", "Mitte", "Süd")

geopot_verID <- ggplot(dat_long, aes(x =Value, fill = Stat)) +
        geom_bar(position = "dodge", aes(y = 2*(..count..)/sum(..count..))) +
        theme_bw() +
        labs(title = "Verteilung der Geopotential-Extrema", 
             x = "vertikale Position",
             y = "Anteil") +
        scale_fill_manual(values = c("red", "blue"), name = "Wert", 
                          labels = c("max. Geopot", "min. Geopot"))

ggsave("bericht/assets/geoVerID.png", geopot_verID, device = "png",
       width = 5, height = 3)
