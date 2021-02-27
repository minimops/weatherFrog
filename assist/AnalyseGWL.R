library(tidyverse)
library(data.table)
library(ggplot2)
library(data.table)
library(stringr)
library(dplyr)
library(plyr)



cli_gwl_1971 <- readRDS("Data\\cli_gwl_1971.rds")


a <- rle(cli_gwl_1971$gwl)

# gibt aus, wie viele Tage eine einzelne GWL andauert
lengthGWL <-as.data.frame(cbind(a[["values"]],a[["lengths"]]))
colnames(lengthGWL) <- c("gwl","length")
crosstable <- table(lengthGWL$gwl,lengthGWL$length)
# Die meisten GWL dauern 3 bis 7 Tage an 



# gibt aus, wie oft eine GWL im Zeitraum 1971 - 2010 vorkommt
GWLAnzahl <- as.data.frame(table(lengthGWL$gwl))
GWLAnzahl <-GWLAnzahl[order(GWLAnzahl$Freq),]
# GWL WZ kommt am haufigsten vor: 345 mal

jpeg(width = 2000,height =1000, pointsize = 29,quality = 100,"documentation/plots/AnalyseGWL/anzahlGWLs.jpeg")

barplot(table(lengthGWL$gwl), main = "Anzahl der GWLs 1971 - 2010", ylab = "Anzahl der GWLs"
        ,cex.names = 1.5, las = 2, ylim = c(0,350),cex.axis = 1.5,cex.main = 1.5, cex.lab = 1.5)
dev.off()


# Anzahl der GWLs je Laenge

table(as.numeric(lengthGWL$length))
jpeg(width = 2000,height =1000, pointsize = 29,quality = 100,"documentation/plots/AnalyseGWL/anzahlLängeGWLs.jpeg")
barplot(table(as.numeric(lengthGWL$length)), main = "Anzahl der Länge der GWLs",
        xlab = "Länge der GWLs", ylab = "Häufigkeit", ylim = c(0,700),cex.axis = 1.5,cex.names = 1.5,cex.main = 1.5, cex.lab = 1.5)
dev.off()
################################
# Gibt es saisonale Unterschiede im Aufkommen der Wetterlagen?

###########################

#Analyse der GWLs nach Jahreszeiten


#Meterologische Jahreszeiten: 
#Winter: 1. 12. - 28./29. 2.
#Frühling: 1. 3. bis 31. 5. 
#Sommer: 1. 6. bis 31. 8.
#Herbst: 1.9 bis 20.11

#Spalte Jahreszeit cli_gwl_1971 hinzufügen

cli_gwl_1971 <- cli_gwl_1971 %>%
  mutate(Jahreszeit = case_when(month %in% c("12", "01", "02") ~ "Winter",
                                month %in% c("03", "04", "05") ~ "Fruehling",
                                month %in% c("06", "07", "08") ~ "Sommer",
                                month %in% c("09", "10", "11") ~ "Herbst") )

setcolorder(cli_gwl_1971,c("index_length_gwl","id","Jahreszeit"))

# Alternative: nur 2 Jahreszeiten:
#Winterzeit: 16.10 bis 15.4 
#Sommerzeit: 16.4 bis 15.10

cli_gwl_1971_Month_Day <- cli_gwl_1971
cli_gwl_1971_Month_Day$Month_Day <- format(cli_gwl_1971_Month_Day$date, "%m-%d")

day_16_to_31 <- seq(from = 16, to = 31, by = 1)
day_16_to_30 <- seq(from = 16, to = 30, by = 1)
day_1_to_9 <- seq(from = 1, to = 9, by = 1)
day_1_to_9 <- paste0(0, day_1_to_9)
day_10_to_15 <- seq(from = 10, to = 15, by = 1)
day_1_to_15 <- c(day_1_to_9,day_10_to_15)

month_10_day_16_31 <- paste(10,day_16_to_31,sep = "-")
month_4_day_1_15 <- paste("04",day_1_to_15,sep = "-")
month_4_day_16_30 <- paste("04",day_16_to_30,sep = "-")
month_10_day_1_15 <- paste(10,day_1_to_15,sep = "-")




 cli_gwl_1971_Month_Day <- cli_gwl_1971_Month_Day %>%
   mutate(Jahreszeit = case_when(month %in% c("11","12","01","02","03") ~ "Winterzeit",
                                 month %in% c("05","06","07","08","09") ~ "Sommerzeit",
                                 Month_Day %in% month_10_day_16_31 ~ "Winterzeit",
                                 Month_Day %in% month_4_day_1_15 ~ "Winterzeit",
                                 Month_Day %in% month_4_day_16_30 ~ "Sommerzeit",
                                 Month_Day %in% month_10_day_1_15 ~ "Sommerzeit"))
 
data_2000 <- cli_gwl_1971_Month_Day[cli_gwl_1971_Month_Day$date <= "2000-12-31",]
Jahreszeit <- data_2000$Jahreszeit
setcolorder(cli_gwl_1971_Month_Day,c("index_length_gwl","id","Jahreszeit"))
cli_gwl_1971_Month_Day <- cli_gwl_1971_Month_Day[,-329]
 
#Laenge der GWLs nach den 4 Jahreszeiten gruppiert berechnen

cli_gwl_1971 <- as.data.table(cli_gwl_1971)
gwlNachJahreszeit <- cli_gwl_1971[,(rle(gwl)), by = Jahreszeit]
table(gwlNachJahreszeit$values,gwlNachJahreszeit$Jahreszeit)
colSums(table(gwlNachJahreszeit$values,gwlNachJahreszeit$Jahreszeit))

#Laenge der GWLs nach den 2 Jahreszeiten gruppiert berechnen

gwlNachJahreszeit2 <- cli_gwl_1971_Month_Day[,(rle(gwl)), by = Jahreszeit]
table(gwlNachJahreszeit2$values,gwlNachJahreszeit2$Jahreszeit)
colSums(table(gwlNachJahreszeit2$values,gwlNachJahreszeit2$Jahreszeit))



# Anzahl der GWLs gruppiert nach  den 4 Jahreszeiten

GWLJahreszeiten <- as.data.table(table(gwlNachJahreszeit$values,gwlNachJahreszeit$Jahreszeit))
barplot(t(table(gwlNachJahreszeit$values,gwlNachJahreszeit$Jahreszeit)), col = terrain.colors(4))
mosaicplot(t(table(gwlNachJahreszeit$values,gwlNachJahreszeit$Jahreszeit)), col = terrain.colors(31), las = 1)



jpeg(width = 1500,height =1000, pointsize = 29,quality = 100,"documentation/plots/AnalyseGWL/GWLs4Jahreszeiten.jpeg")
mosaicplot(table(gwlNachJahreszeit$values,gwlNachJahreszeit$Jahreszeit), col = terrain.colors(4), las = 2, main = "Anzahl der   GWLs in Abhängigkeit der  4Jahreszeiten",
           cex.axis = 1.0)
dev.off()

jpeg(width = 2000,height =1000, pointsize = 29,quality = 100,"documentation/plots/AnalyseGWLgesamtanzahlGWLs4.jpeg")
barplot(colSums(table(gwlNachJahreszeit$values,gwlNachJahreszeit$Jahreszeit)), col = rainbow(4), main = "Gesamtanzahl der GWLs in Abhängigkeit der 4 Jahreszeiten",
        ylab = "Anzahl", ylim = c(0,800),cex.main = 1.5,cex.axis = 1.5, cex.lab = 1.5)
dev.off()


# Anzahl der GWLs gruppiert nach  den 2 Jahreszeiten


jpeg(width = 1500,height =1000, pointsize = 29,quality = 100,"documentation/plots/AnalyseGWL/GWLs_2_Jahreszeiten.jpeg")
mosaicplot(table(gwlNachJahreszeit2$values,gwlNachJahreszeit2$Jahreszeit), col = terrain.colors(4), las = 2, main = "Anzahl der   GWLs in Abhängigkeit der  2Jahreszeiten",
           cex.axis = 1.0)
dev.off()

jpeg(width = 2000,height =1000, pointsize = 29,quality = 100,"documentation/plots/AnalyseGWLgesamtanzahlGWLs_2_Jahreszeiten.jpeg")
barplot(colSums(table(gwlNachJahreszeit2$values,gwlNachJahreszeit2$Jahreszeit)), col = rainbow(4), main = "Gesamtanzahl der GWLs in Abhängigkeit der 2 Jahreszeiten",
        ylab = "Anzahl", ylim = c(0,1700),cex.main = 1.5,cex.axis = 1.5, cex.lab = 1.5)
dev.off()






# Verteilung der Messwerte von Mslp und Geopot ueber 4 Jahreszeiten

cli_gwl_long <- melt(cli_gwl_1971, id.vars = c("Jahreszeit","index_length_gwl","date","gwl"), measure.vars = c(colnames(cli_gwl_1971[,9 : 328])))
cli_gwl_long_mslp <- melt(cli_gwl_1971, id.vars = c("Jahreszeit","index_length_gwl","date","gwl"), measure.vars = c(colnames(cli_gwl_1971[,9 : 168])))
cli_gwl_long_geo <- melt(cli_gwl_1971, id.vars = c("Jahreszeit","index_length_gwl","date","gwl"), measure.vars = c(colnames(cli_gwl_1971[,169 : 328])))


jpeg(width = 2000,height =1000, pointsize = 29,quality = 100,"documentation/plots/AnalyseGWL/MslpRange4Jahreszeiten.jpeg")
boxplot(cli_gwl_long_mslp$value ~ cli_gwl_long_mslp$Jahreszeit, main = "range des Luftdrucks in Abhängigkeit der  4 Jahreszeiten",
        xlab = "Jahreszeit", ylab = "range des Luftdrucks in Pa",cex.main = 1.5, cex.lab = 1.5, cex.axis = 1.5)
dev.off()
jpeg(width = 2000,height =1000, pointsize = 29,quality = 100,"documentation/plots/AnalyseGWL/GeopotentailRange4Jahreszeiten.jpeg")
boxplot(cli_gwl_long_geo$value ~ cli_gwl_long_geo$Jahreszeit, main = "range des Geopotentials in Abhängigkeit der  4 Jahreszeiten",
        xlab = "Jahreszeit", ylab = "range des Geopotentials in m²/s²",cex.main = 1.5, cex.lab = 1.5, cex.axis = 1.5)
dev.off()

# Verteilung der Messwerte von Mslp und Geopot ueber 2 Jahreszeiten

cli_gwl_long2 <- melt(cli_gwl_1971_Month_Day, id.vars = c("Jahreszeit","index_length_gwl","date","gwl"), measure.vars = c(colnames(cli_gwl_1971_Month_Day[,9 : 328])))
cli_gwl_long_mslp2 <- reshape2::melt(cli_gwl_1971_Month_Day, id.vars = c("Jahreszeit","index_length_gwl","date","gwl"), measure.vars = c(colnames(cli_gwl_1971_Month_Day[,9 : 168])))
cli_gwl_long_geo2 <- reshape2::melt(cli_gwl_1971_Month_Day, id.vars = c("Jahreszeit","index_length_gwl","date","gwl"), measure.vars = c(colnames(cli_gwl_1971_Month_Day[,169 : 328])))


#jpeg(width = 2000,height =1000, pointsize = 29,quality = 100,"documentation/plots/AnalyseGWL/MslpRange2Jahreszeiten.jpeg")
boxplot(cli_gwl_long_mslp2$value ~ cli_gwl_long_mslp2$Jahreszeit, main = "range des Luftdrucks in Abhängigkeit der 2 Jahreszeiten",
        xlab = "Jahreszeit", ylab = "range des Luftdrucks in Pa",cex.main = 1.5, cex.lab = 1.5, cex.axis = 1.5)
dev.off()
jpeg(width = 2000,height =1000, pointsize = 29,quality = 100,"documentation/plots/AnalyseGWL/GeopotentailRange2Jahreszeiten.jpeg")
boxplot(cli_gwl_long_geo2$value ~ cli_gwl_long_geo2$Jahreszeit, main = "range des Geopotentials in Abhängigkeit der 2 Jahreszeiten",
        xlab = "Jahreszeit", ylab = "range des Geopotentials in m²/s²",cex.main = 1.5, cex.lab = 1.5, cex.axis = 1.5)
dev.off()


boxplot(cli_gwl_long_mslp2$value ~ cli_gwl_long_mslp2$Jahreszeit, main = "range des Luftdrucks in Abhängigkeit der 2 Jahreszeiten",
       xlab = "Jahreszeit", ylab = "range des Luftdrucks in Pa",cex.main = 1.5, cex.lab = 1.5, cex.axis = 1.5)


cli_gwl_long_mslp2$value <- cli_gwl_long_mslp2$value/100

 ggplot(aes(x=Jahreszeit, y=value, fill = Jahreszeit), data = cli_gwl_long_mslp2) + 
  geom_boxplot() +
  theme_bw() +
  theme(legend.position="none") +
  xlab("Jahreszeit") +
  ylab ("Luftdruck in [hPa]")+
  ggtitle ("Verteilung des Luftdrucks bezogen auf die Jahreszeit") +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1")

ggsave(plot, file="final_cluster/jahreszeit_mslp.png")


cli_gwl_long_geo2$value <- cli_gwl_long_geo2$value/9.80665

plot <- ggplot(aes(x=Jahreszeit, y=value, fill = Jahreszeit), data = cli_gwl_long_geo2) + 
  geom_boxplot() +
  theme_bw() +
  theme(legend.position="none") +
  xlab("Jahreszeit") +
  ylab ("Geopotential in gpm")+
  ggtitle ("Verteilung des Geopotentials bezogen auf die Jahreszeit") +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1")

ggsave(plot, file="final_cluster/jahreszeit_geo.png")






#Verteilung der einzelnen GWLs über Jahreszeit


gwlUeberJahreszeit <- function(GWL){
  # cli_gwl_long <- melt(cli_gwl_1971, id.vars = c("Jahreszeit","index_length_gwl","date","gwl"), measure.vars = c(colnames(cli_gwl_1971[,9 : 328])))
  dataTable <- cli_gwl_long_mslp2[cli_gwl_long_mslp2$gwl == GWL,]
  boxplot(dataTable$value ~ dataTable$Jahreszeit,ylab = GWL, main = "Mslp")
}


gwlUeberJahreszeit("BM")
gwlUeberJahreszeit("HB")
gwlUeberJahreszeit("HFA")
gwlUeberJahreszeit("HFZ")
gwlUeberJahreszeit("HM")
gwlUeberJahreszeit("HNA")
gwlUeberJahreszeit("HNFA")
gwlUeberJahreszeit("HNFZ")
gwlUeberJahreszeit("HNZ")
gwlUeberJahreszeit("NA")
gwlUeberJahreszeit("NEA")
gwlUeberJahreszeit("NEZ")
gwlUeberJahreszeit("NWA")
gwlUeberJahreszeit("NWZ")
gwlUeberJahreszeit("NZ")
gwlUeberJahreszeit("SA")
gwlUeberJahreszeit("SEA")
gwlUeberJahreszeit("SEZ")
gwlUeberJahreszeit("SWA")
gwlUeberJahreszeit("SWZ")
gwlUeberJahreszeit("SZ")
gwlUeberJahreszeit("TB")
gwlUeberJahreszeit("TM")
gwlUeberJahreszeit("TRM")
gwlUeberJahreszeit("TRW")
gwlUeberJahreszeit("WA")
gwlUeberJahreszeit("WS")
gwlUeberJahreszeit("WW")
gwlUeberJahreszeit("WZ")

# Andere Analyse für : Sind die Messwerte der GWLs inden verschiedenen
#Jahreszeiten verschieden?

# Mittelwert und Median pro GWL ausrechenen

meanJahreszeitenMslp <- cli_gwl_long_mslp[, lapply(.SD, function (x) mean(x)), by = c("gwl","Jahreszeit"), .SDcols = 6]
meanJahreszeitenGeo <- cli_gwl_long_geo[, lapply(.SD, function (x) mean(x)), by = c("gwl","Jahreszeit"), .SDcols = 6]

medianJahreszeitenMslp <- cli_gwl_long_mslp[, lapply(.SD, function (x) median(x)), by = c("gwl","Jahreszeit"), .SDcols = 6]
medianJahreszeitenGeo <- cli_gwl_long_geo[, lapply(.SD, function (x) median(x)), by = c("gwl","Jahreszeit"), .SDcols = 6]


# Streeung der Mittelwerte der Jahreszeiten pro GWL ausrechnen

sdMeanJahreszeitenMslp <- meanJahreszeitenMslp[, lapply(.SD, function(x) sd(x)), by = gwl, .SDcols = 3]
sdMeanJahreszeitenGeo <- meanJahreszeitenGeo[, lapply(.SD, function(x) sd(x)), by = gwl, .SDcols = 3]

sdMedianJahreszeitenMslp <- medianJahreszeitenMslp[, lapply(.SD, function(x) sd(x)), by = gwl, .SDcols = 3]
sdMedianJahreszeitenGeo <- medianJahreszeitenGeo[, lapply(.SD, function(x) sd(x)), by = gwl, .SDcols = 3]

#Barplot

# Barplot Streuung der Mittelwerte der einzelnen Jahreszeitem für Mslp
mslpValue <- sdMeanJahreszeitenMslp$value
names(mslpValue) <- sdMeanJahreszeitenMslp$gwl

jpeg(width = 2000,height =1000, pointsize = 29,quality = 100,"documentation/plots/AnalyseGWL/streungMslpMeanProJahreszeit.jpeg")
barplot(mslpValue, main = "Mslp Mean")
dev.off()

# Barplot Streuung der Mittelwerte der einzelnen Jahreszeitem für Geopotential
GeoValue <- sdMeanJahreszeitenGeo$value
names(GeoValue) <- sdMeanJahreszeitenGeo$gwl
jpeg(width = 2000,height =1000, pointsize = 29,quality = 100,"documentation/plots/AnalyseGWL/streungGeoMeanProJahreszeit.jpeg")
barplot(GeoValue, main = "Geo Mean")
dev.off()

# das gleiche für median
mslpValueMedian <- sdMedianJahreszeitenMslp$value
names(mslpValueMedian) <- sdMedianJahreszeitenMslp$gwl
jpeg(width = 2000,height =1000, pointsize = 29,quality = 100,"documentation/plots/AnalyseGWL/streungMslpMedianProJahreszeit.jpeg")
barplot(mslpValueMedian, main = "Mslp Median")
dev.off()

GeoValueMedian <- sdMedianJahreszeitenGeo$value
names(GeoValueMedian) <- sdMeanJahreszeitenGeo$gwl
jpeg(width = 2000,height =1000, pointsize = 29,quality = 100,"documentation/plots/AnalyseGWL/streungGeoMedianProJahreszeit.jpeg")
barplot(GeoValueMedian, main = "Geo Mean")
dev.off()




############################################
#Analyse, ob erster und letzter Tag in einer GWL von den Messwerten der inneren Tage unterschiedlich ist

# Alle GWLs, die weniger als 4 Tage andauern, löschen
clii <- cli_gwl_1971[,.(.N), by = index_length_gwl]
cli_gwl_1971 <- merge(clii, cli_gwl_1971,by = "index_length_gwl")

cli_gwl_groesser3 <- cli_gwl_1971 %>%
  group_by(index_length_gwl) %>%
  filter(N >3)
cli_gwl_groesser3 <- as.data.table(cli_gwl_groesser3)


# Range pro Tag ueber alle Standorte berechnen je GWL

rangeProTagMslp <- as.data.table(t(apply(cli_gwl_groesser3[,10:169], 1, range)))
colnames(rangeProTagMslp) <- c("minMslp", "maxMslp")
rangeProTagMslp <- cbind(cli_gwl_groesser3[,1:9],rangeProTagMslp)

rangeProTagGeo <- as.data.table(t(apply(cli_gwl_groesser3[,170 : 329], 1, range)))
colnames(rangeProTagGeo) <- c("minGeo", "maxGeo")
rangeProTagGeo <- cbind(cli_gwl_groesser3[,1:9],rangeProTagGeo)

rangeProTagMslp <- melt(rangeProTagMslp, id.vars = c("index_length_gwl", "N", "id", "Jahreszeit","date",
                                                     "year", "month", "day", "gwl"),
                        measure.vars = c("minMslp","maxMslp"))

rangeProTagGeo <- melt(rangeProTagGeo, id.vars = c("index_length_gwl", "N", "id", "Jahreszeit","date",
                                                   "year", "month", "day", "gwl"),
                       measure.vars = c("minGeo","maxGeo"))
colnames(rangeProTagMslp)[colnames(rangeProTagMslp) == "variable"] <- "MinMaxMslp"
colnames(rangeProTagMslp)[colnames(rangeProTagMslp) == "value"] <- "Mslp"

colnames(rangeProTagGeo)[colnames(rangeProTagGeo) == "variable"] <- "MinMaxGeo"
colnames(rangeProTagGeo)[colnames(rangeProTagGeo) == "value"] <- "Geo"

ranges <- cbind(rangeProTagMslp, rangeProTagGeo[,10 : 11])



#Berechnung

rangesProGwl <- ranges[, lapply(.SD, function(x) range(x)), by = index_length_gwl, .SDcols = c(11,13)] 
rangesDiff <- rangesProGwl[,lapply(.SD, function(x) x - lag(x)),by = index_length_gwl, .SDcols = c(2,3)]
rangesDiff <- na.omit(rangesDiff)

#######Berechung der ranges über alle Standorte pro GWL ohne den ersten und letzten Tag

#cli_gwl_inner erstellen (GWL ohne den ersten und letzten Tag)
cli_gwl_inner <- ddply(cli_gwl_1971, .(index_length_gwl),function(x) x[c(2 : (nrow(x) -1)), ])




rangeProTagMslpInner <- as.data.table(t(apply(cli_gwl_inner[,10:169], 1, range)))
colnames(rangeProTagMslpInner) <- c("minMslp", "maxMslp")
rangeProTagMslpInner <- cbind(cli_gwl_inner[,1:9],rangeProTagMslpInner)

rangeProTagGeoInner <- as.data.table(t(apply(cli_gwl_inner[,170 : 329], 1, range)))
colnames(rangeProTagGeoInner) <- c("minGeo", "maxGeo")
rangeProTagGeoInner <- cbind(cli_gwl_inner[,1:9],rangeProTagGeoInner)

rangeProTagMslpInner <- as.data.table(rangeProTagMslpInner)
rangeProTagMslpInner <- melt(rangeProTagMslpInner, id.vars = c("index_length_gwl", "N", "id", "Jahreszeit","date",
                                                               "year", "month", "day", "gwl"),
                             measure.vars = c("minMslp","maxMslp"))

rangeProTagGeoInner <- as.data.table(rangeProTagGeoInner)
rangeProTagGeoInner <- melt(rangeProTagGeoInner, id.vars = c("index_length_gwl", "N", "id", "Jahreszeit","date",
                                                             "year", "month", "day", "gwl"),
                            measure.vars = c("minGeo","maxGeo"))
colnames(rangeProTagMslpInner)[colnames(rangeProTagMslpInner) == "variable"] <- "MinMaxMslp"
colnames(rangeProTagMslpInner)[colnames(rangeProTagMslpInner) == "value"] <- "Mslp"

colnames(rangeProTagGeoInner)[colnames(rangeProTagGeoInner) == "variable"] <- "MinMaxGeo"
colnames(rangeProTagGeoInner)[colnames(rangeProTagGeoInner) == "value"] <- "Geo"

rangesProGwlInner <- cbind(rangeProTagMslpInner, rangeProTagGeoInner[,10 : 11])

#########
rangesDiffInner <- rangesProGwlInner[,lapply(.SD, function(x) x - lag(x)),by = index_length_gwl, .SDcols = c(2,3)]
rangesDiffInner <- na.omit(rangesDiffInner)

colnames(rangesDiffInner) <- c("index_length_gwl","MslpInner","GeoInner")
rangesGWL <- merge(rangesDiff, rangesDiffInner, by = "index_length_gwl")


jpeg(width = 1000,height = 1700, pointsize = 29,quality = 100,"documentation/plots/AnalyseGWL/rangeGWLMslp.jpeg")
boxplot(rangesGWL[,c(2,4)], main = "Vergleich ranges pro GWL ohne 
        und mit ersten und letzen Tag einer GWL",
        ylab = "Luftdruck in Pa",cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
dev.off()

jpeg(width = 1000,height =1700, pointsize = 29,quality = 100,"documentation/plots/AnalyseGWL/rangeGWLGeo.jpeg")
boxplot(rangesGWL[,c(3,5)], main = "Vergleich ranges pro GWL ohne 
        und mit ersten und letzen Tag einer GWL",
        ylab = "Geopotential in m²/s²",cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
dev.off()


###########Durchführen eines Signifikanztest, ob Unterschiede signifikannt sind

# Prüfen auf Testvoraussetzungen
#Normalverteilung?

ks.test(rangesGWL$Geo,"pnorm", mean = mean(rangesGWL$Geo), sd = sd(rangesGWL$Geo))

# keine Normalverteilung

#Varianzen gleich?
var.test(rangesGWL$Geo,rangesGWL$GeoInner)
# Varianzen sind gleich

# Wech Test
t.test(rangesGWL$Geo, rangesGWL$GeoInner, alternative = "two.sided" , 
       paired = T, var.equal = T)
# Die Mittelwerte der ranges mit und ohne die ersten und letzten Tage bei 
# Geopotential sind signifikannt unterschiedlich

ks.test(rangesGWL$Mslp,"pnorm", mean = mean(rangesGWL$Mslp), sd = sd(rangesGWL$Mslp))
var.test(rangesGWL$Mslp,rangesGWL$MslpInner)
t.test(rangesGWL$Mslp, rangesGWL$MslpInner, alternatlive = "two.sided" , 
       paired = T, var.equal = T)

# Mittelwerte mit und ohne ersten und letzten Tag bei Luftdruck sind signifiaknnt 
#unterschiedlich

#Gibt es bestimmte Tage, wo die range besonders groß ist? Alle Werte herausfiltern,
# die nicht im IQR liegen

IQR(rangesGWL$Mslp)
IQR(rangesGWL$MslpInner)

IQR(rangesGWL$Geo)
IQR(rangesGWL$GeoInner)
quantile(rangesGWL$Mslp, 0.25)

# IQR der ranges sind sehr gleich
# schauen, wie viele ranges der GWL im IQR derranges der GWL ohne ersten und letzten Tag drin ist   
a <- rangesGWL$Mslp[rangesGWL$Mslp  %inrange% c(quantile(rangesGWL$MslpInner, 0.25), quantile(rangesGWL$MslpInner, 0.75))]
a <- rangesGWL$Geo[rangesGWL$Geo  %inrange% c(quantile(rangesGWL$GeoInner, 0.25), quantile(rangesGWL$GeoInner, 0.75))]

