# weatherFrog
## LMU StatPrakt WS 20/21
### large-scale weather conditions in europe

Datum der Präsentation: 01.03.2021  
Institut: Statistik, LMU München  
Kurs: Statistisches Praktikum  
Projektpartner: M.Sc. Maximilian Weigert, M.Sc. Magdalena Mittermeier  
Betreuer: Prof. Dr. Helmut Küchenhoff  
Autoren: Anne Gritto, Katja Gutmair, Stella Akouete, Noah Hurmer


Um Tage anhand ihrer Wetterdaten in Gruppen oder soganannte Wetterlagen einzuteilen, wird hier explorativ nach unsupervised Verfahren gesucht, welche dies erreichen. Dafür liegen pro Tag Messdaten zum Luftdruck auf Meeresspiegelhöhe sowie Geopotential auf 500 hPa an mehreren Standorten vor. Mit diesen Informationen werden verschiedene Clusteranalysen durchgeführt und dessen Ergebnisse verglichen sowie Bezug auf eine vorhandene Einteilung in Großwetterlagen nach Hess und Brezowsky genommen. Um Clusterergebnisse vergleichen zu können, werden Kriterien vorgestellt und diskutiert, die den Erfolg und die fachliche Sinnhaftigkeit repräsentieren. Zwei Verfahren werden hier genauer beschrieben. Dabei befasst sich ein folgend Filter-Ansatz genanntes Verfahren mit der räumlichen Struktur bestimmter Gebiete, indem durch ein zweistufiges Clustern pro Tag Gebiete von Interesse gefunden werden, um Tage folglich anhand dessen zu vergleichen. Der zweite Ansatz extrahiert aus dem Datensatz repräsentative Variablen, um die räumliche Struktur der Daten zu entfernen, somit eine Clusteranalyse zu erleichtern, sowie bestimmten Ausprägungen mehr Einfluss zuordnen zu können. Für letzteren Ansatz werden Ausprägungen der Cluster deskriptiv analysiert und die Aufteilung der Großwetterlagen in den Clustern genauer betrachtet. Abschließend werden auf beobachtete Probleme in den Verfahren sowie mögliche Verbesserungsvorschläge eingegangen.


### Setup

Um Codes in de Dateien auszuführen, ist es wichtig, dass zuerst *dataset_mutate.Rmd* durchgelaufen wird, um die benötigten Datensätze zu kreieren und zu speichern.
Dafür muss zuerst ein Order "Data" in dem Projekt erstellt werden, dem rie rohen Datensätze hinzugefügt werden: *"GWL_1900-2010.csv", "data_reanalysis_20201109.rds"*.
Diese sind hier nicht enthalten, da sie sehr groß sind. Informationen über die erstellten Datensätze beinhaltet die Datei *dataset_doc.Rmd* im Ordner *documentation*.
 
### Dependencies

data.table, tidyverse, rgeos, gstat, rnaturalearth, cluster, ggfortify, tsne, checkmate, parallel, ggmosaic, plyr, factoextra, gridExtra, parallelDist,
mlr3, ppclust, fclust, Rtsne, dbscan, KneeArrower, clusterR, grid, e1071, akmedoids 
