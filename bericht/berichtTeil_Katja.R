
# Deskriptive Analyse der Clusterergebnisse

Dieser Abschnitt präsentiert die Ergebnisse der deskriptiven Analyse der finalen Clusterlösung. Hierbei wird in Abschnitt 3.1 betrachtet, wie die Cluster eins bis sechs über die Jahre 1971 bis 2010 verteilt sind. Zudem wird das Verhältnis von Sommer- und Wintertagen in den einzelnen Clustern betrachtet.  Abschnitt 3.2 befasst sich mit den Ähnlichkeiten und Unterschieden der extrahierten 49 Variablen in den Clustern. Abschließend wird in Abschnitt 3.3 die Clusterlösung mit der GWL Einteilung nach Hess und Brezowsky verglichen, also in welchem Ausmaß die GWLs über die Cluster verteilt sind.

## Verteilung der Cluster über die Zeit

Abb. \ref{fig:distribution_of_years_over_cluster} stellt dar, wie häufig jedes Cluster im Zeitraum 1971 bis 2000 vorkommt. 
Zu erkennen ist, dass sich ein Cluster nicht auf eine bestimmte Zeitperiode beschränkt, sondern über den gesamten Zeitraum erstreckt.
Die Cluster eins bis drei sind heterogen über den Zeitraum von 1971 bis 2000 verteilt. Beispielsweise beinhaltet das Jahr 1985 6% aller Tage, die Cluster 1 zugeordnet sind, das Jahr 1990 beinhaltet hingegen nur 2% aller Tage, die Cluster 1 zugeordnet sind. Damit verglichen sind Cluster vier bis sechs sehr gleichmäßig über alle Jahre verteilt. In Cluster 4 sticht das Jahr 2000 hervor. In diesem sind 6,3% aller Tage, die Cluster 4 zugeordnet sind, vertreten, während in allen anderen Jahren durchschnittlich je 3% aller Tage, die Cluster 4 zugeordnet sind, vertreten sind. Somit beinhaltet das Jahr 2000 ca. doppelt so viele Cluster 4-Tage verglichen mit den anderen Jahren. Von allen Clustern ist Cluster 6 am gleichmäßigsten über alle Jahre verteilt. 

\begin{figure}[h]
\caption{Verteilung der Cluster über die Jahre}
\includegraphics{distribution_of_years_over_cluster}
\centering
\label{fig:distribution_of_years_over_cluster}
\end{figure}


Temperaturen im Winter sind durchschnittlich niedriger als im Winter. Deshalb wurde angenommen, dass diese saisonalen Unterschiede eventuell sich in der Clusterlösung wieder spiegeln. 
Um saisonale Unterschiede und deren Verteilung in den Clustern zu betrachten, wurde ein Kalenderjahr in Winter- und Sommertage aufgeteilt. Alle Tage im Zeitraum 16. Oktober bis 15. April werden als Wintertage definiert, die restlichen Tage folglich als Sommertage. 
Der Mosaikplot in Abb. \ref{fig:mosaic_seasons} visualisiert die relativen Häufigkeiten der Winter- und Sommertage in jedem Cluster. 
Cluster 1 bis 3 enthält überwiegend Wintertage; in Cluster 1 sind 88% aller Tage Wintertage, in Cluster 2 99% und in Cluster 3 98% aller Tage Wintertage. In Cluster 4 bis 6 sind hingegen überwiegend Sommertage vertreten; in Cluster 4 sind 79% aller Tage Sommertage und in Cluster 5 83% aller Tage Sommertage. Ein Sonderfall ist Cluster 6, da dieses ausschließlich aus Sommertagen besteht. 

\begin{figure}[h]
\caption{Vorkommen von Winter- und Sommertagen in jedem Cluster}
\includegraphics{mosaic_seasons}
\centering
\label{fig:mosaic_seasons}
\end{figure}


## Unterschiede und Ähnlichkeiten in den Clustern

Dieser Abschnitt befasst sich mit der Fragestellung, wie sich die Werte der 49 extrahierten Variablen, mit denen geclustert wurde, zwischen den Clustern unterscheiden. Dazu werden Mittelwerte, Standardabweichung und Verteilungen ausgewählter, repräsentativer Variablen betrachtet und 
dessen räumliche Verteilung über die 160 Messorte.

Abb. \ref{fig:observation_in_clusterzghjn} zeigt die Aufteilung der Tage im Zeitraum 1971 bis 2000 auf die Cluster 1 bis 6. Hierbei bildet Cluster 3 mit einer Anzahl an 1422 Tagen das kleinste Cluster. Cluster 6 beinhaltet die meisten Tage (2565 Tage). Während in Cluster 2 1952 Tage vertreten sind, beinhalten Cluster 1,4 und 5 eine ähnliche Anzahl an Tagen.

\begin{figure}[h]
\caption{Verteilung der Tage im Zeitraum 1971 - 2000 auf die Cluster 1 bis 6}
\includegraphics{number_of_observations_in_each_cluster}
\centering
\label{fig:observation_in_cluster}
\end{figure}



Tabelle \ref{table:descriptive_cluster} bildet Mittelwert und Standardabweichung der Variablen mean.mslp, max, mslp, mean.geopot, max.geopt und min.geopot in jedem Cluster ab. Hierbei unterschieden sich die Werte einer Variablen in jedem Cluster nur gering voneinander. Zum Beispiel beträgt die maximale Abweichung der Variable mean.mslp über alle Cluster nur 8,2 hPa und die maximale Abweichung der Variable mean.geopot 287,7 gpm.

\begin{table}[h]
\centering
\includegraphics{table_descriptive_cluster}
\caption{: Mittelwert und Standardabweichung ausgewählter Variablen pro Cluster. Alle Werte sind in der Einheit hPa für die Variablen mslp und gpm für die Variablen geopot}
\label{table:descriptive_cluster}

Abb. \ref{fig: mean_mslp_boxplot} und Abb. \ref{fig:mean_geopot_boxplot} bilden die Verteilung der Variablen mean.mslp und mean.geopot je Cluster ab. Verglichen mit der Variable mean.geopot unterschiedet sich der Median und die Verteilung von mean.mslp in jedem Cluster wenig. Hierbei beinhaltet Cluster 2 mit einem Median von 1009,5 hPa und einem Interquartilsabstand(IQR) von 3,8 hPa (1. Quartil = 1007,4  3.Quartil = 1011,2 hPa)  tendenziell die kleinsten Werte auf, während Cluster 5 mit einem Median von 1017 hPa und einem Interquartilsabstand von 3 hPa (1.Quartil = 1016 hPa, 3. Quartil = 1019 hPa) tendenziell die höchsten Werte aufweist. Zudem beinhaltet jedes Cluster mehrere Ausreiser. Ein Wert wird als Ausreiser definiert, wenn er außerhalb des Intervalls [median ±IQR] liegt.
Bei der Variable mean.geopot weisen die Werte der Cluster 4 bis 6 deutlich höhere Werte auf als die Werte in den Clustern 1 bis 3. Wie bei der Variable mean.mslp beinhaltet auch das Cluster 2 bei der Variable mean.geopot tendenziell die niedrigsten Werte mit einem Median von 5360 gpm und einem IQR von 58 gpm (1.Quartil = 5334 gpm, 3. Quartil = 5392 gpm). Cluster 6 weist tendenziell die höchsten Werte mit einem Median von 5654 gpm und einem IQR von 42 gpm (1.Quartil = 5632 gpm , 3. Quartil = 5674 gpm). Zudem weisen Cluster 2,3 und 6 Ausreiser auf.

\begin{figure}[h]
\caption{Verteilung der Variable mean.mslp in jedem Cluster}
\includegraphics{mean_mslp_boxplot}
\centering
\label{fig:mean_mslp_boxplot}
\end{figure}

\begin{figure}[h]
\caption{Verteilung der Variable mean.geopot in jedem Cluster}
\includegraphics{mean_geopot_boxplot}
\centering
\label{fig:mean_geopot_boxplot}
\end{figure}


Alle anderen extrahierten Variablen, die in die Clusteranalyse miteingegangen sind und die die Verteilung beschreiben, ähneln den beschriebenen Ergebnissen von mean.mslp und mean. geopot. Star, so dass mean. mslp und mean. geopot representativ für die übrigen Verteilungsvariablen stehen uns deshalb hier nicht erwähnt werden.


Nun wird betrachtet, wie sich der Mittelwert des Luftdrucks und  der Mittelwert des Geopotentials räumlich unterscheidet. Abb \ref{fig:mslp_measurepoints} bzw. Abb.\ref{fig:geopot_measurepoints} stellt pro  Cluster und für jeden der insgesamt 160 Standorte das arithmetrische Mittel der mslp bzw. geoppot Werte im Zeitraums 1971 – 2000 dar.
Blau Flächen visualisieren Gebiete mit niedrigen, rote Flächen visualisieren Gebiete mit hohen Werten. 

\begin{figure}[h]
\caption{Räumliche Verteilung des gemittelten Luftdrucks über 30 Jahre an 160 Standorten je Cluster}
\includegraphics{mslp_over_all_measurepoints}
\centering
\label{fig:mslp_measurepoints}
\end{figure}

\begin{figure}[h]
\caption{Räumliche Verteilung des gemittelten Geopotentials über 30 Jahre an 160 Standorten je Cluster}
\includegraphics{geopot_measurepoints}
\centering
\label{fig:mean_geopot_boxplot}
\end{figure}

Die räumliche Verteilung der gemittelten Werte des Luftdrucks ist pro Cluster unterschiedlich. In Cluster 1 befinden sich nordwestlich sehr hohe Werte während in Cluster 2 solch ähnlich hohe Werte des Luftdrucks im Süden zu finden sind. Zudem sind dort die hohen Werte über eine größere Fläche verteilt als in Cluster 1. In Cluster 3 ist hingegen das Gebiet mit hohen Luftdruckwerten östlich gelegen, in Cluster 4 und 6 südwestlich. Cluster 5 und 6  sind gekennzeichnet von tendenziell hohen Luftdruckwerten, ein ausgeprägtes Gebiet mit tiefen Luftdruckwerten ist in Cluster 5 nicht enthalten, und in Cluster 6 nur südöstlich. Im Gegensatz dazu befindet sich in Cluster 2 die größte Fläche mit niedrigen Luftdruckwerten, das nördlich gelegen ist. 
Insgesamt hat jedes Cluster eine eigene, charakteristische Verteilung der Luftdruckwerte.

Bei der räumlichen Verteilung der Werte des Geopotentials aufgeteilt nach Cluster ergibt sich hingegen ein anderes Bild im Vergleich zum Luftdruck. Tendenziell befinden sich in jedem Cluster  nördlich, bzw. nordwestlich niedrigere Werte ,  südlich, bzw. südwestlich sind höhere Werte. In den Clustern 1 bis 3 ist der Unterschied zwischen den niedrigsten und höchsten Werten des Geopotentials stärker ausgeprägt als in den Clustern 4 bis 6. 


## Vergleich der Clusterlösung mit den GWL

Der Mosaikplot in Abb.\ref{fig:mosaic_GWL} stellt die Aufteilung der GWLs auf die 6 Cluster dar. Zudem wird gezeigt, wie häufig eine bestimmte GWL in den betrachteten Zeitraum von 1971 bis 2000 vorkommt. Im Allgemeinen ist die Anzahl der auftretenden Wetterlagen heterogen. Hierbei ist die Großwetterlage WZ: Westlage zyklonal am häufigsten vertreten, dicht gefolgt von BM: Hochdruckbrücke Mitteleuropa. Selten vertreten sind die GWL NA: Nordlage antizyklonal, SZ: Südlage zyklonal und undefinierte Tage, also Tage, denen keine GWL zugeordnet werden kann. 
Jede GWL ist in allen Clustern vertreten; hierbei verteilen sich die GWLs gleichmäßig über die Cluster. Dennoch gibt es GWLs, die hauptsächlich in einem Cluster vorherrschen. WZ ist hauptsächlich in Cluster 2 vertreten SEZ hauptsächlich in Cluster 3 und WS hauptsächlich in Cluster 1.
Zudem wechseln in den meisten Fällen die GWLs und die Zugehörigkeit der Tage, die zu einem Cluster gehören, am selben Tag.

\begin{figure}[h]
\caption{Mosaikplot, der darstellt, mit welchem Anteil die GWLs auf die Cluster 1 bis 6 aufgeteilt sind}
\includegraphics{mosaicLegends}
\centering
\label{fig:cluster_GWL}
\end{figure}
