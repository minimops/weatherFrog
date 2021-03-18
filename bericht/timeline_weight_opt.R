Um die Timeline einer Clusterlösung quantifizierbar und vergleichbar zu gestalten, wird über die Timeline eine Gewichtungsfunktion gelegt (Abbildung \ref{fig:timeline_weights}). Der *Timeline-Score* (TLS) ergibt sich dann aus der Summe der Gewichteten Längen der CWL. Um den Wertebereich auf [0,1] zu beschränken, entspricht der *Timeline-Score* der Clusterlösung dem Verhältnis zur Optimalen Timeline Verteilung.

```{r timeline_weights, echo = FALSE, fig.cap="\\label{fig:timeline_weights}Gewichte zum Quantifizieren der Timeline", fig.align="center", out.width="80%"}
include_graphics("assets/timeline_weights.png")
```

$$
  TLS = \frac{\sum_{i = 1}^{40}{w_i  x_i}}{\sum_{j = 1}^{n}{x_i w_{max}}}
$$ $$
  wobei: X = (x_1, ..., x_n) = \text{Anzahl Tage der jeweiligen CWL-Länge}
$$ $$
  W = (w_1, ..., w_{40}) = \text{Gewichtungsvektor}; w_{max} = max(W)
$$
  
  **ODER:**