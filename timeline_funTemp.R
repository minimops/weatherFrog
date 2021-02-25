
data <- extrapolate(seq(2000, 2006))

pam <- pam(data, 5, metric = "manhattan")

newdat <- data.table(date = data[, date], cluster = pam$clustering)
pam$medoids


Cl.timeline(newdat, seperated = TRUE)


if(seperated) {
  runLengths <- rle(use[["ClustID"]])
  
  plots <- list()
  
  for (i in unique(use[["ClustID"]])) {
    
    length.runLengths.part <- runLengths$lengths[which(runLengths$values == i)]
    print(paste("distribution of runLengths", "Cluster:", i))
    print(table(length.runLengths.part))
    
    data <- data.table(table(length.runLengths.part))
    colnames(data) <- c("length", "count")
    print(data[length > 25, count])
    
    #print(Tl.weight.fun(data))
    
    p <- ggplot(as.data.frame(data), 
                aes(x= as.numeric(length), y = count)) +
      geom_bar(stat = "identity") +
      #labs(x = "Length", 
       #    title = paste("Occurence frequencies of lengths", 
        #                 paste(titleAdd, "Cluster:", i))) + 
      theme_bw()  +
      #scale_x_continuous(limits = c(0, 25), breaks = seq(1, 25, by = 2)) + 
      scale_y_continuous(limits = c(0, 35)) + 
      geom_text(x=23, y=20, label= "oof")
    #xlim(c(0, 25))
    
    plots[[i]] <- p
  }
  grid.arrange(grobs = plots)
}





data



mosaicplot(table(datClustGWL$gwl, datClustGWL$cluster), color = TRUE,
           ylab = "Cluster", xlab = "GWL", cex.axis = 0.6, las = 2,
           main = paste0(" Cluster - GWL"))
mosaic_examp <- ggplot(data = flights) +
  geom_mosaic(aes(x = product(do_you_recline, rude_to_recline), fill = do_you_recline)) +   
  labs(y="Do you recline?", x="Is it rude to recline?", title = "Mosaic Plot (2 variables)") 

data.gwl

ggplot(data = datClustGWL) +
  geom_mosaic(aes(x = product(cluster, gwl), fill = as.factor(cluster))) +
  theme_bw()+ 
  scale_fill_brewer(palette = "Set1") +
  labs(y = "Cluster", x = "GWL", title = "Mosaikplot von Cluster zu GWL") +
  scale_alpha_manual(values = 0.8) +
  theme(aspect.ratio = 3,
        axis.text.x = element_blank())
  
newdat
gwl

datClust <- datextr[, cluster := pam.manhat.30$clustering]
datClustGWL <- copy(gwl)[datClust, on = "date"]
datClustGWLf <- copy(datClustGWL) 
datClustGWLf$gwl <- as.factor(datClustGWLf$gwl)
levels(datClustGWLf$gwl)
datClustGWLf$cluster <- as.factor(datClustGWLf$cluster)
levels(datClustGWLf$gwl)



