
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



datClust <- datextr[, cluster := pam.manhat.30$clustering]
datClustGWL <- copy(gwl)[datClust, on = "date"]
datClustGWLf <- copy(datClustGWL) 
datClustGWLf$gwl <- as.factor(datClustGWLf$gwl)
levels(datClustGWLf$gwl)
datClustGWLf$cluster <- as.factor(datClustGWLf$cluster)
levels(datClustGWLf$gwl)
datClustGWLf$cluster <- forcats::fct_relevel(datClustGWLf$cluster, "1", "2", "3", "4", "5", "6")


ggplot(data = datClustGWLf) +
  geom_mosaic(aes(x = product(cluster, gwl), fill = as.factor(cluster)), offset = 0.015) +
  theme_bw()+ 
  scale_fill_brewer(palette = "Set1") +
  labs(y = "Cluster", x = "GWL", title = "Mosaikplot von Cluster zu GWL",
       legend = "Cluster") +
  guides(fill = guide_legend(title = "Cluster")) +
  
  

scale_alpha_manual(values = 0.8) 

  theme(aspect.ratio = 3,
        axis.text.x = element_blank())
  
newdat
gwl

?forcats::fct_relevel
Cl.timeline(copy(gwl30), "gwl")
ggsave("TryoutMosaik", path = "Documentation/plots/PAMfinal/", device = "jpeg",
       width = 5, height = 3)

Cl.timeline <- function(data, cluster = "cluster", titleAdd = "", seperated = FALSE) {
  assertDataTable(data)
  assertString(cluster)
  assertString(titleAdd)
  assertSubset(c("date"), names(data))
  assertLogical(seperated)
  
  #this is next level stupid,i cant figure out a different way to extract the
  #cluster column while leaving it a variable
  use <- data.table(ClustID = copy(data)[[as.character(cluster)]],
                    date = copy(data)[["date"]])
  setorder(use, date)
  print("Table of Cluster frequencies:")
  print(table(use$ClustID))
  if(seperated){
    runLengths <- rle(use[["ClustID"]])
    plots <- list()
    for (i in unique(use[["ClustID"]])) {
      
      length.runLengths.part <- runLengths$lengths[which(runLengths$values == i)]
      print(paste("distribution of runLengths", "Cluster:", i))
      print(table(length.runLengths.part))
      
      data <- data.table(table(length.runLengths.part))
      colnames(data) <- c("length", "count")
      
      # print(Tl.weight.fun(data))
      
      
      #data2 <- data[order(as.numeric(length))]
      dataOver30 <- copy(data)[as.numeric(length) > 30]
      cutoffs <- sum(as.numeric(dataOver30$length) * dataOver30$count)
      colVector <- RColorBrewer::brewer.pal(8, "Set1")
      
      p <- ggplot(as.data.frame(data), 
                  aes(x= as.numeric(length), y = count)) +
        geom_col(fill = colVector[i], col = "black") +
        labs(x = "Länge", 
             title = paste("Cluster ", i),
             y = "Anzahl") +
        ylim(0, 150) +
        scale_x_continuous(limits = c(0, 30), breaks = seq(1, 30, by = 2)) +
        theme_bw() +
        geom_text(x=23, y=20, label= paste0("Abgeschnitten: ", cutoffs),
                  size = 4)
      plots[[i]] <- p
    }
    grid.arrange(grobs = plots)
  } else{
    runLengths <- rle(use[["ClustID"]])
    
    print("distribution of runLengths:")
    print(table(runLengths$lengths))
    
    data <- data.table(table(runLengths$lengths))
    colnames(data) <- c("length", "count")
    data[, ":=" (length = as.numeric(length))]
    
    print("timeline Value:")
    print(Tl.weight.fun(data))
    
    if (cluster == "cluster") {
      mainAdd <- "Cluster"
    }
    else {
      mainAdd <- "GWL"
    }
    
    ggplot(as.data.frame(data), 
           aes(x= as.numeric(length), y = count)) +
      geom_col(col = "black", fill = "gray77") +
      labs(x = "Länge", 
           title = paste("Länge der aufeinanderfolgenden, gleichen GWL"),
           y = "Anzahl") +
      ylim(0, 600) +
      
      scale_x_continuous(breaks = c(seq(0, 23))) +
      
      theme_bw()
    
  }
}
Cl.timeline(copy(gwl30), "gwl")

