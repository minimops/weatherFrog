
data <- extrapolate(seq(2000, 2006))
pam <- pam(data, 5, metric = "manhattan")

newdat <- data.table(date = data[, date], cluster = pam$clustering)



Cl.timeline(newdat, seperated = TRUE)


if(seperated){
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
      labs(x = "Length", 
           title = paste("Occurence frequencies of lengths", 
                         paste(titleAdd, "Cluster:", i))) + 
      theme_bw()  +
      #scale_x_continuous(limits = c(0, 25), breaks = seq(1, 25, by = 2)) + 
      scale_y_continuous(limits = c(0, 30)) + 
      geom_text(x=23, y=20, label= "oof")
    #xlim(c(0, 25))
    
    plots[[i]] <- p
  }
  grid.arrange(grobs = plots)