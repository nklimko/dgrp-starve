#sparsegl is loading: 100 every 2 days puts completion at current pace(20 jbs, 16gb per, 5hr runtimeish) around early march: RISKY

#start rewriting bayesC Code to run funny business



#fixin to bindingIsActive(
  
  
 # bin bayesC analysis using correct metric 1.96 z score 95th percentile
  
  
    temp <- 'snake/data/go/40_all/sexf/partData.Rds'

  
  
  temp <- readRDS(temp)
  
  tableUseful <- data.table(index=1:dim(temp)[1], term=temp[,4], cor=as.numeric(temp[,5]))
  
  data <- tableUseful[order(-cor),]
  
  itsPeak <- data[1:(dim(data)[1]/20),]
  
  plot(x=data$index, y=data$cor)
  plot(x=itsPeak$index, y=itsPeak$cor)
  
  
  dim(data)[1]/20
  
  partMake2 <- function(data, sex, nullInt, upperCutoff, lowerCutoff, psize, custom.title, custom.Xlab, custom.Ylab){
  plothole <- ggplot(data, aes(x=term, y=cor, label=term))+
    geom_point(color=viridis(1, begin=0.5), size=psize)+
    geom_text(aes(label=ifelse(cor>upperCutoff, as.character(term),'')), hjust=0, size=2, angle=0)+
    geom_text(aes(label=ifelse(cor<lowerCutoff, as.character(term),'')), hjust=1, size=2, angle=90)+
    geom_hline(yintercept = nullInt) +
    theme_minimal() +
    labs(x=custom.Xlab, y=custom.Ylab, tag=sex, title=custom.title) +
    theme(text=element_text(size=10), plot.tag = element_text(size=15))
  return(plothole)
}

  myFactor <- 2
  finalFactor <- mean(data$cor) + myFactor * sd(data$cor)
  
  gg[[1]] <- partMake2(data, 'F', 0.31, finalFactor, 0.2, 1, 'Effect of GO Annotations in TBLUP models', 'GO Term', 'Prediction Accuracy')
  
  gg[[1]]
  
  finalFactor2 <- mean(data2$cor) + myFactor * sd(data2$cor)
  
  
  gg[[2]] <- partMake2(data2, 'F', 0.31, 0.345, 0.2, 1, 'Effect of GO Annotations in TBLUP models', 'GO Term', 'Prediction Accuracy')
  
  
  
  gg[[2]]
  
  dim(
  
  
  peakPlus <- itsPeak[(cor > finalFactor),]
  
  
  dim(itsPeak[(cor > finalFactor),])
  
  
  dim(data[(cor > finalFactor),])
  
  dim(data2[(cor > finalFactor2),])
  