library(data.table)

life <- list.files(path="snake/data/sr/23_paropt/m/nn", pattern = ".Rds", recursive = TRUE, full.names=TRUE)
print(life)

bapples <- c(4,5,9,10,14,15)
bapples <- c(4,5,9,13,14)

life <- life[-bapples]

fruit <- sapply(life, readRDS)

cors <- fruit[1,]
print(cors)

corNames <- names(cors)

corVals <- as.numeric(unlist(cors))

corTable <- cbind(corNames, corVals)

samp <- '123456789'

temp <- corNames[1]
corLabels <- substring(corNames, 36, 39)

corTable <- data.table(Hidden=corLabels, Trial=as.factor(rep(c(1,2,3), 5)), Cor=as.numeric(corVals))
corTable

data <- corTable

ggMakeNN<- function(data, sex, custom.title, custom.Xlab, custom.Ylab){
  plothole <- ggplot(data,aes(y=Cor,x=Hidden, color=Trial))+
    labs(x=custom.Xlab,y=custom.Ylab, tag=sex, title=custom.title, fill='Method Type') +
    geom_point()+
    theme_minimal()
  return(plothole)
}

ggMakeNN(corTable, 'M', 'NN_test', 'Hidden Neurons', 'Correlation Coefficient')

{
  #plothole <- 
  ggplot(data,aes(y=Cor,x=Hidden, fill=Trial))+
    labs(x=custom.Xlab,y=custom.Ylab, tag=sex, title=custom.title, fill='Method Type') +
    geom_point()+
    theme_minimal()
  return(plothole)
}



saveRDS(corTable, 'data/nnCors.Rds')
