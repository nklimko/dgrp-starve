blupPathF <- 'snake/data/go/40_all/f/blup.Rds'
blupPathM <- 'snake/data/go/40_all/m/blup.Rds'
bayesPathF <- 'snake/data/go/40_all/sexf/bayesFREEformatted.Rds'
bayesPathM <- 'snake/data/go/40_all/sexm/bayesFREEformatted.Rds'




bayesF <- readRDS(bayesPathF)
bayesM <- readRDS(bayesPathM)
blupF <- readRDS(blupPathF)
blupM <- readRDS(blupPathM)

fAll <- bayesF[blupF, on=.(term)]
names(fAll) <- c('Term', 'Bayes', 'TBLUP')

mAll <- bayesM[blupM, on=.(term)]
names(mAll) <- c('Term', 'Bayes', 'TBLUP')

tempM1 <- corCheckMuteRevamp(mAll, 10, 2580, 10)

temp <- corCheckMuteRevamp(fAll, 10, 2620, 10)

corCheckMuteRevamp <- function(data, start, stop, increment){
  
  rindex <- seq(start, stop, increment)
  bayesOrder <- data[order(-Bayes), ]
  blupOrder <- data[order(-TBLUP), ]
  
  hold <- c(0,0,0,0)
  names(hold) <- c('Rank', 'Top Bayes', 'Top TBLUP', 'Composite')
  
  
  for(i in rindex){
    subSpace <- 1:i
    
    bayesSub <- bayesOrder[subSpace,]
    bayesCor <- cor(bayesSub[,2], bayesSub[,3])
    
    blupSub <- blupOrder[subSpace,]
    blupCor <- cor(blupSub[,2], blupSub[,3])
    
    composite <- (bayesCor + blupCor)/2
    
    corce <- c(i, bayesCor, blupCor, composite)
    
    hold <- rbind(hold, corce)
  }
  
  final <- hold[-1,]
  return(final)
}


tidy2 <- function(data){
  len <- dim(data)[1]  
  raw <- unlist(data)
  
  method <- rep(raw[1:len], 3)
  cor <- raw[seq(len+1,length(raw))]
  
  index <- unlist(c(rep('a', len), rep('b', len), rep('c', len)))
  
  final <- data.table(method, index, cor)
  return(final)  
}

tempM2 <- tidy2(tempM1)

temp2 <- tidy2(temp)

psize <- 0.8

ggplot(temp2, aes(x=method, y=cor, color=index)) +
  geom_point(size=psize) +
  scale_color_viridis(discrete = TRUE, labels=c('Top Bayes : Blup','Top Blup : Bayes','Composite')) +
  theme_minimal()

ggplot(tempM2, aes(x=method, y=cor, color=index)) +
  geom_point(size=psize) +
  scale_color_viridis(discrete = TRUE, labels=c('Top Bayes : Blup','Top Blup : Bayes','Composite')) +
  theme_minimal()




sliceCheck <- function(data, start, stop, increment, slice){
  
  rindex <- seq(start, stop, increment)
  bayesOrder <- data[order(-Bayes), ]
  blupOrder <- data[order(-TBLUP), ]
  
  hold <- c(0,0,0,0)
  names(hold) <- c('Rank', 'Top Bayes', 'Top TBLUP', 'Composite')
  
  
  for(i in rindex){
    subSpace <- i:(i+slice)
    
    bayesSub <- bayesOrder[subSpace,]
    bayesCor <- cor(bayesSub[,2], bayesSub[,3])
    
    blupSub <- blupOrder[subSpace,]
    blupCor <- cor(blupSub[,2], blupSub[,3])
    
    composite <- (bayesCor + blupCor)/2
    
    corce <- c(i, bayesCor, blupCor, composite)
    
    hold <- rbind(hold, corce)
  }
  
  final <- hold[-1,]
  return(final)
}



mSlice <- sliceCheck(mAll, 0, 2430, 1, 150)

fSlice <- sliceCheck(fAll, 0, 2478, 1, 150)


mSlice100 <- sliceCheck(mAll, 0, 2425, 25, 100)


sliceM100 <- tidy2(mSlice100)
sliceM <- tidy2(mSlice)
sliceF <- tidy2(fSlice)


ggplot(sliceM, aes(x=method, y=cor, color=index)) +
  geom_point(size=psize) +
  scale_color_viridis(discrete = TRUE, labels=c('Top Bayes : Blup','Top Blup : Bayes','Composite')) +
  theme_minimal()

ggplot(sliceM100, aes(x=method, y=cor, color=index)) +
  geom_point(size=psize) +
  scale_color_viridis(discrete = TRUE, labels=c('Top Bayes : Blup','Top Blup : Bayes','Composite')) +
  theme_minimal()



ggMakeSlice <- function(data, sex, yint, psize, custom.title, custom.Xlab, custom.Ylab, scaleStart, scaleEnd){
  plothole <- ggplot(data,aes(x=method, y=cor, color=index))+
    geom_hline(yintercept = yint)+
    labs(x=custom.Xlab,y=custom.Ylab, tag=sex, title=custom.title, color='Comparison') +
    geom_point(size=psize) +
    theme_minimal()+
    theme(text=element_text(size=15),
          plot.tag = element_text(size=10)) +
    geom_point(size=psize) +
    scale_color_viridis(discrete = TRUE, labels=c('Top GO-BayesC : GO-TBLUP','Top GO-TBLUP : GO-BayesC','Composite Score')) 
  return(plothole)
}

ggMakeSlice(sliceF, 'A', 0, 0.5, 'Correlation between Ranking Subsets by Method in Females', 'Slice', 'Correlation', 0.1, 0.9)

ggMakeSlice(sliceM, 'B', 0, 0.5, 'Correlation between Ranking Subsets by Method in Males', 'Slice', 'Correlation', 0.1, 0.9)

ggplot(mAll, aes(x=Bayes, y=TBLUP))+
  geom_point(size=psize)+
  geom_smooth(method=lm, color='blue') + 
  xlim(0.25, 0.55)+
  ylim(0.25, 0.55)

ggplot(fAll, aes(x=Bayes, y=TBLUP))+
  geom_point(size=psize) +
  geom_smooth(method=lm, color='red')+
  xlim(0.15, 0.5)+
  ylim(0.15, 0.5)

fReg <- lm(formula = Bayes~TBLUP, data=fAll)
lm(formula = Bayes~TBLUP, data=mAll)

cor(mAll$Bayes, mAll$TBLUP) ^ 2

cor(fAll$Bayes, fAll$TBLUP) ^ 2


data <- mAll


ggMakeCor <- function(data, sex, psize, custom.title, custom.Xlab, custom.Ylab, limStart, limStop, color){
  plothole <- ggplot(data, aes(x=Bayes, y=TBLUP))+
    labs(x=custom.Xlab,y=custom.Ylab, tag=sex, title=custom.title) +
    geom_point(color=viridis(1, begin=color, option='turbo'), size=psize) +
    geom_smooth(method=lm, color='black')+
    xlim(limStart, limStop)+
    ylim(limStart, limStop)+
    theme_minimal()+
    theme(text=element_text(size=10),
          plot.tag = element_text(size=10))
  return(plothole)
}

gg[[1]]<- ggMakeCor(fAll, 'A', 0.25, 'Scatter Plot of GO Methods in Females', 'GO-BayesC Prediction Accuracy', 'GO-TBLUP Prediction Accuracy', 0.15, 0.5, 0.9)
gg[[2]] <- ggMakeCor(mAll, 'B', 0.25, 'Scatter Plot of GO Methods in Males', 'GO-BayesC Prediction Accuracy', 'GO-TBLUP Prediction Accuracy', 0.25, 0.55, 0.1)

plot_grid(gg[[1]], gg[[2]], ncol=2)


"#C42503FF"

library(viridisLite)
viridis(1, begin=0.9, option='turbo')




# Example data
data <- data.frame(
  x = 1:10,
  y = rnorm(10)
)

# Set a specific fill color for the points
point_fill_color <- "black"
  temp <- 0.9
  # Create the plot
  ggplot(data, aes(x = x, y = y)) +
    geom_point(fill = point_fill_color, color = viridis(1, begin=temp, option='turbo')) + # Set fill color
    theme_minimal()
  