
library(ggplot2)
library(data.table)
library(viridis)
library(cowplot)

#options
options(bitmapType = 'cairo')
options(error = function() traceback(3))

#seed
set.seed(123)

#ggplot holder list
gg <- vector(mode='list', length=12)


#preformatting bayesC----
bayesF <- readRDS('snake/data/go/40_all/sexf/bayesFREEfinal.Rds')
bayesF <- data.table(term=bayesF[,3], cor = as.numeric(unlist(bayesF[,5])))
names(bayesF) <- c('term', 'cor')
saveRDS(bayesF, 'snake/data/go/40_all/sexf/bayesFREEformatted.Rds')

bayesM <- readRDS('snake/data/go/40_all/sexm/bayesFREEfinal.Rds')
bayesM <- data.table(term=bayesM[,3], cor = as.numeric(unlist(bayesM[,5])))
names(bayesM) <- c('term', 'cor')
saveRDS(bayesM, 'snake/data/go/40_all/sexm/bayesFREEformatted.Rds')

#plotFunctions----
maxGridSD <- function(dataPath, ordered, sex, psize, custom.title, custom.Xlab, custom.Ylab, yint, sdFactor, color){
  
  core <- readRDS(dataPath)
  index <- 1:dim(core)[1]
  
  if(ordered){ core <- core[order(-cor),] }
  
  data <- data.table(index, core)
  
  ycutoff <- mean(data[,cor]) + sdFactor * sd(data[,cor])
  
  print(paste0('ycutoff: ', ycutoff))
  
  plothole <- ggplot(data, aes(x=index, y=cor, label=term))+
    geom_point(color=viridis(1, begin=0.5), size=psize)+
    theme_minimal() +
    geom_hline(yintercept = yint)+
    geom_hline(yintercept = ycutoff)+
    labs(x=custom.Xlab, y=custom.Ylab, tag=sex, title=custom.title) +
    theme(text=element_text(size=10), plot.tag = element_text(size=15))
  return(plothole)
}

maxGrid <- function(dataPath, ordered, sex, psize, custom.title, custom.Xlab, custom.Ylab, yint, color){
  
  core <- readRDS(dataPath)
  index <- 1:dim(core)[1]
  
  if(ordered){ core <- core[order(-cor),] }
  
  data <- data.table(index, core)
  
  plothole <- ggplot(data, aes(x=index, y=cor, label=term))+
    geom_point(color=viridis(1, begin=color), size=psize)+
    theme_minimal() +
    geom_hline(yintercept = yint)+
    labs(x=custom.Xlab, y=custom.Ylab, tag=sex, title=custom.title) +
    theme(text=element_text(size=10), plot.tag = element_text(size=15))
  return(plothole)
}



#plotCalls----
dataPath <- 'snake/data/go/40_all/sexf/bayesFREEformatted.Rds'

maxGridSD(dataPath, 0, 'F', 1, 'Top Female Results: BayesC', 'Rank', 'Correlation', bayesYintF, 1.96, 0.5)

blupPathF <- 'snake/data/go/40_all/f/blup.Rds'
blupPathM <- 'snake/data/go/40_all/m/blup.Rds'
bayesPathF <- 'snake/data/go/40_all/sexf/bayesFREEformatted.Rds'
bayesPathM <- 'snake/data/go/40_all/sexm/bayesFREEformatted.Rds'

allYintF <- readRDS('snake/data/sr/40_all/f/srQuarterFinal.Rds')
allYintM <- readRDS('snake/data/sr/40_all/m/srQuarterFinal.Rds')

blupYintF <- as.numeric(allYintF[8,2])
blupYintM <- as.numeric(allYintM[8,2])
bayesYintF <-as.numeric(allYintF[5,2])
bayesYintM <-as.numeric(allYintM[5,2])

# yintDataFbayes <- readRDS('snake/data/sr/33_metric/go/sexf/rmax0.8/rgo0/term1/rowData.Rds')
# yintDataMbayes <- readRDS('snake/data/sr/33_metric/go/sexm/rmax0.8/rgo0/term1/rowData.Rds')
# 
# bayesYintF <- as.numeric(yintDataFbayes[5])
# bayesYintM <- as.numeric(yintDataMbayes[5])


maxGrid <- function(dataPath, ordered, sex, psize, custom.title, custom.Xlab, custom.Ylab, yint, cutoff, color){
  core <- readRDS(dataPath)
  len <- dim(core)[1]
  
  index <- 1:len
  cutdex <- as.integer(len*cutoff)
  
  orderBase <- core[order(-cor),]
  
  trueCutoff <- as.numeric(orderBase[cutdex, 2, with=FALSE])
  
  if(ordered){ core <- orderBase }
  
  data <- data.table(index, core)
  plothole <- ggplot(data, aes(x=index, y=cor, label=term))+
    geom_point(color=viridis(1, begin=color, option='turbo'), size=psize)+
    theme_minimal() +
    geom_hline(yintercept = yint)+
    geom_hline(yintercept = trueCutoff, linetype="dashed")+
    labs(x=custom.Xlab, y=custom.Ylab, tag=sex, title=custom.title) +
    theme(text=element_text(size=10), plot.tag = element_text(size=15))
  return(plothole)
}



#maxGrid(bayesPathF, 0, 'F', 0.7, 'GO-BayesC Prediction Accuracy of GO Annotations', 'GO Term', 'Prediction Accuracy', bayesYintF, 0.05, color=0.9)

gg[[1]] <- maxGrid(bayesPathF, 0, 'A', 0.7, 'GO-BayesC Prediction Accuracy of GO Annotations in Females', 'GO Term', 'Prediction Accuracy', bayesYintF, 0.05, color=0.9)
gg[[2]] <- maxGrid(bayesPathM, 0, 'B', 0.7, 'GO-BayesC Prediction Accuracy of GO Annotations in Males', 'GO Term', 'Prediction Accuracy', bayesYintM, 0.05, color=0.1)
gg[[3]] <- maxGrid(blupPathF, 0, 'A', 0.7, 'GO-TBLUP Prediction Accuracy of GO Annotations in Females', 'GO Term', 'Prediction Accuracy', blupYintF, 0.05, color=0.9)
gg[[4]] <- maxGrid(blupPathM, 0, 'B', 0.7, 'GO-TBLUP Prediction Accuracy of GO Annotations in Males', 'GO Term', 'Prediction Accuracy', blupYintM, 0.05, color=0.1)

plot_grid(gg[[1]], gg[[3]], ncol=2)
plot_grid(gg[[2]], gg[[4]], ncol=2)

ggsave('plots/GO_2x2/bayesF.png', plot=gg[[1]])
ggsave('plots/GO_2x2/bayesM.png', plot=gg[[2]])
ggsave('plots/GO_2x2/blupF.png', plot=gg[[3]])
ggsave('plots/GO_2x2/blupM.png', plot=gg[[4]])

plot_grid(gg[[5]],gg[[6]], ncol=2)


plot_grid(gg[[7]],gg[[8]], ncol=2)

#gene enrichment selection----
#needs to take top 5% of terms and save as rds for linux extraction

blupPathF <- 'snake/data/go/40_all/f/blup.Rds'
blupPathM <- 'snake/data/go/40_all/m/blup.Rds'
bayesPathF <- 'snake/data/go/40_all/sexf/bayesFREEformatted.Rds'
bayesPathM <- 'snake/data/go/40_all/sexm/bayesFREEformatted.Rds'

topSelect <- function(dataPath, outPath){
  
  core <- readRDS(dataPath)
  data <- core[order(-cor), term] 
  cutoff <- as.integer(length(data) * 0.05)
  final <- data[1:cutoff]
  subbed <- sub(pattern='.', replacement=':', x=final, fixed=TRUE) # replace . with : for all terms
  
  write(subbed, file=outPath)
}

topSelect(blupPathF,  'code/go/enrichment/f/blup/topHits.txt')
topSelect(blupPathM,  'code/go/enrichment/m/blup/topHits.txt')
topSelect(bayesPathF, 'code/go/enrichment/f/bayes/topHits.txt')
topSelect(bayesPathM, 'code/go/enrichment/m/bayes/topHits.txt')

###2024/04/04
GO-TBLUP complete for both sexes, data included in current figure
All figures are complete except for the Gene enrichment analysis.
I will sort out formatting for tables after I update the results findings as they have changed for three sections.
For this, I will manually go through and color code cells to match the Excel duplicate matching.
