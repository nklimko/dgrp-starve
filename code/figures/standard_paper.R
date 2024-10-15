

if(1){
  #regular
  library(dplyr)
  library(data.table)
  library(ggplot2)
  library(cowplot)
  library(viridis)
  library(ggplot2)
  library(cowplot)
  library(scales)
  library(tidyverse)
  library(ggcorrplot)
  library(melt)
  library(reshape2)
  library(knitr)
  library(gridExtra)
  library(argparse)
  library(ggpubr)
  #options
  options(bitmapType = 'cairo')
  options(error = function() traceback(3))

  #seed
  set.seed(123)

  #ggplot holder list
  gg <- vector(mode='list', length=6)
}

if(0){
  script='code/sr/analysis/standard_paper.R'
  femalePath='snake/data/sr/40_all/f/histData.Rds'
  malePath='snake/data/sr/40_all/m/histData.Rds'
  W=7.5
  H=5
}


#functions----
ggTidy <- function(data){
  for(i in 1:dim(data)[2]){
    name <- colnames(data)[i]
    temp <- cbind(rep(name, dim(data)[1]), data[,i, with=FALSE])
    if(i==1){
      hold <- temp
    } else{
      hold <- rbind(hold, temp, use.names=FALSE)
    }
  }
  colnames(hold) <- c("method", "cor")
  hold$method <- factor(hold$method, levels=unique(hold$method))
  return(hold)
}

finalLabel <- function(data){

  names(data) <- c('PCR', 'PLS', 'RR', 'LASSO', 'BayesC', 'VARBVS', 'MR.ASH', 'TBLUP', 'RF', 'NN')

  halfdata <- ggTidy(data)

  type <- c(rep('Dimension Reduction', 50), rep('Penalized', 50), rep('Bayesian', 75), rep('Mixed Model', 25), rep('Machine Learning', 50))
  typeRank <- c(rep(1, 50), rep(2, 50), rep(3, 75), rep(4, 25), rep(5, 50))

  select <- c(rep(0,75), rep(1,100), rep(0,25), rep(1,25), rep(0,25))

  tableStep <- cbind(halfdata, type, typeRank = as.factor(typeRank), varSelect = as.factor(select))

  final <- na.omit(tableStep)
  return(final)
}

ggMakePaper <- function(data, sex, yint, psize, custom.title, custom.Xlab, custom.Ylab, scaleStart, scaleEnd){
  plothole <- ggplot(data,aes(y=cor,x=method, fill=typeRank))+
    geom_hline(yintercept = yint)+
    labs(x=custom.Xlab,y=custom.Ylab, tag=sex, title=custom.title, fill='Method Type') +
    geom_violin(color = NA, width = 0.8) +
    geom_boxplot(color='#440154FF', width = 0.15, outlier.size = 1) +
    theme_minimal()+
    ylim(-0.5, 0.9)+
    theme(axis.text.x = element_text(angle = 45),
          text=element_text(size=10),
          legend.position="none",
          plot.tag = element_text(size=15),
          legend.text = element_text(size=8),
          legend.title = element_text(size=10)) +
    scale_fill_viridis(begin = scaleStart, end = scaleEnd, discrete=TRUE, option='turbo',
                       labels=c('Dimension Reduction', 'Penalized', 'Bayesian', 'Mixed Model', 'Machine Learning'))+#false discrete for number ranked layover, true normally
    stat_summary(fun=mean, color='#440154FF', geom='point',
                 shape=18, size=1, show.legend=TRUE)
  return(plothole)
}


#main call----
#plotMake <- function(dataPath, W, H, output){
plotMake <- function(femalePath, malePath, W, H, output){
  femaleData <- readRDS(femalePath)
  maleData <- readRDS(malePath)
  #dataStep <- readRDS(dataPath)

  #data <- finalLabel(dataStep)
  dataF <- finalLabel(femaleData)
  dataM <- finalLabel(maleData)

  #gg[[1]] <- ggMakePaper(data, NULL, 0, 1, NULL, 'Method', 'Prediction Accuracy', 0.1, 0.9)
  gg[[1]] <- ggMakePaper(dataF, 'A', 0, 1, NULL, 'Method', 'Prediction Accuracy', 0.1, 0.9)
  gg[[2]] <- ggMakePaper(dataM, 'B', 0, 1, NULL, 'Method', 'Prediction Accuracy', 0.1, 0.9)

  #standardFig1 <- arrangeGrob(gg[[1]], gg[[2]], nrow=1) #generates g

  final <- ggarrange(gg[[1]], gg[[2]], ncol=2, common.legend = 1, legend = "bottom")

  #ggsave(gg[[1]], path='plots/paper', filename = output, dpi = 300, device = 'pdf', units='in', width=W, height=H)
  #ggsave(gg[[2]], path='plots/paper', filename = output, device = 'pdf', units='in', width=W, height=H)
  ggsave(final,  path='output/figs', filename = output, device = 'pdf', dpi = 450, units='in', width=W, height=H)

}


#args----
parser <- ArgumentParser(description= 'snakemake transfer')


parser$add_argument('--femalePath')
parser$add_argument('--malePath')
parser$add_argument('--W', type="double")
parser$add_argument('--H', type="double")
parser$add_argument('--output')

snake <- parser$parse_args()
print(str(snake))

#call----
plotMake(snake$femalePath,
         snake$malePath,
         snake$W,
         snake$H,
         snake$output)

