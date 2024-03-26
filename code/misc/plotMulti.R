
#SETUP
if(TRUE){
  #regular
  library(dplyr)
  library(data.table)
  library(ggplot2)
  library(cowplot)
  library(qqman)
  library(doParallel)
  library(viridis)
  library(scales)
  library(tidyverse)
  
  #options
  options(bitmapType = "cairo")
  options(error = function() traceback(3))
  
  #seed
  set.seed(123)
  
  #loop count and data limit
  iter <- 50
  #ggplot holder list
  gg <- vector(mode='list', length=12)
  setwd('/data2/morgante_lab/nklimko/rep/dgrp-starve/')
  
  
}


#FEMALE
if(TRUE){
  
  multigblupData <- readRDS('snake/data/20_cor/multigblup_f_starvation.Rds')
  temp <- unlist(multigblupData)
  multigblup <- temp[seq(1, length(temp), by=4)]
  
  multibayesCData <- readRDS('snake/data/20_cor/multibayesC_f_starvation.Rds')
  temp <- unlist(multibayesCData)
  multibayesC <- temp[seq(1, length(temp), by=4)]
  
  mr.mashData <- readRDS('snake/data/20_cor/mr.mash_f_starvation.Rds')
  temp <- unlist(mr.mashData)
  mr.mash <- temp[seq(1, length(temp), by=4)]
  
  topmbc <- unlist(readRDS('snake/data/20_cor/multibayesC_f_sr.top3.Rds'))
  top_multibayesC <- topmbc[seq(1, length(topmbc), by=16)]
  
  topblup <- unlist(readRDS('snake/data/20_cor/multiblup_f_sr.top3.Rds'))
  top_multiblup <- topblup[seq(1, length(topblup), by=16)]
  
  temp <- c(multigblup, multibayesC, mr.mash, top_multiblup, top_multibayesC)
  
  label <- c(rep("multigblup", iter), rep("multibayesC", iter), rep("mr.mash", iter), rep('top_multiblup', iter), rep("top_multibayesC", iter))
  
  data <- data.table(cor=as.numeric(temp), method=label)
  
  gg[[1]] <- ggplot(data, aes(x=method, y=cor, fill=method)) +
    geom_violin(color = NA, width = 0.65) +
    geom_boxplot(color='#440154FF', width = 0.15) +
    theme_minimal() +
    stat_summary(fun=mean, color='#440154FF', geom='point', 
                 shape=18, size=3, show.legend=FALSE) +
    labs(x=NULL,y='Correlation between True and Predicted Phenotype',tag='F') +
    theme(legend.position='none',
          axis.text.x = element_text(angle = -45, size=10),
          text=element_text(size=10),
          plot.tag = element_text(size=15)) +
    scale_fill_viridis(begin = 0.4, end=0.9,discrete=TRUE)
  
  final <- data.table(method=c('multibayesC', 'top_multibayesC', 'multigblup', 'top_multigblup'))  
  
  final <- cbind(final, f=c(mean(multibayesC), mean(top_multibayesC), mean(multigblup), mean(top_multiblup)))
  
  
  }

#MALE
if(TRUE){
  multigblupData <- readRDS('snake/data/20_cor/multigblup_m_starvation.Rds')
  temp <- unlist(multigblupData)
  multigblup <- temp[seq(1, length(temp), by=4)]
  
  multibayesCData <- readRDS('snake/data/20_cor/multibayesC_m_starvation.Rds')
  temp <- unlist(multibayesCData)
  multibayesC <- temp[seq(1, length(temp), by=4)]
  
  mr.mashData <- readRDS('snake/data/20_cor/mr.mash_m_starvation.Rds')
  temp <- unlist(mr.mashData)
  mr.mash <- temp[seq(1, length(temp), by=4)]
  
  topmbc <- unlist(readRDS('snake/data/20_cor/multibayesC_m_sr.top3.Rds'))
  top_multibayesC <- topmbc[seq(1, length(topmbc), by=16)]
  
  topblup <- unlist(readRDS('snake/data/20_cor/multiblup_m_sr.top3.Rds'))
  top_multiblup <- topblup[seq(1, length(topblup), by=16)]
  
  temp <- c(multigblup, multibayesC, mr.mash, top_multiblup, top_multibayesC)
  
  label <- c(rep("multigblup", iter), rep("multibayesC", iter), rep("mr.mash", iter), rep('top_multiblup', iter), rep("top_multibayesC", iter))
  
  data <- data.table(cor=as.numeric(temp), method=label)
  
  gg[[2]] <- ggplot(data, aes(x=method, y=cor, fill=method)) +
    geom_violin(color = NA, width = 0.65) +
    geom_boxplot(color='#440154FF', width = 0.15) +
    theme_minimal() +
    stat_summary(fun=mean, color='#440154FF', geom='point', 
                 shape=18, size=3, show.legend=FALSE) +
    labs(x=NULL,y='Correlation between True and Predicted Phenotype',tag='M') +
    theme(legend.position='none',
          axis.text.x = element_text(angle = -45, size=10),
          text=element_text(size=10),
          plot.tag = element_text(size=15)) +
    scale_fill_viridis(begin = 0.4, end=0.9,discrete=TRUE)
  
  final <- cbind(final, m=c(mean(multibayesC), mean(top_multibayesC), mean(multigblup), mean(top_multiblup)))
  
}

setwd('/data2/morgante_lab/nklimko/rep/dgrp-starve/snake/data')

saveRDS(final, 'fm_vs_top.Rds')

finalCor <- readRDS('fm_vs_top.Rds')
plot_grid(gg[[1]],gg[[2]], ncol=2)

finalCor
