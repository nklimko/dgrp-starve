
#Standard
if(1){
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
  
  if(1){
    #regular
    library(dplyr)
    library(data.table)
    library(ggplot2)
    library(cowplot)
    library(doParallel)
    library(viridis)
    library(ggplot2)
    library(cowplot)
    library(scales)
    library(tidyverse)
    library(ggcorrplot)
    library(melt)
    library(reshape2)
    library(knitr)
    
    #options
    options(bitmapType = 'cairo')
    options(error = function() traceback(3))
    
    #seed
    set.seed(123)
    
    #ggplot holder list
    gg <- vector(mode='list', length=6)
  }
  
  
  
  femaleData <- readRDS('snake/data/sr/40_all/f/cor/all.Rds')
  maleData <- readRDS('snake/data/sr/40_all/m/cor/all.Rds')
  
  varf <- list.files('snake/data/sr/23_paropt/sexf/varbvs', full.names = TRUE)
  f.varbvs <- sapply(varf, readRDS)
  varFinalF <- unlist(f.varbvs[seq(1,99,2)])
  
  marf <- list.files('snake/data/sr/23_paropt/sexm/varbvs', full.names = TRUE)
  m.varbvs <- sapply(marf, readRDS)
  varFinalM <- unlist(m.varbvs[seq(1,99,2)])
  
  maleFinal <- cbind(maleData, varFinalM)
  femaleFinal <- cbind(femaleData, varFinalF)
  
  #Univariate Male
  uniData <- maleFinal
  
  names(uniData) <- c("RF", "BayesC", "TBLUP", "PCR", "NN", "MR.ASH", "RR", "LASSO", "PLS", "VARBVS")
  colOrder <- c(4,9,7,8,2,10,6,3,1,5)
  setcolorder(uniData, colOrder)
  
  halfdata <- ggTidy(uniData)
  
  type <- c(rep('Transformation', 100), rep('Penalized', 100), rep('Bayesian', 150), rep('Mixed Model', 50), rep('Machine Learning', 100))
  typeRank <- c(rep(1, 100), rep(2, 100), rep(3, 150), rep(4, 50), rep(5, 100))
  
  dataStep <- cbind(halfdata, type, typeRank = as.factor(typeRank))
  dataM <- na.omit(dataStep)
  
  #Univariate Female
  uniData <- femaleFinal
  
  names(uniData) <- c("RF", "BayesC", "TBLUP", "PCR", "NN", "MR.ASH", "RR", "LASSO", "PLS", "VARBVS")
  colOrder <- c(4,9,7,8,2,10,6,3,1,5)
  setcolorder(uniData, colOrder)
  
  
  halfdata <- ggTidy(uniData)
  
  type <- c(rep('Transformation', 100), rep('Penalized', 100), rep('Bayesian', 150), rep('Mixed Model', 50), rep('Machine Learning', 100))
  typeRank <- c(rep(1, 100), rep(2, 100), rep(3, 150), rep(4, 50), rep(5, 100))
  
  dataStep <- cbind(halfdata, type, typeRank = as.factor(typeRank))
  dataF <- na.omit(dataStep)
  
  
  
  saveRDS(dataF, file='snake/data/sr/40_all/dataF.Rds')
  saveRDS(dataM, file='snake/data/sr/40_all/dataM.Rds')
  
  #CHECKPOINT
  
  dataF <- readRDS('snake/data/sr/40_all/dataF.Rds')
  dataM <- readRDS('snake/data/sr/40_all/dataM.Rds')
  
  #plotmaker function
  ggMake2 <- function(data, sex, yint, psize, custom.title, custom.Xlab, custom.Ylab, scaleStart, scaleEnd){
    plothole <- ggplot(data,aes(y=cor,x=method, fill=typeRank))+
      geom_hline(yintercept = yint)+
      labs(x=custom.Xlab,y=custom.Ylab, tag=sex, title=custom.title, fill='Method Type') +
      geom_violin(color = NA, width = 0.65) +
      geom_boxplot(color='#440154FF', width = 0.15) +
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 90),
            text=element_text(size=25),
            plot.tag = element_text(size=20)) +
      scale_fill_viridis(begin = scaleStart, end = scaleEnd, discrete=TRUE, option='turbo',
                         labels=c('Transformation', 'Penalized', 'Bayesian', 'Mixed Model', 'Machine Learning'))+#false discrete for number ranked layover, true normally
      stat_summary(fun=mean, color='#440154FF', geom='point', 
                   shape=18, size=3, show.legend=FALSE)
    return(plothole)
  }
  
  gg[[1]] <- ggMake2(dataF, 'F', 0, 1, 'Prediction Accuracy by Method in Females', 'Method', 'Prediction Accuracy', 0.1, 0.9)
  gg[[1]]
  
  gg[[2]] <- ggMake2(dataM, 'M', 0, 1, 'Prediction Accuracy by Method in Males', 'Method', 'Prediction Accuracy', 0.1, 0.9)
  gg[[2]]
  
  #consider marking types with 1/2/3/4/5 and then labelling in ggplot2
  
  plot_grid(gg[[1]],gg[[2]], ncol=2)
  
  stanFactor <- 1
  stanH <- 10 * stanFactor
  stanW <- 10 * stanFactor
  
  ggsave("funi.png", plot=gg[[1]], height = stanH, width = stanW, units = 'in')
  ggsave("muni.png", plot=gg[[2]], height = stanH, width = stanW, units = 'in')
}



#------------------------------------
#GO Terms
if(1){
  
  partTidy <- function(dataCols){
    newIndex <- seq(1, dim(dataCols)[1])
    corder <- dataCols[order(term),3]
    final <- data.table(term=newIndex, cor=corder)
    names(final) <- c('term', 'cor')
    return(final)
  }
  
  partMake2 <- function(data, sex, yint, psize, colorNum, custom.title, custom.Xlab, custom.Ylab){
    plothole <- ggplot(data,aes(y=cor,x=term))+
      geom_point(color=viridis(1, begin=colorNum, option = 'turbo'))+
      geom_hline(yintercept = yint) +
      theme_minimal() +
      labs(x=custom.Xlab,y=custom.Ylab, tag=sex, title=custom.title) +
      theme(text=element_text(size=20),
            axis.text.x=element_blank(),
            plot.tag = element_text(size=18))
    return(plothole)
  }
  
  load('snake/data/go/50_tables/enrichment.Rdata')
  
  yintDataF <- readRDS('snake/data/sr/33_metric/go/sexf/rmax0.8/rgo0/term1/rowData.Rds')
  yintDataM <- readRDS('snake/data/sr/33_metric/go/sexm/rmax0.8/rgo0/term1/rowData.Rds')
  yintF <- as.numeric(yintDataF[5])
  yintM <- as.numeric(yintDataM[5])
  
  bayesFdata <- partTidy(bayesF)
  bayesMdata <- partTidy(bayesM)
  blupFdata <- partTidy(blupF)
  blupMdata <- partTidy(blupM)
  
  gg[[3]] <- partMake2(bayesFdata, 'F', yintF, 1, 0.85, 'GO-BayesC Prediction Accuracy of GO Annotations', 'GO Term', 'Prediction Accuracy')
  gg[[4]] <- partMake2(bayesMdata, 'M', yintM, 1, 0.15, 'GO-BayesC Prediction Accuracy of GO Annotations', 'GO Term', 'Prediction Accuracy')
  gg[[5]] <- partMake2(blupFdata, 'F', yintF, 1, 0.9, 'GO-TBLUP Prediction Accuracy of GO Annotations', 'GO Term', 'Prediction Accuracy')
  gg[[6]] <- partMake2(blupMdata, 'M', yintM, 1, 0.1, 'GO-TBLUP Prediction Accuracy of GO Annotations', 'GO Term', 'Prediction Accuracy')
  
  plot_grid(gg[[3]], gg[[4]], gg[[5]], gg[[6]], ncol=2)
  
  goFactor <- 1
  goH <- 5.5 * goFactor
  goW <- 11 * goFactor
  ggsave("bayesF.png", plot=gg[[3]], height = goH, width = goW, units = 'in')
  ggsave("bayesM.png", plot=gg[[4]], height = goH, width = goW, units = 'in')
  ggsave("blupF.png",  plot=gg[[5]], height = goH, width = goW, units = 'in')
  ggsave("blupM.png",  plot=gg[[6]], height = goH, width = goW, units = 'in')
  
  
}

