#!/usr/bin/env Rscript

#setup----
if(TRUE){
  library(data.table)
  library(doParallel)
  library(doRNG)
  library(dplyr)
  library(argparse)
  
  #Principal Component Regression
  library(pls)
  
  options(error = function() traceback(3))
  set.seed(123)
}


#testing----
if(0){
  inPathF <- 'data/sr/10_matched/f_starvation.Rds'
  inPathM <- 'data/sr/10_matched/m_starvation.Rds'
  inPath <- 'data/sr/10_matched/f_starvation.Rds'
  percentLimit <- 95
  #outPath <- 'data/sr/12_pc/{type}/{pctLim}/starvation.Rds'
  outPathF <- 'data/sr/12_pc/f/95/pcs.Rds'
  outPathM <- 'data/sr/12_pc/m/95/pcs.Rds'
  outPath <- 'data/junk.Rds'
}


pcMaker <- function(inPath, percentLimit, outPath){
  
  
  yX <- na.omit(readRDS(inPath))
  
  y <- yX[,1]
  y <- scale(y)
  
  X <- yX[,-1]
  
  pca <- prcomp(X)
  
  pct_var_explained <- pca$sdev^2 / sum(pca$sdev^2)
  
  truePCT <- percentLimit / 100
  
  #selects only the pcas where the cumulative sum of total variance explained is less than cutoff
  runtime <- system.time(pcs <- pca$x[,(cumsum(pct_var_explained) < truePCT)])
  
  final <- data.table(y, pcs)
  names(final)[1] <- 'trait'
  
  saveRDS(final, outPath)
  
  
  
  
}



pcMaker(inPathF, 95, outPathF)

pcMaker(inPathM, 95, outPathM)






