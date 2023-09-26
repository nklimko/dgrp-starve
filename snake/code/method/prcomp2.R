#!/usr/bin/env Rscript

#setup----
if(TRUE){
  library(data.table)
  library(doParallel)
  library(doRNG)
  library(dplyr)
  library(argparse)
  
  
  options(error = function() traceback(3))
  set.seed(123)
}

#testing----
if(0){
  inPath <- 'data/sr/10_matched/f_starvation.Rds'
  pcPath <- 'data/sr/12_pc/f/95/pcs.Rds'
  idPath <- 'data/sr/02_ids/f/ids_1.Rds'
  outPath <- 'data/junk.Rds'
}

#function----
prince <- function(pcPath, idPath, outPath){
  
  yPC <- readRDS(pcPath)
  test_IDs <- readRDS(idPath)
  
  trainSet <- data.table(yPC[-test_IDs,])
  testSet <- data.table(yPC[test_IDs,])
  
  Ytest <- testSet[,1]
  PCtest <- testSet[,-1]
  
  runtime <- system.time(fitPC <- lm(trait ~ ., data = trainSet))
  
  Ypred <- predict(fitPC, newdata = PCtest)
  
  corResult <- cor(Ypred, Ytest)
  
  final <- list(cor=corResult, time=runtime)
  
  saveRDS(final, outPath)
}


#args----
parser <- ArgumentParser(description= 'snakemake transfer')

parser$add_argument('--pcPath')
parser$add_argument('--idPath')
parser$add_argument('--outPath')

snake <- parser$parse_args()
print(str(snake))

#call----
prince(snake$pcPath,
       snake$idPath,
       snake$outPath)

