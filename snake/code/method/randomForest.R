#!/usr/bin/env Rscript

#setup----
if(TRUE){
  library(data.table)
  library(doParallel)
  library(doRNG)
  library(dplyr)
  library(argparse)
  
  library(randomForest)
  
  options(error = function() traceback(3))
  set.seed(123)
}


if(0){
  inPath <- 'data/10_matched/f_starvation.Rds'
  idPath <- 'data/02_ids/ids_1_f_top3.Rds'
}

#function----
treeFitty <- function(inPath,
                      idPath,
                      outPath,
                      ntree){
  
  yX <- na.omit(readRDS(inPath))
  
  y <- yX[,1]
  X <- yX[,-1]
  
  ids <- readRDS(idPath)
  
  test_IDs <- ids
  
  Xtrain <- scale(X[-test_IDs,])
  Ytrain <- matrix(unlist(y[-test_IDs]))
  
  Xtest <- scale(X[test_IDs,])
  Ytest <- matrix(unlist(y[test_IDs]))
  
  
  runtime <- system.time(treeFit <- randomForest(x = Xtrain,
                                                 y = Ytrain,
                                                 ntree = ntree))
  
  
  # Predicting the Test set results
  Ypred <- predict(treeFit, newdata = Xtest)
  
  corResult <- cor(Ytest, Ypred)
  final <- list(cor=corResult, time=runtime)
  
  saveRDS(final, outPath)
  
}


#args----
parser <- ArgumentParser(description= 'snakemake transfer')

parser$add_argument("--inPath")
parser$add_argument("--idPath")
parser$add_argument("--outPath")
parser$add_argument('--ntree', type='integer')
snake <- parser$parse_args()
print(str(snake))

#call----
treeFitty(snake$inPath,
          snake$idPath,
          snake$outPath,
          snake$ntree)


