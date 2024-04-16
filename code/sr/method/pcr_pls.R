#!/usr/bin/env Rscript

#setup----
if(1){
  library(data.table)
  library(dplyr)
  library(argparse)
  library(pls)
  
  options(error = function() traceback(3))
  set.seed(123)
}

if(0){
  inPath='snake/data/01_matched/m_starvation.Rds'
  idPath='snake/data/02_ids/m/ids_1.Rds'
  method='pcr'
  nopts=8
  nfolds=5
  outPath = 'deleteThis.Rds'
  
}


#function----
prince <- function(inPath, idPath, method, nopts, nfolds, outPath){
  
  yX <- na.omit(readRDS(inPath))
  
  y <- yX[,1]
  X <- yX[,-1]
  
  ids <- readRDS(idPath)
  
  base <- 1:length(unlist(y))
  
  test_IDs <- base[-ids]
  
  Xtest <- data.frame(scale(X[ids,]))
  Ytest <- matrix(unlist(y[ids,]))
  
  
  if(method=='pcr'){
    print(paste0('PCR Method : ', method))
    runtime <- system.time(fitPCA <- pcr(trait ~., data = yX, subset = test_IDs, validation = "CV", segments=nfolds))
  }else{
    print(paste0('PLS Method : ', method))#, maxit=5000
    runtime <- system.time(fitPCA <- plsr(trait ~., method=method, data = yX, subset=test_IDs, validation = "CV", segments=nfolds))
  }
  
  eightCount <- predict(fitPCA, newdata=Xtest, ncomp=nopts)
  allCount   <- predict(fitPCA, newdata=Xtest, ncomp=fitPCA$ncomp)
  
  #cor(fitPCA$y[ids], Ytest)
  
  corResult <- cor(Ytest, allCount)
  corAlternate <- cor(Ytest, eightCount)
  
  final <- list(cor=corResult, time=runtime, alt=corAlternate)
  
  saveRDS(final, outPath)
  
}

#args----
parser <- ArgumentParser(description= 'snakemake transfer')

parser$add_argument("--inPath")
parser$add_argument("--idPath")
parser$add_argument("--method")
parser$add_argument("--nopts", type="integer")
parser$add_argument("--nfolds", type="integer")
parser$add_argument("--outPath")
snake <- parser$parse_args()
print(str(snake))

#call----
prince(snake$inPath,
       snake$idPath,
       snake$method,
       snake$nopts,
       snake$nfolds,
       snake$outPath)
