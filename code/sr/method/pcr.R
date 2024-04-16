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


#function----
prince <- function(inPath, idPath, nopts, nfolds, outPath){
  
  yX <- na.omit(readRDS(inPath))
  
  y <- yX[,1]
  y <- scale(y)
  
  X <- yX[,-1]
  
  ids <- readRDS(idPath)
  
  base <- 1:length(y)
  
  test_IDs <- base[-ids]
  
  Xtest <- data.frame(scale(X[ids,]))
  Ytest <- matrix(unlist(y[ids,]))
  
  runtime <- system.time(fitPCR <- pcr(trait ~., data = yX, subset = test_IDs, validation = "CV", segments=nfolds, y=TRUE))
  
  eightCount <- predict(fitPCR, newdata=Xtest, ncomp=ncustom)
  allCount   <- predict(fitPCR, newdata=Xtest, ncomp=length(varList))
  
  corResult <- cor(Ytest, allCount)
  altCor <- cor(Ytest, eightCount)
  
  final <- list(cor=corResult, time=runtime, altCor=eigthCount)
  
  saveRDS(final, outPath)
  
}

#args----
parser <- ArgumentParser(description= 'snakemake transfer')

parser$add_argument("--inPath")
parser$add_argument("--idPath")
parser$add_argument("--nopts", type="integer")
parser$add_argument("--nfolds", type="integer")
parser$add_argument("--outPath")
snake <- parser$parse_args()
print(str(snake))

#call----
prince(snake$inPath,
       snake$idPath,
       snake$nopts,
       snake$nfolds,
       snake$outPath)
