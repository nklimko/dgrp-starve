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
prince <- function(inPath,
                   idPath,
                   method,
                   nfolds,
                   outPath){

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
    runtime <- system.time(fitPCA <- pcr(trait ~.,  data = yX, subset = test_IDs, validation = "CV", segments=nfolds))
  }else{
    print(paste0('PLS Method : ', method))
    runtime <- system.time(fitPCA <- plsr(trait ~., data = yX, subset = test_IDs, validation = "CV", segments=nfolds, method=method))
  }

  #pulls errors, morphs 2 x ncomp table into vector of cv / adjCV alternating, extract ncomp from
  errors <- RMSEP(fitPCA)
  delist <- unlist(errors[1])
  ncomps <- as.integer((which.min(delist) - 2) / 2)

  print(paste0('ncomps: ', ncomps))
  if(ncomps==0){
    corResult <- NA
  }else{
    allCount   <- predict(fitPCA, newdata=Xtest, ncomp=ncomps)
    corResult <- cor(Ytest, allCount)
  }

  final <- list(cor=corResult, time=runtime)

  saveRDS(final, outPath)

}

#args----
parser <- ArgumentParser(description= 'snakemake transfer')

parser$add_argument("--inPath")
parser$add_argument("--idPath")
parser$add_argument("--method")
parser$add_argument("--nfolds", type="integer")
parser$add_argument("--outPath")
snake <- parser$parse_args()
print(str(snake))

#call----
prince(snake$inPath,
       snake$idPath,
       snake$method,
       snake$nfolds,
       snake$outPath)
