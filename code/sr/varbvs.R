#!/usr/bin/env Rscript

#setup----
if(TRUE){
  library(data.table)
  library(doParallel)
  library(doRNG)
  library(dplyr)
  library(argparse)
  
  library(varbvs)
  
  options(error = function() traceback(3))
  set.seed(123)
}

#test----
if(FALSE){
  inPath <- 'data/1_matched/f_starvation.Rds'
  outPath <- 'data/temp.Rds'
  cores <- 1
  loopIter <- 1
  maxiter <- 10000
  sa <- 1
  start <- Sys.time()
  homemadeVARBVS(inPath, outPath, cores, loopIter, maxiter, sa)
  stop <- Sys.time() - start
}

#function----
homemadeVARBVS <- function(inPath, idPath, outPath, maxiter) {
  
  xp <- readRDS(inPath)  
  
  X <- xp[,2:dim(xp)[2]]
  y <- unlist(xp[,1])
  W <- scale(X)
  
  test_IDs <- readRDS(idPath)
  
  #setup train and test sets with trait vectors
  W_train <- W[-test_IDs,]
  W_test <- W[test_IDs,]
  y_train <- y[-test_IDs]
  y_test <- y[test_IDs]
  
  ### VARBVS
  
  runtime <- system.time(fitVarb <- varbvs::varbvs(X = W_train, Z=NULL, y=y_train, family = "gaussian", logodds=seq(-3.5,-1,0.1), maxiter = maxiter, verbose=FALSE)
  )
  y_calc <- predict(fitVarb, X=W_test)
  corResult <- cor(y_test, y_calc)
  
  #Overall result
  final <- list(cor=corResult, time=runtime)
  saveRDS(final, outPath)
  
  
  
}

#args----
parser <- ArgumentParser(description= 'snakemake transfer')

parser$add_argument('--inPath')
parser$add_argument('--idPath')
parser$add_argument('--outPath')
parser$add_argument('--maxiter', type='integer')

snake <- parser$parse_args()
print(str(snake))


#call----
homemadeVARBVS(snake$inPath,
               snake$idPath,
               snake$outPath,
               snake$maxiter)




