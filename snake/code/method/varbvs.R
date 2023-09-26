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
homemadeVARBVS <- function(inPath, outPath, cores, loopIter, maxiter, sa) {
  
  xp <- readRDS(inPath)  
  
  X <- xp[,2:dim(xp)[2]]
  y <- unlist(xp[,1])
  W <- scale(X)
  
  ids <- readRDS("data/id_bank.Rds")
  
  registerDoParallel(cores = cores)
  registerDoRNG(123)
  
  loopIter <- loopIter
  
  #loop----
  corLoop <- foreach(i = 1:loopIter) %dorng% {
    
    #setup train and test sets with trait vectors
    test_IDs <- unlist(ids[i])
    
    W_train <- W[-test_IDs,]
    W_test <- W[test_IDs,]
    y_train <- y[-test_IDs]
    y_test <- y[test_IDs]
    
    ### VARBVS
    
    runtime <- system.time(fitVarb <- varbvs::varbvs(X = W_train, Z=NULL, y=y_train, family = "gaussian", logodds=seq(-3.5,-1,0.1), sa = sa, maxiter = maxiter, verbose=FALSE)
    )
    y_calc <- predict(fitVarb, X=W_test)
    corResult <- cor(y_test, y_calc)
    
    #Overall result
    final <- list(cor=corResult, time=runtime)
    final
    
  }  
  
  saveRDS(corLoop, outPath)
  
}

#args----
parser <- ArgumentParser(description= 'snakemake transfer')

parser$add_argument('--input')
parser$add_argument('--output')
parser$add_argument('--cpus', type='integer')
parser$add_argument('--loopIter', type='integer')
parser$add_argument('--maxiter', type='integer')
parser$add_argument('--sa', type='integer')

snake <- parser$parse_args()
print(str(snake))


#call----
homemadeVARBVS(snake$input,
               snake$output,
               snake$cpus,
               snake$loopIter,
               snake$maxiter,
               snake$sa)




