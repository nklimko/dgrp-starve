#!/usr/bin/env Rscript

#setup----
if(TRUE){
  library(data.table)
  library(doParallel)
  library(doRNG)
  library(dplyr)
  library(argparse)
  
  library(sparsegl)
  
  options(error = function() traceback(3))
  set.seed(123)
}

#test----
if(FALSE){
  inPath <- 'data/01_matched/f_starvation.Rds'
  idPath <- 'data/02_ids/f/ids_1.Rds'
  goPath <- 'data/go/03_goterms/sexf/GO.0045819.Rds'
  
}

#function----
homemadeSparse <- function(inPath,
                           idPath,
                           goPath,
                           outPath, 
                           nlambda) {
  
  xp <- readRDS(inPath)  
  
  X <- xp[,-1] 
  y <- unlist(xp[,1])
  W <- scale(X)
  
  GOids <- readRDS(goPath)
  
  test_IDs <- readRDS(idPath)
  
  #subset train and test sets
  W_train <- W[-test_IDs,]
  W_test <- W[test_IDs,]
  y_train <- y[-test_IDs]
  y_test <- y[test_IDs]
  
  groupBase <- rep(2, dim(X)[2])
  
  groupBase[GOids] <- 1
  
  #customL <- readRDS('customL.Rds')
  
  runtime <- system.time(fit <- cv.sparsegl(W_train, y_train, groupBase, nfolds=5, nlambda=nlambda))
  
  print(fit)
  
  y_calc <- predict(fit, W_test, s="lambda.min")
  corResult <- cor(y_test, y_calc)
  
  final <- list(cor=corResult, time=runtime)
  #final <- list(cor=corResult, time=runtime, fit=fit)
  
  saveRDS(final, outPath)
  
}  


#args----
parser <- ArgumentParser(description= 'snakemake transfer')

parser$add_argument('--inPath')
parser$add_argument('--idPath')
parser$add_argument('--goPath')
parser$add_argument('--outPath')
parser$add_argument('--nlambda', type='integer')
#parser$add_argument('--parallel', type='integer')
#parser$add_argument('--family')
#parser$add_argument('--alpha', type='integer')
#parser$add_argument('--nfolds', type='integer')
#parser$add_argument('--dfmax', type='integer')
#parser$add_argument('--relax', type='integer')

snake <- parser$parse_args()
print(str(snake))

#call----
homemadeSparse(snake$inPath,
               snake$idPath,
               snake$goPath,
               snake$outPath,
               snake$nlambda)

