#!/usr/bin/env Rscript

#setup----
if(TRUE){
  library(data.table)
  library(tidyverse)
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
                           warnPath,
                           nlambda){
  
  #data readin and splitting
  xp <- readRDS(inPath)
  GOids <- readRDS(goPath)
  test_IDs <- readRDS(idPath)
  
  X <- xp[,-1] 
  y <- unlist(xp[,1])
  W <- scale(X)
  
  #subset train and test sets
  W_train <- W[-test_IDs,]
  W_test <- W[test_IDs,]
  y_train <- y[-test_IDs]
  y_test <- y[test_IDs]
  
  #create groups based on GO ids, default is group 2
  groupBase <- rep(2, dim(X)[2])
  groupBase[GOids] <- 1
  
  #customL <- readRDS('customL.Rds')
  #sparsegl call
  runtime <- system.time(fit <- cv.sparsegl(W_train, y_train, groupBase, nfolds=5, nlambda=nlambda))
  
  #correlation calculation
  y_calc <- predict(fit, W_test, s="lambda.min")
  corResult <- cor(y_test, y_calc)
  
  #debug
  print(fit)
  fitSave <- TRUE
  
  #capture print into line readthrough
  fitPrint <- capture.output(print(fit))
  print('fitprint captured')
  
  #largest lambda warning indicates true warning of lambda.min == smallest
  counter <- 0
  
  for(i in 1:length(fitPrint)){
    if(str_detect(fitPrint[i], 'largest')){
      counter <- counter + 1
    }
  }
  
  if(counter == 0){
    #save fit for final
    print('false error(smallest warning)')
    final <- list(cor=corResult, time=runtime, fit=fit, nlambda = nlambda)
    saveRDS(final, outPath)
  }else{
    #create wildcard base for reruns
    #dont save broken fit
    print('error detected(largest warning)')
    print('intentionally failing run')
    final <- list(cor=corResult, time=runtime, fit='minWarn')
    saveRDS(GOids, warnPath)
    #file.create(warnPath)
  }
  
  print('end script')
  
}

#args----
parser <- ArgumentParser(description= 'snakemake transfer')

parser$add_argument('--inPath')
parser$add_argument('--idPath')
parser$add_argument('--goPath')
parser$add_argument('--outPath')
parser$add_argument('--warnPath')
parser$add_argument('--nlambda', type='integer')

snake <- parser$parse_args()
print(str(snake))

#call----
homemadeSparse(snake$inPath,
               snake$idPath,
               snake$goPath,
               snake$outPath,
               snake$warnPath,
               snake$nlambda)