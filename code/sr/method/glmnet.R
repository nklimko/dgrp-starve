#!/usr/bin/env Rscript

#setup----
if(TRUE){
  library(data.table)
  library(dplyr)
  library(argparse)
  library(glmnet)
  
  options(error = function() traceback(3))
  set.seed(123)
}

#function----
homemadeGLMNET <- function(inPath,
                           idPath,
                           outPath,
                           family,
                           alpha,
                           nfolds) {
  
  xp <- readRDS(inPath)  
  
  X <- xp[,-1] 
  y <- unlist(xp[,1])
  W <- scale(X)
  
  test_IDs <- readRDS(idPath)
  
  #subset train and test sets
  W_train <- W[-test_IDs,]
  W_test <- W[test_IDs,]
  y_train <- y[-test_IDs]
  y_test <- y[test_IDs]
  
  ### GLMNET::LASSO
  runtime <- system.time(fitlm <- glmnet::cv.glmnet(x=W_train, y=y_train, family=family, alpha=alpha, nfolds = nfolds)
  )
  
  y_calc <- predict(fitlm, W_test, s="lambda.min")
  corResult <- cor(y_test, y_calc)
  
  final <- list(cor=corResult, time=runtime, fit = fitlm)
  
  saveRDS(final, outPath)
  
}


#args----
parser <- ArgumentParser(description= 'snakemake transfer')

parser$add_argument('--input')
parser$add_argument('--ids')
parser$add_argument('--output')
parser$add_argument('--family')
parser$add_argument('--alpha', type='integer')
parser$add_argument('--nfolds', type='integer')

snake <- parser$parse_args()
print(str(snake))

#call----
homemadeGLMNET(snake$input,
               snake$ids,
               snake$output,
               snake$family,
               snake$alpha,
               snake$nfolds)

