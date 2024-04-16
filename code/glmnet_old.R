#!/usr/bin/env Rscript

#setup----
if(TRUE){
  library(data.table)
  library(doParallel)
  library(doRNG)
  library(dplyr)
  library(argparse)
  
  library(glmnet)
  
  options(error = function() traceback(3))
  set.seed(123)
}

#test----
if(FALSE){
  inPath23 <- 'data/10_matched/lasso_3_1000_0_f.Rds'
  inPath <- 'data/10_matched/m_starvation.Rds'
  inc <- 'data/10_matched/f_starvation.Rds'
  idPath <- 'data/02_ids/sr/m/ids_18.Rds'
  outPath <- 'data/temp.Rds'
  cpus <- 4
  alpha <- 1
  family <- 'gaussian'
  nfolds <- 3
  parallel <- 1
  relax <- 0
  dfmax <- 1000
  
  
  start <- Sys.time()
  homemadeGLMNET(inPath, idPath, outPath, cpus, parallel, family, alpha, nfolds, dfmax, relax)
  stop <- Sys.time() - start
  
}

#function----
homemadeGLMNET <- function(inPath,
                           idPath,
                           outPath,
                           cpus,
                           parallel,
                           family,
                           alpha,
                           nfolds) {
  
  xp <- readRDS(inPath)  
  
  X <- xp[,2:dim(xp)[2]] 
  y <- unlist(xp[,1])
  W <- scale(X)
  
  test_IDs <- readRDS(idPath)
  
  #subset train and test sets
  W_train <- W[-test_IDs,]
  W_test <- W[test_IDs,]
  y_train <- y[-test_IDs]
  y_test <- y[test_IDs]
  
  #parallel cluster for stepthrough
  registerDoParallel(cores = cpus)
  registerDoRNG(123)
  
  ### GLMNET::LASSO
  runtime <- system.time(fitlm <- glmnet::cv.glmnet(x=W_train, y=y_train, family=family, alpha=alpha, nfolds = nfolds, parallel=parallel)
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
parser$add_argument('--cpus', type='integer')
parser$add_argument('--parallel', type='integer')
parser$add_argument('--family')
parser$add_argument('--alpha', type='integer')
parser$add_argument('--nfolds', type='integer')
parser$add_argument('--dfmax', type='integer')
parser$add_argument('--relax', type='integer')

snake <- parser$parse_args()
print(str(snake))

#call----
homemadeGLMNET(snake$input,
               snake$ids,
               snake$output,
               snake$cpus,
               snake$parallel,
               snake$family,
               snake$alpha,
               snake$nfolds)

