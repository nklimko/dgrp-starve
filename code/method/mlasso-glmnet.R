#!/usr/bin/env Rscript

#setup----
if(TRUE){
  library(data.table)
  library(dplyr)
  library(argparse)
  library(doParallel)
  
  library(glmnet)
  
  options(error = function() traceback(3))
  set.seed(123)
}

#test----
if(0){
  inPath <- 'data/11_multimatch/m_sr.top3.Rds'
  idPath <- 'data/top3/02_ids/m/ids_30.Rds'
  #idPath <- 'data/02_ids/ids_2_m_top3.Rds'
  outPath <- 'data/temp.Rds'
  cpus <- 4
  loopIter <- 2
  alpha <- 1
  nfolds <- 10
  parallel <- 1
  family <- 'mgaussian'
  traitCount <- 4  
  start <- Sys.time()
  homemadeGLMNET(inPath1, inPath2, outPath, cores, loopIter, type, family, alpha, nfolds, parallel)
  stop <- Sys.time() - start
  
}


#test----
if(1){
  inPath <- 'data/11_multimatch/f_sr.top3.Rds'
  idPath <- 'data/top3/02_ids/f/ids_2.Rds'
  #idPath <- 'data/02_ids/ids_2_m_top3.Rds'
  outPath <- 'data/temp.Rds'
  cpus <- 4
  alpha <- 1
  nfolds <- 10
  parallel <- 1
  family <- 'mgaussian'
  traitCount <- 4
  
}



#function----
homemadeGLMNET <- function(inPath,
                           idPath,
                           outPath,
                           cpus,
                           family,
                           alpha,
                           nfolds,
                           parallel,
                           traitCount) {
  
  
  yX <- readRDS(inPath)
  
  y <- yX[,1:traitCount]
  X <- yX[,-(1:traitCount)]
  
  W <- scale(X)
  
  
  #loop----
  
  ids <- readRDS(idPath)
  
  #setup train and test sets with trait vectors
  test_IDs <- ids
  
  #copy vector
  
  y_train <- matrix(unlist(y[-test_IDs,]), ncol=traitCount)
  y_test <- matrix(unlist(y[test_IDs,]), ncol=traitCount)
  
  X_train <- scale(X[-test_IDs,])
  X_test <- scale(X[test_IDs,])
  
  
  ### GLMNET::LASSO
  
  registerDoParallel(cores = cpus)
  
  runtime <- system.time(fitlm <- glmnet::cv.glmnet(x=X_train, y=y_train, family=family, alpha=alpha, nfolds = nfolds, parallel = parallel)
  )
  
  #y_calcBase <- predict(fitlm, X_test, s=fitlm$lambda.min)
  #lamba.min broke predictions
  
  y_calc <- matrix(predict(fitlm, X_test, type="response", s = fitlm$lambda.min*0.5), ncol=traitCount)
  
  corResult <- cor(y_test, y_calc)
  
  final <- list(cor=corResult, time=runtime)
  
  saveRDS(final, outPath)
  
}

#args----
parser <- ArgumentParser(description= 'snakemake transfer')

parser$add_argument('--inPath')
parser$add_argument('--idPath')
parser$add_argument('--outPath')
parser$add_argument('--cpus', type='integer')
parser$add_argument('--family')
parser$add_argument('--alpha', type='integer')
parser$add_argument('--nfolds', type='integer')
parser$add_argument('--parallel', type='integer')
parser$add_argument('--traitCount', type='integer')

snake <- parser$parse_args()
print(str(snake))


#call----
homemadeGLMNET(snake$inPath,
               snake$idPath,
               snake$outPath,
               snake$cpus,
               snake$family,
               snake$alpha,
               snake$nfolds,
               snake$parallel,
               snake$traitCount)

