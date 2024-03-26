#!/usr/bin/env Rscript

#setup----
if(TRUE){
  library(data.table)
  library(dplyr)
  library(argparse)
  library(mashr)
  library(mr.ash.alpha)
  library(glmnet)
  
  options(error = function() traceback(3))
  set.seed(123)
}

#test----
if (FALSE) {
  script <- 'code/method/mr.ash.R'
  inPath <- 'data/01_matched/f_starvation.Rds'
  idPath <- 'data/02_ids/f/ids_1.Rds'
  lassoPath <- 'data/sr/23_paropt/sexf/lasso/1.Rds'
  
  outPath <- 'data/sr/23_paropt/sexf/mr.ash/1.Rds'
}

#function----
homemadeMash <- function(inPath,
                         idPath,
                         lassoPath,
                         outPath) {
  glmbase <- readRDS(lassoPath)
  glmfit <- glmbase$fit
  
  yX <- na.omit(readRDS(inPath))
  
  y <- yX[,1]
  X <- yX[,-1]
  
  ids <- readRDS(idPath)
  
  test_IDs <- ids
  
  Xtrain <- scale(X[-test_IDs,])
  Ytrain <- matrix(unlist(y[-test_IDs]), ncol=1)
  
  Xtest <- scale(X[test_IDs,])
  Ytest <- matrix(unlist(y[test_IDs]), ncol=1)
  
  beta_base <- as.vector(glmnet::coef.glmnet(glmfit, s='lambda.min'))
  beta_init <- beta_base[-1]
  
  runtime <- system.time(fit <- mr.ash(X=Xtrain, y=Ytrain, beta.init=beta_init))
  
  Ypred <- predict(fit, Xtest)
  
  corResult <- cor(Ytest, Ypred)
  final <- list(cor=corResult, time=runtime, fit=fit)
  
  saveRDS(final, outPath)
  
}

#args----
parser <- ArgumentParser(description= 'snakemake transfer')

parser$add_argument("--input")
parser$add_argument("--ids")
parser$add_argument("--lasso")
parser$add_argument("--output")

snake <- parser$parse_args()
print(str(snake))

#call----
homemadeMash(snake$input,
             snake$ids,
             snake$lasso,
             snake$out)




