#!/usr/bin/env Rscript

#setup----
if(TRUE){
  library(data.table)
  library(doParallel)
  library(doRNG)
  library(dplyr)
  library(argparse)
  library(BGLR)
  
  options(error = function() traceback(3))
  set.seed(123)
}

#test----
if (FALSE) {
  inPath <- 'data/11_multimatch/f_sr.top3.Rds'
  genPath <- 'junkgen.Rds'
  resPath <- 'junkres.Rds'
  nIter <- 13
  burnIn <- 3
  thin <- 1
  R2 <- 0.5
  model <- 'SpikeSlab'
  saveAt <- 'data/bglr/covtest_'
  traitCount <- 4
  
  cov_multiBGLR(inPath,
                genPath,
                resPath,
                nIter,
                burnIn,
                thin,
                R2,
                model,
                saveAt,
                traitCount)
}

#function----
cov_multiBGLR <- function(inPath,
                          genPath,
                          resPath,
                          nIter,
                          burnIn,
                          thin,
                          R2,
                          model,
                          saveAt,
                          traitCount) {
  
  
  yX <- na.omit(readRDS(inPath))
  
  y <- yX[,1:traitCount]
  X <- yX[,-(1:traitCount)]
  
  W <- scale(X)
  G <- tcrossprod(W)/ncol(W)
  
  if(model=='RKHS'){
    print('rkhs')
    customETA <- list(list(K = G, model = model))
  }else if(model=='SpikeSlab'){
    print('spikeslab')
    customETA <- list(list(X = W, model = model))
  }
  
  
  y_train <- matrix(unlist(y), ncol=traitCount)
  
  #solo-call----
  ### BGLR input model from function list(G=GRM)
  fitBGLR <- BGLR::Multitrait(y_train,
                              ETA = customETA,
                              nIter = nIter,
                              burnIn = burnIn,
                              thin = thin,
                              verbose = FALSE,
                              R2 = R2,
                              saveAt = saveAt)
  
  
  #generate per method
  covGen <- cov2cor(fitBGLR$ETA[[1]]$Cov$Omega)
  covRes <- cov2cor(fitBGLR$resCov$R)
  
  saveRDS(covGen, genPath)
  saveRDS(covRes, resPath)
  
}


#args----

parser <- ArgumentParser()

print("THEBUG")

parser$add_argument('--input')
parser$add_argument('--genPath')
parser$add_argument('--resPath')
parser$add_argument('--nIter', type='integer')
parser$add_argument('--burnIn', type='integer')
parser$add_argument('--thin', type='integer')
parser$add_argument('--R2', type='double')
parser$add_argument('--model')
parser$add_argument('--saveAt')
parser$add_argument('--traitCount', type='integer')

print('parse str:')

print(str(parser))

print('postParse')
snake <- parser$parse_args()

print('about to snakeprint')

print(str(snake))

print("snake ABOVE")

#call----
cov_multiBGLR(snake$input,
              snake$genPath,
              snake$resPath,
              snake$nIter,
              snake$burnIn,
              snake$thin,
              snake$R2,
              snake$model,
              snake$saveAt,
              snake$traitCount)




