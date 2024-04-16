#!/usr/bin/env Rscript

#setup----
if(TRUE){
  library(data.table)
  library(dplyr)
  library(argparse)
  
  library(BGLR)
  
  options(error = function() traceback(3))
  set.seed(123)
}

#test----
if (FALSE) {
  inPath <- 'data/11_multimatch/f_sr.top3.Rds'
  idPath <- 'data/top3/02_ids/m/ids_30.Rds'
  outPath <- 'junkblup.Rds'
  cpus <- 1
  iter <- 2
  model <- 'SpikeSlab'
  nIter <- 1300
  burnIn <- 300
  thin <- 5
  R2 <- 0.5
  traitCount <- 4
  saveAt <- 'data/bglr/multiblup_'
  
  homemadeMultitrait(inPath,
                     idPath,
                     outPath,
                     cpus,
                     iter,
                     model,
                     nIter,
                     burnIn,
                     thin,
                     R2,
                     traitCount,
                     saveAt)
  
  stop <- Sys.time() - start
  
  stop
}

#function----
homemadeBGLR <- function(inPath,
                         idPath,
                         outPath,
                         cpus,
                         nIter,
                         burnIn,
                         thin,
                         R2,
                         model,
                         saveAt,
                         traitCount)
{
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
  
  ids <- readRDS(idPath)
  
  
  #setup train and test sets with trait vectors
  test_IDs <- ids
  
  Ytrain <- y
  
  Ytrain[test_IDs,] <- NA
  
  y_train <- matrix(unlist(Ytrain), ncol=traitCount)
  
  ### BGLR input model from function list(G=GRM)
  
  runtime <- system.time(fitBGLR <-
                           BGLR::Multitrait(
                             y_train,
                             ETA = customETA,
                             nIter = nIter,
                             burnIn = burnIn,
                             thin = thin,
                             verbose = FALSE,
                             R2 = R2,
                             saveAt = saveAt
                           )
  )
  
  #correlation of TEST ID SECTION of original y matrix to predicted test id section from fit
  corResult <- cor(y[test_IDs, 1:traitCount], fitBGLR$ETAHat[test_IDs, 1:traitCount])
  
  final <- list(cor=corResult, time=runtime)
  saveRDS(final, outPath)
  
}

#args----
parser <- ArgumentParser(description= 'snakemake transfer')

parser$add_argument('--input')
parser$add_argument('--ids')
parser$add_argument('--output')
parser$add_argument('--cpus', type='integer')
parser$add_argument('--nIter', type='integer')
parser$add_argument('--burnIn', type='integer')
parser$add_argument('--thin', type='integer')
parser$add_argument('--R2', type='double')
parser$add_argument('--model')
parser$add_argument('--saveAt')
parser$add_argument('--traitCount', type='integer')

snake <- parser$parse_args()
print(str(snake))


#call----
homemadeBGLR(snake$input,
             snake$ids,
             snake$output,
             snake$cpus,
             snake$nIter,
             snake$burnIn,
             snake$thin,
             snake$R2,
             snake$model,
             snake$saveAt,
             snake$traitCount)

