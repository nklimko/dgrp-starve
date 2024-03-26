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
  inPath <- 'data/sr/10_matched/f_starvation.Rds'
  idPath <- 'data/02_ids/sr/f/ids_1.Rds'
  outPath <- 'bayesTest.Rds'
  nIter <- 130000
  burnIn <- 30000
  thin <- 50
  R2 <- 0.8
  saveAt <- 'data/bglr/f/trace_'
  i <- 1
  model <- 'SpikeSlab'
}


#function----
homemadeBGLR <- function(inPath,
                         idPath,
                         outPath,
                         R2,
                         model,
                         saveAt) {
  #read in data
  yX<- readRDS(inPath)
  X <- yX[, -1]
  y <- unlist(yX[, 1])
  
  #normalize X
  W <- scale(X)
  
  ##model input
  #RKHS for GBLUP
  #SpikeSlab for BayesC
  if(model=='RKHS'){
    print('rkhs')
    G <- tcrossprod(W)/ncol(W)
    customETA <- list(list(K = G, model = model))
  }else if(model=='SpikeSlab'){
    print('spikeslab')
    customETA <- list(list(X = W, model = "BayesC"))
  }
  
  #read in test/train ids
  ids <- readRDS(idPath)
  
  #copy vector for prediction later
  Ytrain <- y
  
  #remove data for prediction later
  Ytrain[ids] <- NA
  
  ### BGLR input model using custpmETA from model selection
  runtime <- system.time(fitBGLR <- BGLR::BGLR(Ytrain,
                                               response_type = "gaussian",
                                               ETA = customETA,
                                               verbose = TRUE,
                                               R2 = R2,
                                               saveAt = saveAt)
  )
  
  #predict.BGLR() fills in NA values
  Ypred <- predict(fitBGLR)
  
  #compare actual to predicted only, subset by ids
  corResult <- cor(y[ids], Ypred[ids])
  
  #Overall result: correlation and runtime
  final <- list(cor=corResult, time=runtime)
  saveRDS(final, outPath)
  
}

#args----
parser <- ArgumentParser(description= 'snakemake transfer')

#All inputs from snakemake config file
parser$add_argument('--input')
parser$add_argument('--ids')
parser$add_argument('--output')
parser$add_argument('--R2', type='double')
parser$add_argument('--model')
parser$add_argument('--saveAt')

snake <- parser$parse_args()
print(str(snake)) #debug print

#call----
homemadeBGLR(snake$input,
             snake$ids,
             snake$output,
             snake$R2,
             snake$model,
             snake$saveAt)
