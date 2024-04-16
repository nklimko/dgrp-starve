#!/usr/bin/env Rscript

#setup----
library(data.table)
library(dplyr)
library(argparse)
library(BGLR)

options(error = function() traceback(3))
set.seed(123)

#function----
center_vec <- function(x) {x - mean(x)}

#bglr call----
homemadeBGLR <- function(inPath,
                         idPath,
                         goPath,
                         outPath,
                         nIter,
                         burnIn,
                         thin,
                         R2_max,
                         R2_null,
                         model,
                         saveAt){

  nIter <- nIter + burnIn

  xp <- readRDS(inPath)


  X <- xp[, 2:dim(xp)[2]]
  y <- unlist(xp[, 1])

  W <- scale(X)

  #go_ids <- seq(1,100,1)
  go_ids <- readRDS(goPath)

  WGO <- W[,go_ids]
  WNON <- W[,-go_ids]

  if(R2_null==1){
    print('null model')
    customETA <- list(list(X = W, model = model, saveEffects=TRUE))
  } else{
    print('GO model')
    customETA <- list(list(X = WGO, model = model, saveEffects=TRUE),
                      list(X = WNON, model = model, saveEffects=TRUE))
  }

  #read in test/train ids
  ids <- readRDS(idPath)

  #copy vector for prediction later
  Ytrain <- y

  #remove data for prediction later
  Ytrain[ids] <- NA

  print('check #1')
  ### BGLR input model from function list(G=GRM)
  runtime <- system.time(fitBGLR <-
                           BGLR::BGLR(
                             Ytrain,
                             response_type = "gaussian",
                             ETA = customETA,
                             nIter = nIter,
                             burnIn = burnIn,
                             thin = thin,
                             saveAt=saveAt,
                             R2=R2_max,
                             verbose = FALSE
                           )
  )

  refill <- predict(fitBGLR)

  corResult <- cor(y[ids], refill[ids])

  print('cor:')
  print(corResult)

  #Overall result
  final <- list(cor=corResult, time=runtime, fit=fitBGLR)
  #final <- list(cor=corResult, time=runtime)

  saveRDS(final, outPath)
}

#args----
parser <- ArgumentParser(description= 'snakemake transfer')

parser$add_argument('--inPath')
parser$add_argument('--idPath')
parser$add_argument('--goPath')
parser$add_argument('--outPath')
parser$add_argument('--nIter', type='integer')
parser$add_argument('--burnIn', type='integer')
parser$add_argument('--thin', type='integer')
parser$add_argument('--R2_max', type='double')
parser$add_argument('--R2_null', type='integer')
parser$add_argument('--model')
parser$add_argument('--saveAt')

snake <- parser$parse_args()
print(str(snake))

#call----
homemadeBGLR(snake$inPath,
             snake$idPath,
             snake$goPath,
             snake$outPath,
             snake$nIter,
             snake$burnIn,
             snake$thin,
             snake$R2_max,
             snake$R2_null,
             snake$model,
             snake$saveAt)



