#!/usr/bin/env Rscript

#setup----
library(data.table)
library(dplyr)
library(argparse)
library(BGLR)

options(error = function() traceback(3))
set.seed(123)

#bglr call----
homemadeBGLR <- function(inPath,
                         goPath,
                         outPath,
                         nIter,
                         burnIn,
                         thin,
                         R2_max,
                         R2_GO,
                         model,
                         saveAt){
  xp <- readRDS(inPath)
  
  X <- xp[, 2:dim(xp)[2]]
  y <- unlist(xp[, 1])
  
  W <- scale(X)
  
  #go_ids <- seq(1,100,1)
  go_ids <- readRDS(goPath)
  
  WGO <- W[,go_ids]
  WNON <- W[,-go_ids]
  
  R2_nonGO <- R2_max - R2_GO
  
  if(R2_GO==0){
    customETA <- list(list(X = W, model = model, saveEffects=FALSE, R2=R2_max))
  } else{
    customETA <- list(list(X = WGO, model = model, saveEffects=FALSE, R2=R2_GO),
                      list(X = WNON, model = model, saveEffects=FALSE, R2=R2_nonGO))
  }

  #copy vector for prediction later
  Ytrain <- y

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
                             verbose = FALSE
                           )
  )
  
  final <- list(fit=fitBGLR, time=runtime)
  #final <- list(cor=corResult, time=runtime) 
  
  saveRDS(final, outPath)
}



#args----
parser <- ArgumentParser(description= 'snakemake transfer')

parser$add_argument('--inPath')
parser$add_argument('--goPath')
parser$add_argument('--outPath')
parser$add_argument('--nIter', type='integer')
parser$add_argument('--burnIn', type='integer')
parser$add_argument('--thin', type='integer')
parser$add_argument('--R2_GO', type='double')
parser$add_argument('--R2_max', type='double')
parser$add_argument('--model')
parser$add_argument('--saveAt')

snake <- parser$parse_args()
print(str(snake))

#call----
homemadeBGLR(snake$inPath,
             snake$goPath,
             snake$outPath,
             snake$nIter,
             snake$burnIn,
             snake$thin,
             snake$R2_max,
             snake$R2_GO,
             snake$model,
             snake$saveAt)



