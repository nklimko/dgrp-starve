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
                         idPath,
                         goPath,
                         saveAt,
                         outPath,
                         goterm,
                         sex,
                         niter,
                         burnin,
                         thin,
                         RMAX,
                         model){

  xp <- readRDS(inPath)

  X <- xp[,-1]
  y <- unlist(xp[, 1])

  W <- scale(X)
  go_ids <- readRDS(goPath)

  WGO <- W[,go_ids]
  WNON <- W[,-go_ids]

  #GRMs created using GO subsets
  GGO <- tcrossprod(WGO)/ncol(WGO)
  GNON <- tcrossprod(WNON)/ncol(WNON)

  #custom model of two BLUP effect terms
  customETA <- list(list(K = GGO, model = model, saveEffects=TRUE), list(K=GNON, model=model, saveEffects=TRUE))

  #read in test/train ids
  ids <- readRDS(idPath)

  #copy vector for prediction later
  Ytrain <- y

  #remove data for prediction later
  Ytrain[ids] <- NA

  #preserve niter burnin schematic
  niter <- niter + burnin


  saveAt <- paste0(saveAt, '/tgo_')

  ### BGLR input model from function list(G=GRM)
  runtime <- system.time(fitBGLR <-
                           BGLR::BGLR(
                             Ytrain,
                             response_type = "gaussian",
                             ETA = customETA,
                             R2=RMAX,
                             nIter = niter,
                             burnIn = burnin,
                             thin = thin,
                             saveAt = saveAt,
                             verbose = FALSE
                           ))

  refill <- predict(fitBGLR)

  corResult <- cor(y[ids], refill[ids])

  final <- list(cor=corResult, time=runtime, fit=fitBGLR)
#  final <- list(cor=corResult, time=runtime)

  #check cor debug
  cat(paste0('cor: ', final$cor,'\n'))

  #save all cor and runtime by subset to separate outPath
  saveRDS(final, outPath)

}



#args----
parser <- ArgumentParser(description= 'snakemake transfer')

parser$add_argument('--inPath')
parser$add_argument('--idPath')
parser$add_argument('--goPath')
parser$add_argument('--saveAt')
parser$add_argument('--outPath')
parser$add_argument('--goterm')
parser$add_argument('--sex')
parser$add_argument('--niter', type='integer')
parser$add_argument('--burnin', type='integer')
parser$add_argument('--thin', type='integer')
parser$add_argument('--RMAX', type='double')
parser$add_argument('--model')

snake <- parser$parse_args()
print(str(snake))

#call----
homemadeBGLR(snake$inPath,
             snake$idPath,
             snake$goPath,
             snake$saveAt,
             snake$outPath,
             snake$goterm,
             snake$sex,
             snake$niter,
             snake$burnin,
             snake$thin,
             snake$RMAX,
             snake$model)

