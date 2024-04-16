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
                         goterm,
                         sex,
                         niter,
                         burnin,
                         thin,
                         repCount,
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
  customETA <- list(list(K = GGO, model = model, saveEffects=FALSE), list(K=GNON, model=model, saveEffects=FALSE))
  
  #function to handle each replicate as a number from 1 to max rep count
  gblupHandler <- function(i, goterm, sex, y, customETA, niter, burnin, thin, RMAX){
    
    set.seed(123)
    
    #iter marker
    print(paste0('loop ', i))
    
    #id specific paths i/o
    idPath <- paste0('snake/data/02_ids/', sex, '/ids_', i, '.Rds')
    saveAt <- paste0('snake/data/bglr/', sex, '/tblup/', goterm, '/', i,'_')
    midPath <-paste0('snake/data/go/24_goCor/', sex, '/tblup/', goterm,'/',i,'.Rds')
    
    #read in test/train ids
    ids <- readRDS(idPath)
    
    #copy vector for prediction later
    Ytrain <- y
    
    #remove data for prediction later
    Ytrain[ids] <- NA
    
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
                             )
    )
    
    refill <- predict(fitBGLR)
    
    corResult <- cor(y[ids], refill[ids])
    
    #final <- list(cor=corResult, time=runtime, fit=fitBGLR)
    final <- list(cor=corResult, time=runtime) 
    
    #check cor debug
    cat(paste0('cor: ', final$cor,'\n'))
    
    #save all cor and runtime by subset to separate outPath 
    saveRDS(final, midPath)
    
    return(final)
  }
  
  #declare number of reps to use from snakemake args
  reps <- 1:repCount
  
  #corResults contains cor and runtime for all reps in list/sapply type
  runtime <- system.time(corResults <- sapply(reps, gblupHandler, goterm = goterm, sex=sex, y=y, customETA = customETA, niter=niter, burnin=burnin, thin=thin, RMAX=RMAX))  
  
  #extract cors
  cors <- unlist(corResults[1,])
  
  #save term and mean correlation coefficient as data table row
  finalRow <- data.table(term=goterm, cor=mean(cors))
  
  saveRDS(finalRow, outPath)
  
}



#args----
parser <- ArgumentParser(description= 'snakemake transfer')

parser$add_argument('--inPath')
parser$add_argument('--goPath')
parser$add_argument('--outPath')
parser$add_argument('--goterm')
parser$add_argument('--sex')
parser$add_argument('--niter', type='integer')
parser$add_argument('--burnin', type='integer')
parser$add_argument('--thin', type='integer')
parser$add_argument('--repCount', type='integer')
parser$add_argument('--RMAX', type='double')
parser$add_argument('--model')

snake <- parser$parse_args()
print(str(snake))

#call----
homemadeBGLR(snake$inPath,
             snake$goPath,
             snake$outPath,
             snake$goterm,
             snake$sex,
             snake$niter,
             snake$burnin,
             snake$thin,
             snake$repCount,
             snake$RMAX,
             snake$model)

