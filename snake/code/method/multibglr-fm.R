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
  inPath1 <- 'data/1_matched/m_starvation.Rds'
  inPath2 <- 'data/1_matched/f_starvation.Rds'
  outPath <- 'data/temp_bglr_m_starvation.Rds'
  cores <- 1
  loopIter <- 3
  model <- 'SpikeSlab'
  nIter <- 1300
  burnIn <- 300
  thin <- 5
  R2 <- 0.5
  
  start <- Sys.time()
  
  homemadeBGLR(inPath1,
                     inPath2,
                     outPath,
                     cores,
                     loopIter,
                     model,
                     nIter,
                     burnIn,
                     thin,
                     R2,
                     saveAt)
  
  stop <- Sys.time() - start
  
  stop
}

#function----
homemadeBGLR <- function(inPath1,
                         inPath2,
                         outPath,
                         cores,
                         loopIter,
                         type,
                         nIter,
                         burnIn,
                         thin,
                         R2,
                         model,
                         saveAt) {
  
  
  if(type=='m'){
    print('switched paths')
    inPathTemp <- inPath2
    inPath2 <- inPath1
    inPath1 <- inPathTemp
  }
  
  xp1 <- readRDS(inPath1)
  
  X <- xp1[, 2:dim(xp1)[2]]
  y1 <- unlist(xp1[, 1])
  
  W <- scale(X)
  G <- tcrossprod(W)/ncol(W)
  
  xp2 <- readRDS(inPath2)
  y2 <- unlist(xp2[, 1])
  
  if(model=='RKHS'){
    print('rkhs')
    customETA <- list(list(K = G, model = model))
  }else if(model=='SpikeSlab'){
    print('spikeslab')
    customETA <- list(list(X = W, model = model))
  }
  
  ids <- readRDS("data/id_bank.Rds")
  
  registerDoParallel(cores = cores)
  registerDoRNG(123)
  
  loopIter <- loopIter
  
  #loop----
  corLoop <- foreach(i = 1:loopIter) %dorng% {
    
    #setup train and test sets with trait vectors
    #test_IDs <- sample(1:n, as.integer(n / fold))
    test_IDs <- unlist(ids[i])
    
    #copy vector
    y_train1 <- y1
    y_train2 <- y2
    
    #scrub data for prediction
    y_train1[test_IDs] <- NA
    y_train2[test_IDs] <- NA
    
    y_train <- matrix(cbind(y_train1, y_train2),ncol=2)
    
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
    yfinal <- matrix(cbind(y1, y2),ncol=2)
    
    corResult <- cor(yfinal[test_IDs,1:2], fitBGLR$ETAHat[test_IDs,1:2])
    
    final <- list(cor=corResult, time=runtime)
    final
  }
  
  saveRDS(corLoop, outPath)
  
}

#args----
parser <- ArgumentParser(description= 'snakemake transfer')

parser$add_argument('--inputF')
parser$add_argument('--inputM')
parser$add_argument('--output')
parser$add_argument('--cpus', type='integer')
parser$add_argument('--loopIter', type='integer')
parser$add_argument('--type')
parser$add_argument('--nIter', type='integer')
parser$add_argument('--burnIn', type='integer')
parser$add_argument('--thin', type='integer')
parser$add_argument('--R2', type='double')
parser$add_argument('--model')
parser$add_argument('--saveAt')

snake <- parser$parse_args()
print(str(snake))


#call----
homemadeBGLR(snake$inputF,
             snake$inputM,
             snake$output,
             snake$cpus,
             snake$loopIter,
             snake$type,
             snake$nIter,
             snake$burnIn,
             snake$thin,
             snake$R2,
             snake$model,
             snake$saveAt)


