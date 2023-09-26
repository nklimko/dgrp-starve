#!/usr/bin/env Rscript
#args = commandArgs(trailingOnly=TRUE)


if(TRUE){
  #regular
  library(dplyr)
  library(data.table)
  library(doParallel)
  library(doRNG)
  #models tested
  library(qgg)
  
  options(error = function() traceback(3))
  
  #seed
  set.seed(1)
}


inPath <- "data/1_matched/f_starvation.Rds"
outPath <- "data/gremlin.Rds"
threads <- 1
maxit <- 10000

homemadeGREML <- function(inPath, outPath, threads, maxit) {
  
  xp <- readRDS(inPath)  
  
  X <- xp[,2:dim(xp)[2]]
  y <- unlist(xp[,1])
  
  W <- scale(X)
  mu <- matrix(rep(1, length(y)), ncol=1)
  rownames(mu) <- rownames(X)
  names(y) <- rownames(X)
  TRM <- tcrossprod(W)/ncol(W)
  
  
  iter <- 50
  corLoop <- rep(9999, iter)
  
  registerDoParallel(cores=threads)
  
  ids <- readRDS("data/id_bank.Rds")
  
  corLoop <- foreach(i=1:iter) %dopar% {
    
    #setup train and test sets with trait vectors
    test_IDs <- unlist(ids[i])
    names(test_IDs) <- as.character(test_IDs)
    ### QGG::GREML
    runtime <- sys.time(fitGreml <- qgg::greml(y=y, X=mu, GRM=list(A=TRM), validate = test_IDs, verbose=FALSE, maxit=maxit)
    )
    corResult <- fitGreml$accuracy$Corr
    
      final <- list(cor=corResult, time=runtime)
      final
  }  
  
  saveRDS(corLoop, outPath)
  
}

homemadeGREML(snakemake@input[[1]],
              snakemake@output[[1]],
              snakemake@threads,
              snakemake@params[['maxit']])

