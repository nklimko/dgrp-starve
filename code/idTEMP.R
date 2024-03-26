#!/usr/bin/env Rscript

#setup----
if(TRUE){
  library(dplyr)
  library(argparse)
  
  options(error = function() traceback(3))
  set.seed(123)
}

#test----
if(0){
  
  inPath <- 'data/11_multimatch/f_sr.top3.Rds'
  
  
  
}

#function----
idMaker <- function(inPath,
                    outPath,
                    iter,
                    fold,
                    wildset){
  
  xY <- na.omit(readRDS(inPath))
  n <- dim(xY)[1]
  
  ids <- vector(mode='list', length=iter)
  
  for(i in 1:iter)
  {
    ids[[i]] <- sample(1:n, as.integer(n / fold))
  }
  
  saveRDS(ids[[wildset]], outPath)
  
}

#args----
parser <- ArgumentParser(description= 'snakemake transfer')

parser$add_argument('--input')
parser$add_argument('--output')
parser$add_argument('--iter', type='integer')
parser$add_argument('--fold', type='integer')
parser$add_argument('--wildset', type='integer')

snake <- parser$parse_args()
print(str(snake))

#call----
idMaker(snake$input,
        snake$output,
        snake$iter,
        snake$fold,
        snake$wildset)





