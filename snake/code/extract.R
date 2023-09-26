#!/usr/bin/env Rscript
#Used to extract correlation coefficients for trait of interest or
#runtime using similar methods

#setup----
if(1){
  library(dplyr)
  library(data.table)
  library(tidyverse)
  library(argparse)
  
  #options
  options(bitmapType = "cairo")
  options(error = function() traceback(3))
  
  #seed
  set.seed(123)
}


#testing----
if(0){
  dataPath <- 'data/sr/30_summary/pcr_f.Rds'
  method <- 'cor'
  outPath <- "junk.Rds"
  
}


#funciton filt----
filt <- function(dataPath, method, outPath){
  
  #data is read in from path
  data <- readRDS(dataPath)
  
  #method determines operation
  
  #sr pulls straight column, only value in 2x50 first column
  if(method=="cor"){
    col <- 1
    return(unlist(data[col, ]))
  } #time pulls elapsed runtime from system.time output, 3rd of 5 values returned
  else if(method=="time"){
    col <- 2
    start <- 3
    skip <-5
    
    raw <- unlist(data[col, ])
    trim <- raw[seq(start, length(raw), by=skip)]
    
    return(trim)
    
  }#come back and fix this, nested niche case/outdated: cor stored poorly
  else if(method=="nested"){
    hold <- vector(length=50)
    for(i in 1:length(data)){
      hold[i] <- data[[i]]$cor 
    }
    print()
    return(hold)
  }#top3 correlation stored as 4x4 covariance grid, only need first of 16 values
  else if(method=='topcor'){
    col <- 1
    start <- 1
    skip <- 16
    
    raw <- unlist(data[col, ])
    trim <- raw[seq(start, length(raw), by=skip)]
    
    return(trim)
  } else{
    #debug
    print('Method not recognized.')
    return(NA)
  }
}

#function wrapped.filt----
wrapped.filt <- function(dataPath, method, outPath){
  
  dataCol <- filt(dataPath, method)
  
  frag <- unlist(strsplit(dataPath, '/'))
  methodName <- unlist(strsplit(frag[length(frag)], '_'))[1]

  final <- data.table(dataCol)
  colnames(final) <- methodName
  
  saveRDS(final, outPath)
  
}

#args----
parser <- ArgumentParser(description= 'snakemake transfer')

parser$add_argument("--inPath")
parser$add_argument("--method")
parser$add_argument("--outPath")
snake <- parser$parse_args()
print(str(snake))

#call----
wrapped.filt(snake$inPath,
             snake$method,
             snake$outPath)



