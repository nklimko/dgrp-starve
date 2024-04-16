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

#function getCor----
getCor <- function(dataPath){
  data <- readRDS(dataPath)
  return(data[1])
}

#function filt----
filt <- function(dataList, method, outPath){
  
  corTake <- sapply(dataList, getCor)
  final <- data.table(cor=unlist(corTake))
  names(final) <- method
  
  saveRDS(final, outPath)
  
}







#args----
parser <- ArgumentParser(description= 'snakemake transfer')

parser$add_argument("--inPath", nargs='+')
parser$add_argument("--method")
parser$add_argument("--outPath")
snake <- parser$parse_args()
print(str(snake))

#call----
filt(snake$inPath,
     snake$method,
     snake$outPath)



