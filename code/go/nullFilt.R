#!/usr/bin/env Rscript
 
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
filt <- function(dataList, sex, outPath){
  
  cor <- sapply(dataList, getCor)
  meanCor <- mean(unlist(cor))
  
  final <- c(sex, meanCor)
  names(final) <- c('sex', 'cor')
  
  saveRDS(final, outPath)
  
}

#args----
parser <- ArgumentParser(description= 'snakemake transfer')

parser$add_argument("--dataList", nargs='+')
parser$add_argument("--sex")
parser$add_argument("--outPath")
snake <- parser$parse_args()
print(str(snake))

#call----
filt(snake$dataList,
     snake$sex,
     snake$outPath)



