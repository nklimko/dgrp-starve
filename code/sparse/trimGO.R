#!/usr/bin/env Rscript

#setup----
if(1){
  
  library(argparse)
  library(tidyverse)
  library(data.table)
  
  #options
  options(bitmapType = "cairo")
  options(error = function() traceback(3))
  
  #seed
  set.seed(123)
}

trimCor <- function(dataList, outPath){
  stored <- readRDS(dataList)
  cleaned <- as.numeric(unlist(stored[1]))
  
  saveRDS(cleaned, outPath)
}

#args----
parser <- ArgumentParser(description= 'snakemake transfer')

parser$add_argument("--inPath")
parser$add_argument("--outPath")
snake <- parser$parse_args()
print(str(snake))

#call----
trimCor(snake$inPath,
        snake$outPath)




