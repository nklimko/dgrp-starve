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

tidyMean <- function(dataList, outPath){
  df <- dataList %>% map_dfr(readRDS)
  saveRDS(df, outPath)
}

#args----
parser <- ArgumentParser(description= 'snakemake transfer')

parser$add_argument("--dataList", nargs='+')
parser$add_argument("--outPath")
snake <- parser$parse_args()
print(str(snake))

#call----
tidyMean(snake$dataList,
         snake$outPath)

