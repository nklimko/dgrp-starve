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

#function----
rowReader <- function(dataList, outPath){
  
  #read in all data
  rowsRead <- lapply(dataList, readRDS)
  
  #bind using dplyr binds rows to condense list to data.table)
  data <- data.table(bind_rows(rowsRead))
  
  names(data) <- c('term', 'cor')
  
  data[, cor := as.numeric(cor)]
  
  saveRDS(data, outPath)
  
}

#args----
parser <- ArgumentParser(description= 'snakemake transfer')

parser$add_argument('--dataList', nargs='+')
parser$add_argument('--outPath')

snake <- parser$parse_args()
print(str(snake))

#call----
rowReader(snake$dataList,
          snake$outPath)


