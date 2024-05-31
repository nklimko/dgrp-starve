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
colReader <- function(dataList, outPath){
  
  #read in all data
  colsRead <- lapply(dataList, readRDS)
  
  #bind using dplyr binds cols to condense list to data.table)
  data <- data.table(bind_cols(colsRead))
  
  #columns to set as factors
  ##change cor from char to numeric type
  #facs <- c('sex','rmax','rgo')
  #data[, (facs) := lapply(.SD, as.factor), .SDcols=facs]
  #data[, cor := as.numeric(cor)]
  
  saveRDS(data, outPath)
  
  #data[order(-cor),]
  
}

#args----
parser <- ArgumentParser(description= 'snakemake transfer')

parser$add_argument('--dataList', nargs='+')
parser$add_argument('--outPath')

snake <- parser$parse_args()
print(str(snake))

#call----
colReader(snake$dataList,
          snake$outPath)
