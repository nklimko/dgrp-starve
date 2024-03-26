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
rowReader <- function(dataList, cutoff, allPath, topPath){
  
  #read in all data
  rowsRead <- lapply(dataList, readRDS)
  
  #bind using dplyr binds rows to condense list to data.table)
  data <- data.table(bind_rows(rowsRead))
  
  #columns to set as factors
  #change cor from char to numeric type
  facs <- c('sex','rmax','rgo')
  data[, (facs) := lapply(.SD, as.factor), .SDcols=facs]
  data[, cor := as.numeric(cor)]
  
  saveRDS(data, allPath)
  
  data[order(-cor),]
  
}

#args----
parser <- ArgumentParser(description= 'snakemake transfer')

parser$add_argument('--dataList', nargs='+')
parser$add_argument('--cutoff', type='double')
parser$add_argument('--allPath')
parser$add_argument('--topPath')

snake <- parser$parse_args()
print(str(snake))

#call----
rowReader(snake$dataList,
          snake$cutoff,
          snake$allPath,
          snake$outPath)
