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

saveMean <- function(dataList, goterm, outPath){
  allCors <- sapply(dataList, readRDS)
  meanCor <- mean(na.omit(unlist(allCors)))
  
  rowData <- data.table(term=goterm, cor=meanCor)
  
  saveRDS(rowData, outPath)
}

#args----
parser <- ArgumentParser(description= 'snakemake transfer')

parser$add_argument("--dataList", nargs='+')
parser$add_argument("--goterm")
parser$add_argument("--outPath")
snake <- parser$parse_args()
print(str(snake))

#call----
saveMean(snake$dataList,
         snake$goterm,
         snake$outPath)




