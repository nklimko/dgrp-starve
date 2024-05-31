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
filt <- function(dataList, term, outPath){

  corData <- sapply(dataList, getCor)

  trim <- na.omit(unlist(corData))

  meanCor <- mean(trim)
  seCor <- sd(trim) / sqrt(length(trim))

  final <- c(term, meanCor, seCor)
  names(final) <- c('term', 'cor', 'se')

  saveRDS(final, outPath)

}


#args----
parser <- ArgumentParser(description= 'snakemake transfer')

parser$add_argument("--dataList", nargs='+')
parser$add_argument("--term")
parser$add_argument("--outPath")
snake <- parser$parse_args()
print(str(snake))

#call----
filt(snake$dataList,
     snake$term,
     snake$outPath)







#/snake/data/go/24_goCor/m/tblup/GO.2001234