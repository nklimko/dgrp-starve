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

#try again
#data list contains all id replicates of a single set of cards
#find the average from all of them
#save average cor to row with remaining cards
#row is made man
# i give you a little bone and you go look for a dinosaur
#dataList <- c('data/sr/25_goopfish/m/0.4/0.05/1/bayesC_1.Rds', "data/sr/25_goopfish/m/0.4/0.05/1/bayesC_2.Rds")


#function getCor----
getCor <- function(dataPath){
  data <- readRDS(dataPath)
  return(data[1])
}

#function filt----
filt <- function(dataList, outPath){
  
  cor <- sapply(dataList, getCor)
  meanCor <- mean(na.omit(unlist(cor)))
  
  frag <- unlist(strsplit(dataList[1], '/'))
  
  #6 index of filename method
  final <- c(frag[6], meanCor)
  names(final) <- c('method', 'cor')
  
  saveRDS(final, outPath)
  
}







#args----
parser <- ArgumentParser(description= 'snakemake transfer')

parser$add_argument("--inPath", nargs='+')
parser$add_argument("--outPath")
snake <- parser$parse_args()
print(str(snake))

#call----
filt(snake$inPath,
     snake$outPath)



