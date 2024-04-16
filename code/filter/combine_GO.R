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
corSummary <- function(dataList, outPath){
  
  hold <- readRDS(dataList[1])
  
  for (i in 2:length(dataList)) {
    print(i)
    hold <- rbind(hold, readRDS(dataList[i]))
  }
  
  colnames(hold) <- c('sex', 'rmax', 'rgo', 'term', 'cor')
  
  saveRDS(hold, outPath)
  
}

#args----
parser <- ArgumentParser(description= 'snakemake transfer')

parser$add_argument('--dataList', nargs='+')
parser$add_argument('--outPath')

snake <- parser$parse_args()
print(str(snake))

#call----
corSummary(snake$dataList,
           snake$outPath)



  
rule fixthis:
  input:
    expand('data/go/24_goCor/f/0.8/0.01/{goterm}/bayesC_goset.Rdsdata/go/24_goCor/m/0.8/0.01/GO.0097602', 
    
    from the main directory:
    ./
    find ./GO.*/*.Rdsdata/* -type f -name '*.Rds' -execdir mv -i {} ../../../../../../.. \;
    
    temp <- readRDS('data/go/24_goCor/f/0.8/0.01/GO.0048010/bayesC_8.Rds')
    
    while read p; do
  echo "$p"
done <peptides.txt