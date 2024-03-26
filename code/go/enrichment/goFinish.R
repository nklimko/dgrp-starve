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

root <- '/data2/morgante_lab/nklimko/rep/dgrp-starve/snake/'

goPath<-"data/go/01_goIndex/goIndex"
xpPath<-'data/01_matched/f_starvation.Rds'

sex <- 'f'
sex <- 'm'

#trial
if(0){
  method <- 'bayesC'
  sex <- 'f'
  num <- 25
}

goFinish <- function(method, sex, num){
  
  root <- '/data2/morgante_lab/nklimko/rep/dgrp-starve/snake/'
  
  filtHoldPath <- paste0(root, 'code/go/enrichment/', method, '/', sex, '/top', num,  '/filtHold.Rds')
  labelPath <- paste0(root, 'code/go/enrichment/', method, '/', sex, '/top', num,  '/geneLabels')
  endPath <- paste0(root, 'code/go/enrichment/', method, '/', sex, '/top', num,  '/finalData.Rds')
  
  
  
  geneFinal <- read.delim(labelPath, sep='\n', header = FALSE)
  #geneFinal <- read.delim('goPost/geneLabelsF', sep='\n', header = FALSE)
  #https://www.biostars.org/p/70821/
  filtHold <- readRDS(filtHoldPath)
  
  finalData <- cbind(filtHold, geneFinal)
  colnames(finalData) <- c('flybase','count','gene')
  
  saveRDS(finalData, endPath)
  
}


#args----
parser <- ArgumentParser(description= 'snakemake transfer')

parser$add_argument('--method')
parser$add_argument('--sex')
parser$add_argument('--num', type='integer')

snake <- parser$parse_args()
print(str(snake))

goFinish(snake$method,
         snake$sex,
         snake$num)