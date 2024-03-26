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

#root <- '/data2/morgante_lab/nklimko/rep/dgrp-starve/snake/'
#
#goPath<-"data/go/01_goIndex/goIndex"
#xpPath<-'data/01_matched/f_starvation.Rds'
#
#sex <- 'f'
#sex <- 'm'

#trial
if(0){
  method <- 'bayesC'
  sex <- 'f'
  num <- 25
}

goFindBetter <- function(method, sex, num){
  
  root <- '/data2/morgante_lab/nklimko/rep/dgrp-starve/snake/'
  
  goPath<-paste0(root, 'data/go/01_goIndex/goIndex')
  xpPath<-paste0(root, 'data/01_matched/f_starvation.Rds')
  
  
  #termPath <- paste0(root, 'code/go/enrichment/', method, '/', sex, '/top', num, '/pointList')
  termPath <- paste0(root, 'code/go/enrichment/', method, '/', sex, '/top', num, '/topHits.txt')
  prePath <- paste0(root, 'code/go/enrichment/', method, '/', sex, '/top', num,  '/preLabels')
  filtHoldPath <- paste0(root, 'code/go/enrichment/', method, '/', sex, '/top', num,  '/filtHold.Rds')
  labelPath <- paste0(root, 'code/go/enrichment/', method, '/', sex, '/top', num,  '/geneLabels')
  endPath <- paste0(root, 'code/go/enrichment/', method, '/', sex, '/top', num,  '/finalData.Rds')
  
  ####termFinal script:
  ####  grep -R 'GO:' trimHitsM > termFin1M
  ####sed 's/^....//' termFin1M > termFinalM
  
  # make sure terms have colons not periods, affects search
  #termList <- c('GO:0045819', 'GO:0033500','GO:0055088')
  
  termList <- read.delim(termPath, sep='\n', header=FALSE)
  
  termList <- as.vector(unlist(termList))
  goPath<-paste0(root, "data/go/01_goIndex/goIndex")
  goData <- read.delim(goPath, sep=' ', header=TRUE)
  colnames(goData) <- c('term', 'gene')
  
  startHold <- data.table(term='term', gene='gene')
  
  for(e in termList){
    #e <- "GO:0000096"
    hits <- goData[goData$term==e,2]
    
    #final <- list(term=e, genes=hits)
    hold <- data.table(term=rep(e, length(hits)), gene=hits)
    
    startHold <- rbind(startHold, hold)
  }  
  
  final <- startHold 
  
  #final
  
  #final[,2]
  
  query <- unique(final[,2])
  
  #for unique in final
  #return row count of searching against
  #save all to final table
  
  
  houseHold <- data.table(gene='gene', count=0)
  
  finalQuery <- as.vector(unlist(query))
  
  for(e in finalQuery){
    hits <- final[gene==e,]
    hitCount <- dim(hits)[1]
    rowData <- data.table(gene=e, count=hitCount)
    houseHold <- rbind(houseHold, rowData)
    
  }
  
  houseHold <- houseHold[order(-count),]
  
  filtHold <- houseHold[(count>2),]
  #print(filtHold[,1])
  
  write(unlist(filtHold[,1]), file=prePath) 
  
  saveRDS(filtHold, file=filtHoldPath)
  
  
  
} 


#interactive steps
#
#copy filt1 printout to nano file
#
#remove filler using:
#sed 's/^....//' temp9 > temp10
#
#copy list to https://www.biotools.fr/drosophila/fbgn_converter
#
#convert fbgn to gene ids
#
#paste back into 'geneLabels'
#copy into file and read file back in


#args----
parser <- ArgumentParser(description= 'snakemake transfer')

parser$add_argument('--method')
parser$add_argument('--sex')
parser$add_argument('--num', type='integer')

snake <- parser$parse_args()
print(str(snake))



goFindBetter(snake$method,
             snake$sex,
             snake$num)


if(0){
  geneFinal <- read.delim(labelPath, sep='\n', header = FALSE)
  #geneFinal <- read.delim('goPost/geneLabelsF', sep='\n', header = FALSE)
  #https://www.biostars.org/p/70821/
  filtHold <- readRDS(filtHoldPath)
  
  finalData <- cbind(filtHold, geneFinal)
  colnames(finalData) <- c('flybase','count','gene')
  
  
  saveRDS(finalData, endPath)
}