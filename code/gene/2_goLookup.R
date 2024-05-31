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

goLookup <- function(goDB, data, output){
  
  termList <- read.delim(data, sep='\n', header=FALSE)
  termList <- as.vector(unlist(termList))
  
  goData <- read.delim(goDB, sep=' ', header=TRUE)
  colnames(goData) <- c('term', 'gene')
  
  database <- data.table(term='term', gene='gene')
  
  for(e in termList){
    hits <- goData[goData$term==e,2]
    hold <- data.table(term=rep(e, length(hits)), gene=hits)
    database <- rbind(database, hold)
  }
  
  query <- unique(database[,2])
  query <- as.vector(unlist(query))
  
  final <- data.table(gene='gene', count=0)
  
  #for unique in final
  #return row count of searching against
  #save all to final table
  for(e in query){
    hits <- database[gene==e,]
    hitCount <- dim(hits)[1]
    rowData <- data.table(gene=e, count=hitCount)
    final <- rbind(final, rowData)
  }
  
  final <- final[-1,]
  final <- final[order(-count),]
  
  saveRDS(final, file=output)
  
}

#args----
parser <- ArgumentParser(description= 'snakemake transfer')

parser$add_argument('--goDB')
parser$add_argument('--data')
parser$add_argument('--output')

snake <- parser$parse_args()
print(str(snake))

goLookup(snake$goDB,
         snake$data,
         snake$output)






