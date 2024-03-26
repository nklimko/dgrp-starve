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



if(0){
 
goPath <- "/data2/morgante_lab/nklimko/rep/dgrp-starve/data/fb/go/test/uniqCols"
xpPath <- 'data/sr/10_matched/f_starvation.Rds'
   
}

#functions----

  xp2gene <- function(geneVec, geneMap){
    matching_rows <- geneMap[,1] %in% geneVec
    final <- geneMap[matching_rows, 2]
    return(final)
  }

makeTerms <- function(goPath, xpPath, type){
  
  #readin 2col df of go term and gene
  goData <- read.delim(goPath, sep=' ')
  colnames(goData) <- c('term', 'gene')
  
  #restructure df into list, split genes(col2) by go term(col1)
  splitList <- split(goData[,2], goData[,1])
  
  #readin xpression data
  yX <- readRDS(xpPath)
  X <- data.table(yX[,-1])
  
  #columns of xp data are used to index genes
  geneCols <- colnames(X)
  geneMap <- cbind(gene=geneCols, index=as.numeric(1:length(geneCols)))
  
  #map genes to col index for every go term set
  growth <- lapply(splitList, xp2gene, geneMap=geneMap)
  
  #index all terms with less than five genes associated and subset
  ids.to.remove <- sapply(growth, function(i) length(i) < 5)
  longmire <- growth[!ids.to.remove]
  
  #tekkit
  #
  for(i in 1:length(longmire)){
    #grab term name from list variable  and replace : with . for filename reasons
    term <- names(longmire)[i]
    term <- chartr(':', '.', term)
    #grab ids as numeric index
    ids <- as.numeric(longmire[[i]])
    #custom filename for every goterm set
    filename <- paste0("data/go/03_goterms/", "sex",type, "/", term, '.Rds')
    saveRDS(ids, file=filename)
  }
  
}


#args----
parser <- ArgumentParser(description= 'snakemake transfer')

parser$add_argument('--goPath')
parser$add_argument('--xpPath')
parser$add_argument('--type')

snake <- parser$parse_args()
print(str(snake))

#call----
makeTerms(snake$goPath,
           snake$xpPath,
           snake$type)


