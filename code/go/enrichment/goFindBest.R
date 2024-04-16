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
  blupPathF <- 'snake/data/go/40_all/f/blup.Rds'
  blupPathM <- 'snake/data/go/40_all/m/blup.Rds'
  bayesPathF <- 'snake/data/go/40_all/sexf/bayesFREEformatted.Rds'
  bayesPathM <- 'snake/data/go/40_all/sexm/bayesFREEformatted.Rds'
  
  topSelect <- function(dataPath, outPath){
    
    core <- readRDS(dataPath)
    data <- core[order(-cor), term] 
    cutoff <- as.integer(length(data) * 0.05)
    final <- data[1:cutoff]
    subbed <- sub(pattern='.', replacement=':', x=final, fixed=TRUE) # replace . with : for all terms
    
    write(subbed, file=outPath)
  }
  
  topSelect(blupPathF,  'code/go/enrichment/f/blup/topHits.txt')
  topSelect(blupPathM,  'code/go/enrichment/m/blup/topHits.txt')
  topSelect(bayesPathF, 'code/go/enrichment/f/bayes/topHits.txt')
  topSelect(bayesPathM, 'code/go/enrichment/m/bayes/topHits.txt')
}


goFindBest <- function(method, sex){
  
  goPath<-'/data2/morgante_lab/nklimko/rep/dgrp-starve/snake/data/go/01_goIndex/goIndex'
  xpPath<-'/data2/morgante_lab/nklimko/rep/dgrp-starve/snake/data/01_matched/f_starvation.Rds'
  
  root <- '/data2/morgante_lab/nklimko/rep/dgrp-starve/code/go/enrichment'
  
  #termPath <- paste0(root, 'code/go/enrichment/', method, '/', sex, '/top', num, '/pointList')
  termPath <-     paste0(root, '/', sex, '/', method, '/topHits.txt')
  prePath <-      paste0(root, '/', sex, '/', method, '/preLabels')
  filtHoldPath <- paste0(root, '/', sex, '/', method, '/filtHold.Rds')
  labelPath <-    paste0(root, '/', sex, '/', method, '/geneLabels')
  endPath <-      paste0(root, '/', sex, '/', method, '/finalData.Rds')
  
  ####termFinal script:
  ####  grep -R 'GO:' trimHitsM > termFin1M
  ####sed 's/^....//' termFin1M > termFinalM
  
  # make sure terms have colons not periods, affects search
  #termList <- c('GO:0045819', 'GO:0033500','GO:0055088')
  
  termList <- read.delim(termPath, sep='\n', header=FALSE)
  
  termList <- as.vector(unlist(termList))
  goPath<-'/data2/morgante_lab/nklimko/rep/dgrp-starve/snake/data/go/01_goIndex/goIndex'
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

#args----
parser <- ArgumentParser(description= 'snakemake transfer')

parser$add_argument('--method')
parser$add_argument('--sex')

snake <- parser$parse_args()
print(str(snake))

goFindBest(snake$method,
           snake$sex)


if(0){
  
  #might integrate upper filtering into goFindBest main framework.
  
  a <- readRDS('code/go/enrichment/f/blup/filtHold.Rds')
  b <- readRDS('code/go/enrichment/m/blup/filtHold.Rds')
  c <- readRDS('code/go/enrichment/f/bayes/filtHold.Rds')
  d <- readRDS('code/go/enrichment/m/bayes/filtHold.Rds')
  
  
  #.     readin(a, 129/131, outfile
  percentile <- function(data, basecount, cutoff, output){
    
    pcent <- data[,count] / basecount * 100 
    
    midway <- data.table(data, percent=pcent)
    
    clipped <- midway[which(pcent > cutoff),]
    return(clipped)
    
  }
  
  blupF <- percentile(a, 131, 5, 'temp')
  blupM <- percentile(b, 129, 5, 'temp')
  bayesF <- percentile(c, 131, 5, 'temp')
  bayesM <- percentile(d, 129, 5, 'temp')
  
  dim(blupF)
  dim(blupM)
  dim(bayesF)
  dim(bayesM)
  
  geneWrite <- function(data, outPath){
    write(unlist(data[,1]), file=outPath) 
  }
  
  geneWrite(blupF, 'code/go/enrichment/f/blup/topLabels.txt')
  geneWrite(blupM, 'code/go/enrichment/m/blup/topLabels.txt')
  geneWrite(bayesF, 'code/go/enrichment/f/bayes/topLabels.txt')
  geneWrite(bayesM, 'code/go/enrichment/m/bayes/topLabels.txt')
  
}

#new interactive runs code above, saves sed step
#touch {f,m}/{bayes,blup}/geneLabels

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


#Final Step
if(0){
  
  root <- 'code/go/enrichment/m/bayes/'
  
  finalTable <- function(root){
    labelPath <- paste0(root, 'geneLabels')
    dataPath <- paste0(root, 'filtHold.Rds')
    outPath <- paste0(root, 'finalData.Rds')
    
    geneFinal <- read.delim(labelPath, sep='\n', header = FALSE)
    data <- readRDS(dataPath)
    
    len <- dim(geneFinal)[1]
    
    finalData <- data.table(index = 1:len, gene=geneFinal[,1], flybase=data[1:len, gene], count=as.numeric(data[1:len, count]))
    #colnames(finalData) <- c('index', 'gene', 'flybase', 'count')
    saveRDS(finalData, outPath)
    
  }
  
  
  finalTable('code/go/enrichment/f/blup/')
  finalTable('code/go/enrichment/m/blup/')
  finalTable('code/go/enrichment/f/bayes/')
  finalTable('code/go/enrichment/m/bayes/')
  
  #temp <- readRDS('code/go/enrichment/f/blup/finalData.Rds')
  
  
  
}

# #quarantine
# if(0){
#   geneFinal <- read.delim(labelPath, sep='\n', header = FALSE)
#   #geneFinal <- read.delim('goPost/geneLabelsF', sep='\n', header = FALSE)
#   #https://www.biostars.org/p/70821/
#   filtHold <- readRDS(filtHoldPath)
#   
#   finalData <- cbind(filtHold, geneFinal)
#   colnames(finalData) <- c('flybase','count','gene')
#   saveRDS(finalData, endPath)
# }