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
  
  #goPath <- "/data2/morgante_lab/nklimko/rep/dgrp-starve/data/fb/go/test/uniqCols"
  #xpPath <- 'data/sr/10_matched/f_starvation.Rds'
  goPath<-"data/go/01_goIndex/goIndex"
  xpPath<-'data/01_matched/f_starvation.Rds'
  
  sex <- 'f'
  sex <- 'm'
  if(1){
    termPath <- paste0('code/go/enrichment/blup/', sex, '/pointList')
    labelPath <- paste0('code/go/enrichment/blup/', sex, '/geneLabels')
    endPath <- paste0('code/go/enrichment/blup/', sex, '/finalData.Rds')
  }
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

termFinal script:
  grep -R 'GO:' trimHitsM > termFin1M
sed 's/^....//' termFin1M > termFinalM

# make sure terms have colons not periods, affects search
#termList <- c('GO:0045819', 'GO:0033500','GO:0055088')

#HOW WAS TERMFINAL MADE



termList <- read.delim(termPath, sep='\n', header=FALSE)

#termList <- read.delim('goPost/termFinalF', sep='\n', header=FALSE)

termList <- as.vector(unlist(termList))
goPath<-"data/go/01_goIndex/goIndex"
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

final

final[,2]

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

filtHold[,1]

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


geneFinal <- read.delim(labelPath, sep='\n', header = FALSE)
#geneFinal <- read.delim('goPost/geneLabelsF', sep='\n', header = FALSE)

finalData <- cbind(filtHold, geneFinal)
colnames(finalData) <- c('flybase','count','gene')


saveRDS(finalData, endPath)
