library(data.table)

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

