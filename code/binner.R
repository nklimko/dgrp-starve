#!/usr/bin/env Rscript

#setup----
library(data.table)
library(dplyr)
library(argparse)
library(BGLR)

options(error = function() traceback(3))
set.seed(123)

#testing----
if(0){
  #hmake
  if(0){
    inPath <- 'data/sr/10_matched/f_starvation.Rds'
    goPath <- 'data/sr/03_goterms/f/1.Rds'
    outPath <- 'tempbinner.Rds'
    
    onePath <- "data/bglr/sexf/go0.01/max0.4/term1/id1/bigfit_ETA_1_b.bin"
    twoPath <- "data/bglr/sexf/go0.01/max0.4/term1/id1/bigfit_ETA_2_b.bin"
    
    
    goPath  <- "data/sr/03_goterms/f/1.Rds"
    inPath  <- "data/sr/10_matched/f_starvation.Rds"
    onePath <- "data/bglr/sexf/go0.01/max0.5/term1/id1/bigfit_ETA_1_b.bin"
    outPath <- "data/sr/36_hplot/f/0.5/0.01/1/hplottemp.Rds"
    twoPath <- "data/bglr/sexf/go0.01/max0.5/term1/id1/bigfit_ETA_2_b.bin"
  }
  
  #binner
  if(0){
    B <- B1
    matriX <- WGO
    
    B <- B2
    matriX <- WNON
    
    #plot for rmd page later
    plot(h2_new,type='o',cex=.5,col=4);abline(h=c(0.01,mean(h2_new)),lty=2,col=c(1,2),lwd=2, title('Trace of heritability for GO terms'))
    plot(h2_new)
  }
}

#functions----
binner <- function(y, B, matriX){
  h2_new=rep(NA,nrow(B))
  varU_new=h2_new
  varE_new=h2_new
  for(i in 1:length(h2_new)){
    u=matriX%*%B[i,]
    varU_new[i]=var(u)
    varE_new[i]=var(y-u)
    h2_new[i]=varU_new[i]/(varU_new[i]+varE_new[i])
  }
  return(h2_new)
}

hmake <- function(inPath, goPath, onePath, twoPath, outPath){
  xp <- readRDS(inPath)
  go_ids <- readRDS(goPath)
  B1 <- readBinMat(onePath)
  B2 <- readBinMat(twoPath)
  
  y <- xp[,1]
  X <- xp[,-1]
  W <- scale(X)
  
  WGO <- W[,go_ids]
  WNON <- W[,-go_ids]
  
  h2_GO <- binner(y, B1, WGO)
  h2_NON <- binner(y, B2, WNON)
  
  final <- cbind(h_go=h2_GO, h_non=h2_NON)
  
  saveRDS(final, outPath)
  
}

#args----
parser <- ArgumentParser(description= 'snakemake transfer')

parser$add_argument('--inPath')
parser$add_argument('--goPath')
parser$add_argument('--onePath')
parser$add_argument('--twoPath')
parser$add_argument('--outPath')

snake <- parser$parse_args()
print(str(snake))

#call----
hmake(snake$inPath,
      snake$goPath,
      snake$onePath,
      snake$twoPath,
      snake$outPath)

