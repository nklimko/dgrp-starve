#!/usr/bin/env Rscript

#setup----
if(TRUE){
  library(data.table)
  library(doParallel)
  library(doRNG)
  library(dplyr)
  library(argparse)
  
  #Principal Component Regression
  library(pls)
  
  options(error = function() traceback(3))
  set.seed(123)
}

#testing----
if(0){
  inPath <- 'data/sr/10_matched/f_starvation.Rds'
  idPath <- 'data/sr/02_ids/f/ids_1.Rds'
  outPath <- 'data/junk.Rds'
}

#function----
prince <- function(inPath, idPath, outPath){
  
  yX <- na.omit(readRDS(inPath))
  
  y <- yX[,1]
  y <- scale(y)
  
  X <- yX[,-1]
  
  ids <- readRDS(idPath)
  
  test_IDs <- ids
  
  Xtrain <- data.frame(scale(X[-test_IDs,]))
  Ytrain <- matrix(unlist(y[-test_IDs,]))
  
  Xtest <- data.frame(scale(X[test_IDs,]))
  Ytest <- matrix(unlist(y[test_IDs,]))
  
  runtime <- system.time(fitPCR <- pcr(Ytrain ~., data = cbind(Xtrain, Ytrain), validation = "CV"))
  
  nopt <- selectNcomp(fitPCR)
  
  print(paste0("Selected nopt: ", nopt,". Ignored, using 6"))
  nopt <- 6
  
  Ypred <- predict(fitPCR, Xtest, ncomp=nopt)
  
  corResult <- cor(Ypred, Ytest)
  
  final <- list(cor=corResult, time=runtime)
  
  saveRDS(final, outPath)
  
  #all prcomp correlations
  if(0){
    scree <- fitPCR$Xvar/sum(fitPCR$Xvar)
    subscree <- scree[1:15]
    
    sumscree <- cumsum(scree)
    
    plot(scree)
    plot(sumscree)
    validationplot(fitPCR, val.type="MSEP")
    validationplot(fitPCR, val.type="R2")
    
    binder <- c(0,0)
    
    for(i in 1:fitPCR$ncomp){
      print(paste0("Working on ",i,"..."))
      Ypred <- predict(fitPCR, Xtest, ncomp=i)
      corFin <- cor(Ypred, Ytest)
      binder <- rbind(binder, c(i,corFin))
    }
    
    real <- binder[-1,]
    
    plot(x=real[,1], y=real[,2], xlab="Principal Component", ylab='Correlation Coefficient')
    
    Ypred <- predict(fitPCR, Xtest, ncomp=i)
    
    
    cumulative <- rep(0, length(scree))
    
    
    
    cumulative[1] <- scree[1]
    
    for(i in 2:length(scree)){
      cumulative[i] <- scree[i]+ cumulative[i-1]
      
    }
    
    cumulative100 <- cumulative * 100
    
    plot(sumscree, xlab="Principal Component", ylab="Total Variance", main = 'PCR Cumulative Variance Plot')
    
    plot(real[1:15])
    
    abline(v=6, col="black", lwd=1, lty=1)
    
    
    
    
    
  }
  
}

#args----
parser <- ArgumentParser(description= 'snakemake transfer')

parser$add_argument("--inPath")
parser$add_argument("--idPath")
parser$add_argument("--outPath")
snake <- parser$parse_args()
print(str(snake))

#call----
prince(snake$inPath,
       snake$idPath,
       snake$outPath)












