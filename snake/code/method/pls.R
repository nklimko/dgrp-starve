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


#method descriptions:
#kernelpls: designed for tall matrices of many observations with few variables

#widekernelpls more efficient on wide matrices ie many variables few observations # this is why runtime was best

#oscorepls: orthogonal scoring(classic) : pure NIPALS algorithm, partial least square loading X scores

#SIMPLS: alternate method to calculate partial least square values using linearcombinations of original variables, skips NIPALS algorithm components



#function----
prince <- function(inPath, idPath, outPath, method){
  
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
  
  #runtime <- system.time(fitPCR <- pcr(Ytrain ~., data = cbind(Xtrain, Ytrain), validation = "CV"))
  
  #"kernelpls"), the wide kernel algorithm ("widekernelpls"), SIMPLS ("simpls") and the classical orthogonal scores algorithm ("oscorespls"). One CPPLS algorithm is available ("cppls")
  runtime <- system.time(fitPLS <- plsr(Ytrain ~., method=method, data = cbind(Xtrain, Ytrain), validation = "CV", maxit=5000))
  
  #maxit
  
  nopt <- selectNcomp(fitPLS)
  
  print(paste0("Selected nopt: ", nopt,". Ignored, using 3"))
  nopt <- 3
  
  Ypred <- predict(fitPLS, Xtest, ncomp=nopt)
  
  corResult <- cor(Ypred, Ytest)
  
  final <- list(cor=corResult, time=runtime)
  
  saveRDS(final, outPath)
  
  #all prcomp correlations
  if(0){
    scree <- fitPLS$Xvar/fitPLS$Xtotvar
    subscree <- scree[1:25]
    
    plot(scree)
    plot(subscree)
    validationplot(fitPLS, val.type="MSEP")
    validationplot(fitPLS, val.type="R2")
    
    binder <- c(0,0)
    registerDoParallel(cores=4)
    
    for(i in 1:fitPLS$ncomp){
      print(paste0("Working on ",i,"..."))
      Ypred <- predict(fitPLS, Xtest, ncomp=i)
      corFin <- cor(Ypred, Ytest)
      binder <- rbind(binder, c(i,corFin))
    }
    
    real <- binder[-1,]
    
    plot(x=real[,1], y=real[,2], xlab="Principal Component", ylab='Correlation Coefficient')
    
    Ypred <- predict(fitPLS, Xtest, ncomp=i)
  }
  
}

#args----
parser <- ArgumentParser(description= 'snakemake transfer')

parser$add_argument("--inPath")
parser$add_argument("--idPath")
parser$add_argument("--outPath")
parser$add_argument("--method")
snake <- parser$parse_args()
print(str(snake))

#call----
prince(snake$inPath,
       snake$idPath,
       snake$outPath,
       snake$method)
















