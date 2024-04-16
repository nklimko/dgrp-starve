#!/usr/bin/env Rscript

#setup----
if(1){
  library(data.table)
  library(dplyr)
  library(argparse)
  library(pls)

  options(error = function() traceback(3))
  set.seed(123)
}

if(0){
  inPath='snake/data/01_matched/m_starvation.Rds'
  idPath='snake/data/02_ids/m/ids_1.Rds'
  method='pcr'
  method='widekernelpls'
  nfolds=5
  nopts=8
  outPath = 'deleteThis.Rds'

}


#function----
prince <- function(inPath, idPath, method, nopts, nfolds, outPath){


  if(1){

    yX <- na.omit(readRDS(inPath))
    ids <- readRDS(idPath)

    yXtrain <- yX[-ids,]

    Xtest <- yX[ids, -1]
    Ytest <- yX[ids, 1]

    bnl <- dim(yXtrain)[1]

    cvSegs <- cvsegments(bnl, nfolds)
  }

  if(method=='pcr'){
    print(paste0('PCR Method : ', method))
    runtime <- system.time(fitPCA <- pcr(trait ~., data = yXtrain, validation = "CV", segments=5))
    runtime <- system.time(fitPCAloo <- pcr(trait ~., data = yXtrain, validation = "LOO"))
  }else{
    print(paste0('PLS Method : ', method))#, maxit=5000
    runtime <- system.time(fitPCA_pls <- plsr(trait ~., data = yXtrain, validation = "CV", segments=cvSegs, method='widekernelpls', maxit=5000))
  }

  selectNcomp(fitPCA, plot=TRUE, method='onesigma')


  fitPCA_pls9 <- plsr(trait ~., data = yXtrain, validation = "CV", segments=cvSegs, method='widekernelpls', maxit=5000, ncomp=9)

  allCount   <- predict(fitPCA_pls, newdata=Xtest, ncomp=9)
  cor(Ytest, data.table(allCount))

  nineCount   <- predict(fitPCA_pls9, newdata=Xtest, ncomp=9)
  cor(Ytest, data.table(nineCount))

summary(fitPCA)
summary(fitPCA_pls)
summary(fitPCAloo)

  selectNcomp(fitPCA, plot=TRUE, method='onesigma')
  selectNcomp(fitPCA_pls, plot=TRUE, method='onesigma')
  selectNcomp(fitPCAloo, plot=TRUE, method='onesigma')


  str(fitPCA)
  str(fitPCA_pls)




    allCount   <- predict(fitPCA_pls, newdata=Xtest, ncomp=0)
  cor(Ytest, data.table(allCount))

  nullMean <- mean(unlist(yXtrain[,1]))
  cor(rep(nullMean, 19), Ytest)

  if(selectionProcess){


    temp <- cbind(1:3,c(1,1,1))

    temp2 <- data.table(matrix(1:6, ncol=2))


  }




  eightCount <- predict(fitPCA, newdata=Xtest, ncomp=nopts)
  allCount   <- predict(fitPCA, newdata=Xtest, ncomp=fitPCA$ncomp)

  #cor(fitPCA$y[ids], Ytest)

  corResult <- cor(Ytest, allCount)
  corAlternate <- cor(Ytest, eightCount)

  final <- list(cor=corResult, time=runtime, alt=corAlternate)

  saveRDS(final, outPath)

}

#args----
parser <- ArgumentParser(description= 'snakemake transfer')

parser$add_argument("--inPath")
parser$add_argument("--idPath")
parser$add_argument("--method")
parser$add_argument("--nopts", type="integer")
parser$add_argument("--nfolds", type="integer")
parser$add_argument("--outPath")
snake <- parser$parse_args()
print(str(snake))

#call----
prince(snake$inPath,
       snake$idPath,
       snake$method,
       snake$nopts,
       snake$nfolds,
       snake$outPath)


 rmseps <- c(RMSEP(object, "CV")$val) ## includes zero
    maxIdx <- ncomp + 1
    absBest <- which.min(rmseps[seq_len(maxIdx)])

str(fitPCA)


temp <- fitPCA$validation





