#!/usr/bin/env Rscript

#setup----
if(TRUE){
  library(data.table)
  library(doParallel)
  library(doRNG)
  library(dplyr)
  library(argparse)
  
  library(partykit)
  options(error = function() traceback(3))
  set.seed(123)
}


if(0){
  inPath <- 'data/sr/10_matched/f_starvation.Rds'
  idPath <- 'data/sr/02_ids/f/ids_1.Rds'
}

#ctree(formula, data, subset, weights, na.action = na.pass, offset, cluster, 
#     control = ctree_control(...), ytrafo = NULL, 
#    converged = NULL, scores = NULL, doFit = TRUE, ...)




#function----
inferFitty <- function(inPath,
                       idPath,
                       outPath,
                       ntree){
  
  
  yX <- na.omit(readRDS(inPath))
  
  y <- yX[,1]
  X <- yX[,-1]
  
  ids <- readRDS(idPath)
  
  test_IDs <- ids
  
  
  yXpart <- yX[-test_IDs, ]
  
  
  Xtest <- data.frame(scale(X[test_IDs,]))
  Ytest <- unlist(y[test_IDs])
  
  
  
  
  runtime <- system.time(treeMake <- ctree(formula= trait ~ ., data=yXpart))
  
  
  # Predicting the Test set results
  Ypred <- predict(treeMake, newdata = Xtest)
  
  corResult <- cor(Ytest, Ypred)
  
  final <- list(cor=corResult, time=runtime)
  
  saveRDS(final, outPath)
  
}


#args----
parser <- ArgumentParser(description= 'snakemake transfer')

parser$add_argument("--inPath")
parser$add_argument("--idPath")
parser$add_argument("--outPath")
snake <- parser$parse_args()
print(str(snake))

#call----
inferFitty(snake$inPath,
           snake$idPath,
           snake$outPath)


