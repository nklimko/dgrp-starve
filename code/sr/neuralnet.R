#!/usr/bin/env Rscript

#setup----
if(TRUE){
  library(data.table)
  library(dplyr)
  library(argparse)
  
  library(neuralnet)
  
  options(error = function() traceback(3))
  set.seed(123)
}

#setup----
if(0){
  inPath <- 'data/sr/10_matched/f_starvation.Rds'
  idPath <- 'data/top3/02_ids/m/ids_30.Rds'
  
}

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

neuralink <- function(inPath, idPath, neurons, outPath){
  
  yX <- na.omit(readRDS(inPath))
  test_IDs <- readRDS(idPath)
  
  maxmindf <- as.data.frame(lapply(yX, normalize))
  
  # TRAINING AND TEST DATA
  trainset <- maxmindf[-test_IDs, ]
  testset <- maxmindf[test_IDs, ]
  
  
  print('running')
  runtime <- system.time(nn <- neuralnet(trait ~ ., data=trainset, hidden=neurons))
  
  print(nn$result.matrix)
  
  Ypred <- as.vector(predict(nn, testset[,-1]))
  
  Ytest <- testset[,1]
  
  corResult <- cor(Ytest, Ypred)
  final <- list(cor=corResult, time=runtime)
  
  saveRDS(final, outPath)
  
}

#args----
parser <- ArgumentParser(description= 'snakemake transfer')

parser$add_argument("--inPath")
parser$add_argument("--idPath")
parser$add_argument("--neurons", type='integer')
parser$add_argument("--outPath")

snake <- parser$parse_args()
print(str(snake))

#call----
neuralink(snake$inPath,
          snake$idPath,
          snake$neurons,
          snake$outPath)



