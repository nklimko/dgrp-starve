#Nightmarish Bayesian prior on a gene ontology effect + all other effects
#Use BGLR?? iirc
#y= mu + Xb1 + Xb2 + epsilon
#bayes C clab is a normal distribution


#!/usr/bin/env Rscript

#setup----
if(TRUE){
  library(data.table)
  library(doParallel)
  library(doRNG)
  library(dplyr)
  library(argparse)
  library(BGLR)
  
  options(error = function() traceback(3))
  set.seed(123)
}

#test----
if (FALSE) {
  inPath <- 'data/sr/10_matched/m_starvation.Rds'
  idPath <- 'data/sr/02_ids/m/ids_18.Rds'
  goPath <- 'data/sr/03_goterms/f/1238.Rds'
  outPath <- 'bayesTest.Rds'
  nIter <- 1300
  burnIn <- 300
  thin <- 5
  R2 <- 0.5
  saveAt <- 'data/bglr/temp_'
  i <- 1
  model <- 'BayesC'
  
}


#function----
homemadeBGLR <- function(inPath,
                         idPath,
                         goPath,
                         outPath,
                         nIter,
                         burnIn,
                         thin,
                         R2,
                         model,
                         saveAt) {
  xp <- readRDS(inPath)
  
  X <- xp[, 2:dim(xp)[2]]
  y <- unlist(xp[, 1])
  
  W <- scale(X)
  
  #go_ids <- seq(1,100,1)
  go_ids <- readRDS(goPath)
  
  WGO <- W[,go_ids]
  WNON <- W[,-go_ids]
  
  customETA <- list(list(X = WGO, model = model), list(X=WNON, model=model))
  
  test_IDs <- readRDS(idPath)
  
  #copy vector
  y_train <- y
  
  #scrub data for prediction
  y_train[test_IDs] <- NA
  
  ### BGLR input model from function list(G=GRM)
  runtime <- system.time(fitBGLR <-
                           BGLR::BGLR(
                             y_train,
                             response_type = "gaussian",
                             ETA = customETA,
                             nIter = nIter,
                             burnIn = burnIn,
                             thin = thin,
                             verbose = FALSE,
                             R2 = R2,
                             saveAt = saveAt
                           )
  )
  
  refill <- predict(fitBGLR)
  
  corResult <- cor(y[test_IDs], refill[test_IDs])
  
  #Overall result
  final <- list(cor=corResult, time=runtime)
  
  saveRDS(final, outPath)
  
}

#args----
parser <- ArgumentParser(description= 'snakemake transfer')

parser$add_argument('--inPath')
parser$add_argument('--idPath')
parser$add_argument('--goPath')
parser$add_argument('--outPath')
parser$add_argument('--nIter', type='integer')
parser$add_argument('--burnIn', type='integer')
parser$add_argument('--thin', type='integer')
parser$add_argument('--R2', type='double')
parser$add_argument('--model')
parser$add_argument('--saveAt')

snake <- parser$parse_args()
print(str(snake))

#call----
homemadeBGLR(snake$inPath,
             snake$idPath,
             snake$goPath,
             snake$outPath,
             snake$nIter,
             snake$burnIn,
             snake$thin,
             snake$R2,
             snake$model,
             snake$saveAt)








