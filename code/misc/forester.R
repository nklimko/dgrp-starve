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


if (TRUE) {
  idPath <- 'data/sr/02_ids/m/ids_18.Rds'
  outPath <- 'goodTest.Rds'
  nIter <- 130000
  burnIn <- 30000
  thin <- 50
  R2 <- 0.9
  saveAt <- 'data/bglr/good_'
  model <- 'BayesC'
  saveVar <- 1
}

#W <- matrix(rnorm(200000), ncol = 1000)
#
#colnames(W) <- as.character(1:ncol(W))
#rownames(W) <- as.character(1:nrow(W))
#
#colnames(W) <- paste0("col",1:ncol(W))
#rownames(W) <- paste0("row",1:nrow(W))
#
#y <- rowSums(W[, 1:20]) + rnorm(nrow(W))

#True  Rating  Corr
#20/20 perfect 0.958
#15/20 good    0.962
#10/20 mid     0.961
#5/20  bad     0.960
#0/20  null    0.960

perfectIDS <- 1:20
goodIDS <- 5:25
midIDS <- 10:30
badIDS <- 15:35
nullIDS <- 20:40
  
setwd("snake/data/sr/02_ids")
saveRDS(goodIDS, 'good_ids.Rds')
saveRDS(midIDS, 'mid_ids.Rds')
saveRDS(badIDS, 'bad_ids.Rds')

#setwd('../../..')
saveRDS(W, 'W.Rds')

X <- W
y <- y

go_ids <- perfectIDS
go_ids <- goodIDS
go_ids <- midIDS
go_ids <- badIDS
go_ids <- nullIDS

if(1){
WGO <- W[,go_ids]
WNON <- W[,-go_ids]
customETA <- list(list(X = WGO, model = model, saveEffects = TRUE), list(X=WNON, model=model, saveEffects = TRUE))
customETA2 <- list(list(X=W, model=model))
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
                           saveAt = saveAt)
)

refill <- predict(fitBGLR)

corResult <- cor(y[test_IDs], refill[test_IDs])

#Overall result
final <- list(cor=corResult, time=runtime)
final$cor
}

if(1){
  runtime <- system.time(fitBGLR <-
                           BGLR::BGLR(
                             y_train,
                             response_type = "gaussian",
                             ETA = customETA2,
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
  final$cor
}

0.9588
final$cor

saveRDS(final, 'data/gobug/good.Rds')
saveRDS(final, outPath)
saveRDS(final, outPath)












setwd("../../..")
#https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5937171/

#True  Rating  Corr
#20/20 perfect 0.958
#15/20 good    0.962
#10/20 mid     0.961
#5/20  bad     0.960
#0/20  null    0.960

