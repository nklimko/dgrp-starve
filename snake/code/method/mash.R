#!/usr/bin/env Rscript

#setup----
if(TRUE){
  library(data.table)
  library(doParallel)
  library(doRNG)
  library(dplyr)
  library(argparse)
  library(mashr)
  library(mr.mash.alpha)
  
  options(error = function() traceback(3))
  set.seed(123)
}

#test----
if (FALSE) {
  inPath1 <- 'data/1_matched/m_starvation.Rds'
  inPath2 <- 'data/1_matched/f_starvation.Rds'
  outPath <- 'data/temp_bglr_m_starvation.Rds'
  cores <- 1
  loopIter <- 2
  type <- 'male'
  start <- Sys.time()
  
  homemadeMash(inPath1,
               inPath2,
               outPath,
               cores,
               loopIter)
  
  stop <- Sys.time() - start
  
  stop
}

#function----
homemadeMash <- function(inPath,
                         idPath,
                         outPath,
                         cpus,
                         traitCount,
                         tol,
                         convergence_criterion,
                         w0_threshold,
                         compute_ELBO,
                         verbose,
                         hetStep,
                         null_weight) {
  
  yX <- na.omit(readRDS(inPath))
  
  y <- yX[,1:traitCount]
  X <- yX[,-(1:traitCount)]
  
  ids <- readRDS(idPath)
  
  test_IDs <- ids
  
  Xtrain <- scale(X[-test_IDs,])
  Ytrain <- matrix(unlist(y[-test_IDs]), ncol=traitCount)
  
  Xtest <- scale(X[test_IDs,])
  Ytest <- matrix(unlist(y[test_IDs]), ncol=traitCount)
  
  
  S0_canon <- compute_canonical_covs(dim(Ytrain)[2], singletons = TRUE, hetgrid = seq(0,1,hetStep))
  univ_sumstats <- compute_univariate_sumstats(Xtrain, Ytrain, standardize = TRUE, mc.cores=cpus)
  
  scaling_grid <- autoselect.mixsd(univ_sumstats, mult = sqrt(2))^2
  
  S0 <- expand_covs(S0_canon, scaling_grid)
  
  #80% set to zero
  #null_weight <- null_weight
  non_null_weight <- 1-null_weight
  w0_init <- c(null_weight, rep(non_null_weight/(length(S0)-1), (length(S0)-1)))
  
  
  runtime <- system.time(fit <- mr.mash(X=Xtrain,Y=Ytrain, S0=S0, w0=w0_init, tol=tol,
                                        convergence_criterion=convergence_criterion,
                                        w0_threshold=w0_threshold, compute_ELBO=compute_ELBO,
                                        verbose=verbose, update_V=TRUE, nthreads=cpus)
  )
  
  Ypred <- predict(fit, Xtest)
  
  corResult <- cor(Ytest, Ypred)
  final <- list(cor=corResult, time=runtime)
  
  saveRDS(final, outPath)
  
}




#args----
parser <- ArgumentParser(description= 'snakemake transfer')

parser$add_argument("--inPath")
parser$add_argument("--idPath")
parser$add_argument("--outPath")
parser$add_argument('--cpus', type='integer')
parser$add_argument('--traitCount', type='integer')

#mashing
parser$add_argument('--tol', type='double')
parser$add_argument('--convergence_criterion')
parser$add_argument('--w0_threshold', type='double')
parser$add_argument('--compute_ELBO', type='integer')
parser$add_argument('--verbose', type='integer')
parser$add_argument('--hetStep', type='double')
parser$add_argument('--null_weight', type='double')
snake <- parser$parse_args()
print(str(snake))

#call----
homemadeMash(snake$inPath,
             snake$idPath,
             snake$outPath,
             snake$cpus,
             snake$traitCount,
             snake$tol,
             snake$convergence_criterion,
             snake$w0_threshold,
             snake$compute_ELBO,
             snake$verbose,
             snake$hetStep,
             snake$null_weight)
