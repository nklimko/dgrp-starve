#!/usr/bin/env Rscript

#setup----
if(TRUE){
  library(data.table)
  library(doParallel)
  library(doRNG)
  library(dplyr)
  library(argparse)
  library(mr.mash.alpha)
  
  options(error = function() traceback(3))
  set.seed(123)
  
}

#test----
if(FALSE){
  inPath <- 'data/11_multimatch/f_sr.top3.Rds'
  genPath <- 'srtop3_junk.Rds'
  resPath <- 'asjkknd.Rds'
  cores <- 1
  tol <- 1e-2
  convergence_criterion <- 'ELBO'
  w0_threshold <- 0
  compute_ELBO <- 1
  verbose <- 1
  hetStep <- 0.1
  null_weight <- 0.8
  traitCount <- 4
  i <- 1
  
  covMash(inPath,
          genPath,
          resPath,
          tol,
          convergence_criterion,
          w0_threshold,
          compute_ELBO,
          verbose,
          hetStep,
          null_weight,
          traitCount)
  
}

#function----
covMash <- function(inPath,
                    genPath,
                    resPath,
                    tol,
                    convergence_criterion,
                    w0_threshold,
                    compute_ELBO,
                    verbose,
                    hetStep,
                    null_weight,
                    traitCount) {
  
  print(null_weight)
  print(traitCount)
  
  yX <- na.omit(readRDS(inPath))
  
  y <- yX[,1:traitCount]
  X <- yX[,-(1:traitCount)]
  
  print(paste0('dimy: ', dim(y), 'dimx: ', dim(X)))
  
  Xtrain <- scale(X)
  Ytrain <- matrix(unlist(y), ncol=traitCount)
  
  print('begin')
  #calculate S0
  #het grid lowered from step size of 0.25 to 0.1, 11 vs 5
  # Added hetStep param to snakefile
  S0_base <- compute_canonical_covs(dim(Ytrain)[2],singletons = TRUE, hetgrid = seq(0,1,hetStep))
  univ_sumstats <- compute_univariate_sumstats(Xtrain, Ytrain, standardize = TRUE, mc.cores = 4)
  scaling_grid <- autoselect.mixsd(univ_sumstats, mult = sqrt(2))^2
  S0 <- expand_covs(S0_base, scaling_grid)
  
  #80% set to zero by null weight
  #null_weight <- null_weight # redundant
  non_null_weight <- 1 - null_weight
  w0_init <- c(null_weight, rep(non_null_weight/(length(S0)-1), (length(S0)-1)))
  
  fit <- mr.mash(X=Xtrain,Y=Ytrain, S0=S0, w0=w0_init, tol=tol,
                 convergence_criterion=convergence_criterion,
                 w0_threshold=w0_threshold, compute_ELBO=compute_ELBO,
                 verbose=verbose, update_V=TRUE, nthreads = 8)
  
  covGen <- cov2cor(fit$G)
  covRes <- cov2cor(fit$V)
  
  saveRDS(covGen, genPath)
  saveRDS(covRes, resPath)
  
  
}

#args----
parser <- ArgumentParser(description= 'snakemake transfer')

parser$add_argument('--input')
parser$add_argument('--genPath')
parser$add_argument('--resPath')
parser$add_argument('--tol', type='double')
parser$add_argument('--convergence_criterion')
parser$add_argument('--w0_threshold', type='double')
parser$add_argument('--compute_ELBO', type='integer')
parser$add_argument('--verbose', type='integer')
parser$add_argument('--hetStep', type='double')
parser$add_argument('--null_weight', type='double')
parser$add_argument('--traitCount', type='integer')

snake <- parser$parse_args()
print(str(snake))

#call----
covMash(snake$input,
        snake$genPath,
        snake$resPath,
        snake$tol,
        snake$convergence_criterion,
        snake$w0_threshold,
        snake$compute_ELBO,
        snake$verbose,
        snake$hetStep,
        snake$null_weight,
        snake$traitCount)

