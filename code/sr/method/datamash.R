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
if (FALSE) {
  inPath <- 'data/11_multimatch/f_sr.top3.Rds'
  idPath <- 'data/02_ids/ids_1_f_top3.Rds'
  outPath <- 'srtop3_junk.Rds'
  cores <- 1
  iter <- 1
  tol <- 1e-2
  convergence_criterion <- 'ELBO'
  w0_threshold <- 0
  compute_ELBO <- 1
  verbose <- 1
  hetStep <- 0.2
  null_weight <- 0.8
  traitCount <- 4
  start <- Sys.time()
  
  homemadeMash(inPath,
               outPath,
               cores,
               iter,                   
               tol,
               convergence_criterion,
               w0_threshold,
               compute_ELBO,
               verbose,
               hetStep,
               null_weight,
               traitCount)
  
  stop <- Sys.time() - start
  
  stop
}

#function----
homemadeMash <- function(inPath,
                         idPath,
                         outPath,
                         cpus,
                         tol,
                         convergence_criterion,
                         w0_threshold,
                         compute_ELBO,
                         verbose,
                         hetStep,
                         null_weight,
                         traitCount) {
  
  yX <- na.omit(readRDS(inPath))
  
  y <- yX[,1:traitCount]
  X <- yX[,-(1:traitCount)]
  
  ids <- readRDS(idPath)
  
  #loop----
  
  #setup train and test sets with trait vectors
  test_IDs <- ids
  
  Xtrain <- scale(X[-test_IDs,])
  Ytrain <- matrix(unlist(y[-test_IDs]), ncol=traitCount)
  
  Xtest <- scale(X[test_IDs,])
  Ytest <- matrix(unlist(y[test_IDs]), ncol=traitCount)
  
  #calculate S0
  #het grid lowered from step size of 0.25 to 0.1, 11 vs 5
  # Added hetStep param to snakefile
  S0_canon <- compute_canonical_covs(dim(Ytrain)[2], singletons = TRUE, hetgrid = seq(0,1,hetStep))
  univ_sumstats <- compute_univariate_sumstats(Xtrain, Ytrain, standardize = TRUE, mc.cores=cpus)
  
  S0_addon <- compute_data_driven_covs(univ_sumstats)
  
  scaling_grid <- autoselect.mixsd(univ_sumstats, mult = sqrt(2))^2
 
  S0 <- expand_covs(c(S0_canon, S0_addon), scaling_grid)
  
  #80% set to zero
  null_weight <- null_weight # redundant
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

parser$add_argument('--input')
parser$add_argument('--ids')
parser$add_argument('--output')
parser$add_argument('--cpus', type='integer')
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
homemadeMash(snake$input,
             snake$ids,
             snake$output,
             snake$cpus,
             snake$tol,
             snake$convergence_criterion,
             snake$w0_threshold,
             snake$compute_ELBO,
             snake$verbose,
             snake$hetStep,
             snake$null_weight,
             snake$traitCount)

