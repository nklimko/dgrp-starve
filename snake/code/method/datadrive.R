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

###Function needed
is_strong <- function(x, thresh){
  strong <- any(abs(x) > thresh)
  return(strong)
}


if(0){
  inPath <- 'data/11_multimatch/m_sr.top3.Rds'
  idPath <- 'data/top3/02_ids/m/ids_30.Rds'
  resPath <- 'diagonal'
  resPath <- 'data/22_cov/residual/mr.mash_f_sr.top3.Rds'
  outPath <- 'data/top3/23_paropt/datadrive_m_30.Rds'
  
  #dd
  npcs <- 4
  Zthr <- 3
  flash_remove_singleton <- 1
  ED_algorithm <- "bovy"
  ted_zero_thresh <- 1e-10
  canonical_cov <- 1
  
  #mash
  tol <- 1e-2
  convergence_criterion <- 'ELBO'
  w0_threshold <- 0
  compute_ELBO <- 1
  verbose <- 1
  hetStep <- 0.1
  null_weight <- 0.8
  traitCount <- 4
  cpus <- 4
  
}


#begin----
dataDriver <- function(inPath,
                       resPath,
                       idPath,
                       outPath,
                       cpus,
                       traitCount,
                       npcs,
                       Zthr,
                       flash_remove_singleton,
                       ED_algorithm,
                       ted_zero_thresh,
                       tol,
                       convergence_criterion,
                       w0_threshold,
                       compute_ELBO,
                       verbose,
                       hetStep,
                       null_weight){
  
  yX <- na.omit(readRDS(inPath))
  
  y <- yX[,1:traitCount]
  X <- yX[,-(1:traitCount)]
  
  ids <- readRDS(idPath)
  
  test_IDs <- ids
  
  Xtrain <- scale(X[-test_IDs,])
  Ytrain <- matrix(unlist(y[-test_IDs]), ncol=traitCount)
  
  Xtest <- scale(X[test_IDs,])
  Ytest <- matrix(unlist(y[test_IDs]), ncol=traitCount)
  
  
  registerDoParallel(cpus)
  
  dat <- compute_univariate_sumstats(Xtrain, Ytrain, standardize = TRUE, mc.cores = cpus)
  
  Z <- dat$Bhat/dat$Shat
  
  strong_thresh <- Zthr
  
  strong <- which(apply(Z, 1, is_strong, strong_thresh))
  
  Z_strong <- Z[strong,]
  
  Bhat_strong <- dat$Bhat[strong, ]
  Shat_strong <- dat$Shat[strong, ]
  
  if(nrow(Bhat_strong) < 20){
    print("Too few strong effects with current Z threshold.")
  }
  
  ###Estimate data-driven prior
  if(resPath=="diagonal"){
    V <- diag(ncol(Bhat_strong))
  } else {
    V <- readRDS(resPath)
  }
  
  
  #Estimate V using weak effects
  Z_weak <- Z[-strong, ]
  
  Vhat <- matrix(0, nrow=ncol(Z_weak), ncol=ncol(Z_weak))
  
  for(j in 1:nrow(Z_weak)){
    Vhat <- Vhat + tcrossprod(Z_weak[j,])
  }
  
  Vhat <- Vhat/nrow(Z_weak)
  print(cov2cor(Vhat))
  
  dat_mash <- mash_set_data(Z_strong, V=cov2cor(Vhat))
  
  if(ED_algorithm=="bovy"){
    ##Compute factor analyses
    U_pca <- cov_pca(data=dat_mash, npc=npcs)
    U_flash_default <- cov_flash(data=dat_mash, factors="default", tag="default",
                                 remove_singleton=flash_remove_singleton, output_model=NULL)
    # U_flash_nonneg <- cov_flash(data=dat_mash, factors="nonneg", tag="nonneg",
    #                             remove_singleton=flash_remove_singleton, output_model=NULL)
    U_emp <- crossprod(Z_strong) / nrow(Z_strong)
    
    ##De-noise data-driven matrices via extreme deconvolution
    U_datadriven <- c(list(BB=U_emp), U_pca)
    
    if(!is.null(U_flash_default)){
      U_datadriven <- c(U_datadriven, U_flash_default)
    }
    
    # if(!is.null(U_flash_nonneg)){
    #   U_datadriven <- c(U_datadriven, U_flash_nonneg)
    # }
    
    ##HARDWIRE TRUE
    canonical_cov <- 1
    if(canonical_cov){
      U_can <- cov_canonical(dat_mash)
      U_datadriven <- c(U_datadriven, U_can)
    }
    
    U_ed <- cov_ed(dat_mash, U_datadriven)
  } else if(ED_algorithm=="ted"){
    library(udr)
    # 1. Initialize 50 unconstrained covariance matrices for udr.
    R <- nrow(V)
    K <- 50
    
    # 2. Add small amount of penalty(inverse-Wishart), strength = R. When sample size is large, 
    # the penalty amount R is small.
    fit0 <- ud_init(dat_mash, n_unconstrained = K)
    fit1 <- ud_fit(fit0, control = list(unconstrained.update = "ted", lambda = R, penalty.type = "iw",                                      maxiter=5000, tol = 1e-2, tol.lik = 1e-3), verbose=TRUE,
                   zero.threshold=ted_zero_thresh)
    
    U_ed <- fit1$U
  }
  
  
  ###Save file - end of data driving - must integrate to mash
  #  saveRDS(U_ed, outPath)
  
  print(paste0("VVVVV Data driving complete VVVVV"))
  
  
  #calculate S0
  #het grid lowered from step size of 0.25 to 0.1, 11 vs 5
  # Added hetStep param to snakefile
  #S0_canon <- compute_canonical_covs(dim(Ytrain)[2], singletons = TRUE, hetgrid = seq(0,1,hetStep))
  #univ_sumstats <- compute_univariate_sumstats(Xtrain, Ytrain, standardize = TRUE, mc.cores=cpus)
  
  univ_sumstats <- dat
  #S0_addon <- U_ed
  S0_all <- U_ed
  
  scaling_grid <- autoselect.mixsd(univ_sumstats, mult = sqrt(2))^2
  
  #S0 <- expand_covs(c(S0_canon, S0_addon), scaling_grid)
  S0 <- expand_covs(S0_all, scaling_grid)
  
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
parser$add_argument("--resPath")
parser$add_argument("--ids")
parser$add_argument("--outPath")
parser$add_argument('--cpus', type='integer')
parser$add_argument('--traitCount', type='integer')

#data driving
parser$add_argument("--npcs", type="integer")
parser$add_argument("--Zthr", type="integer")
parser$add_argument("--flash_remove_singleton", type="integer") #bool
parser$add_argument("--ED_algorithm")
parser$add_argument("--ted_zero_thresh", type="numeric")
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
dataDriver(snake$inPath,
           snake$resPath,
           snake$ids,
           snake$outPath,
           snake$cpus,
           snake$traitCount,
           snake$npcs,
           snake$Zthr,
           snake$flash_remove_singleton,
           snake$ED_algorithm,
           snake$ted_zero_thresh,
           snake$tol,
           snake$convergence_criterion,
           snake$w0_threshold,
           snake$compute_ELBO,
           snake$verbose,
           snake$hetStep,
           snake$null_weight)





