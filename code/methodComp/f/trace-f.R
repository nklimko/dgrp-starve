setwd("/data2/morgante_lab/nklimko/rep/dgrp-starve")

#regular
library(dplyr)
library(data.table)
library(ggplot2)
library(cowplot)
library(qqman)
library(doParallel)

#models tested
library(qgg)
library(varbvs)
library(glmnet)

#options
options(bitmapType = "cairo")
options(error = function() traceback(3))

#seed
set.seed(1)

#loop count and data limit
iter <- 48

# result storage elements
fit_greml <- vector(mode='list', length=iter)
fit_gbayesC <- vector(mode='list', length=iter)
fit_varbvs <- vector(mode='list', length=iter)
fit_glmnet <- vector(mode='list', length=iter)




dataFlag <- TRUE

if(dataFlag){

 #wolb infection and inversion status data with phenotype adjustment function
load("/data2/morgante_lab/data/dgrp/misc/adjustData.RData")

  
  #expression data matched to line and starvation phenotype
  xp_f <- fread("data/xp-f.txt")
  
 # xp_m <- fread("data/xp-m.txt")
  
  #create matrix of only gene expression, trims line and starvation
  X <- as.matrix(xp_f[,3:11340])
  rownames(X) <- xp_f[,line]
  W <- scale(X)
  
  y_temp <- xp_f[,starvation]
  dat <- data.frame(id=xp_f[,line], y=y_temp)
  y <- adjustPheno(dat, "starvation")

} else{
  
  # Toy Data set, 200x100 matrix
  W <- matrix(rnorm(20000), ncol = 100)
  colnames(W) <- paste0("gene", 1:ncol(W))
  rownames(W) <- paste0("line", 1:nrow(W))
  
  #model uses genes 1:5 and 10:20
  y <- rowSums(W[, 1:5]) + rowSums(W[, 10:20]) + rnorm(nrow(W))

}

### qgg_greml

#model to solve for, vector of ones
mu <- matrix(rep(1, length(y)), ncol=1)
#names(mu) <- paste0("line", 1:length(mu))
rownames(mu) <- xp_f[,line]
TRM <- tcrossprod(W)/ncol(W)

# k-fold parameters
n <- length(y)
fold <- 10

iter <- 48

### sample analysis of gbayesC to show that convergence is working as expected 
test_IDs <- sample(1:n, as.integer(n / fold))
  
  W_train <- W[-test_IDs,]
  W_test <- W[test_IDs,]
  y_train <- y[-test_IDs]
  y_test <- y[test_IDs]
  

  ### GBAYES-C
  
  
  fitC <- qgg::gbayes(y=y_train, W=W_train, method="bayesC", scaleY=FALSE, nit=100000, nburn=30000)
  
  saveRDS(fitC, "/data2/morgante_lab/nklimko/rep/dgrp-starve/data/gbayes_100k-f.Rds")



