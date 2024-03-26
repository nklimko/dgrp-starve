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

#Parallel core count
registerDoParallel(cores = 8)

#ggplot holder list
gg <- vector(mode='list', length=12)

# result storage elements
fit_greml <- vector(mode='list', length=iter)
fit_gbayesC <- vector(mode='list', length=iter)
fit_varbvs <- vector(mode='list', length=iter)
fit_glmnet <- vector(mode='list', length=iter)

#wolb infection and inversion status data with phenotype adjustment function
load("/data2/morgante_lab/data/dgrp/misc/adjustData.RData")

dataFlag <- TRUE

if(dataFlag){
 
  
  #expression data matched to line and starvation phenotype
  xp_f <- fread("data/xp-f.txt")
  #xp_m <- fread("data/xp-m.txt")
  
  #setwd("C:/Users/noahk/OneDrive/Desktop/amogus")
  #getwd()
  
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




corLoop <- foreach(i=1:iter) %dopar% {

#Linear Header
#for(i in 1:iter){
  
  corResult <- (1:4)
  
  
  
  #setup train and test sets with trait vectors
  test_IDs <- sample(1:n, as.integer(n / fold))
  
  W_train <- W[-test_IDs,]
  W_test <- W[test_IDs,]
  y_train <- y[-test_IDs]
  y_test <- y[test_IDs]
  
  
  ### GREML, qgg package
  fitGreml <- qgg::greml(y=y, X=mu, GRM=list(A=TRM), validate = matrix(test_IDs,ncol=1), verbose=FALSE)
  
  #Store coeff directly
  fit_greml[[i]] <- fitGreml$accuracy$Corr
  
  corResult[1] <- fitGreml$accuracy$Corr

  
  
  ### GBAYES-C
  fitC <- qgg::gbayes(y=y_train, W=W_train, method="bayesC", scaleY=FALSE, nit=10000, nburn=5000)
  
  # expected/calculated value for y_test
  # \hat{y}_test = W_{test} * \hat{b} + \hat{mu}
  y_calc <- W_test %*% fitC$b + mean(y_train)
  
  # store coeff
  fit_gbayesC[[i]] <- cor(y_test, y_calc)
  
  corResult[2] <- cor(y_test, y_calc)
  
  ### VARBVS
  fitVarb <- varbvs::varbvs(X = W_train, NULL, y=y_train, family = "gaussian", logodds=seq(-3.5,-1,0.1), sa = 1, verbose=FALSE)
  
  # \hat{y}_test = W_{test} * \hat{b} + \hat{mu}
  y_calc <- W_test %*% fitVarb$beta + mean(y_train)
  
  fit_varbvs[[i]] <- cor(y_test, y_calc)
  
  corResult[3] <- cor(y_test, y_calc)
  
  
  
  
  ### GLMNET  
  fitlm <- glmnet::cv.glmnet(x=W_train, y=y_train, alpha=1)
  

  b_hat <- glmnet::coef.glmnet(fitlm, s="lambda.min")


  y_int <- b_hat[1]

  b_hat <- b_hat[2:length(b_hat)]

  y_calc <- W_test %*% b_hat + y_int

  fit_glmnet[[i]] <- cor(y_test, y_calc)
  
  corResult[4] <- cor(y_test, y_calc)
  
  
  corResult

}


saveRDS(corLoop, "/data2/morgante_lab/nklimko/rep/dgrp-starve/data/corLoop-f.rds")
