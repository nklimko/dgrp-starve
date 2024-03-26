setwd("/data2/morgante_lab/nklimko/rep/dgrp-starve")

#regular
library(dplyr)
library(data.table)
#library(ggplot2)
#library(cowplot)
library(qqman)
library(doParallel)

#models tested
library(qgg)
library(varbvs)
library(glmnet)
library(BGLR)

#options
options(bitmapType = "cairo")
options(error = function() traceback(3))

#seed
set.seed(1)


#loop count and data limit
iter <- 50

#Parallel core count
registerDoParallel(cores = 8)

#wolb infection and inversion status data with phenotype adjustment function
load("/data2/morgante_lab/data/dgrp/misc/adjustData.RData")

#expression data matched to line and starvation phenotype
#xp_f <- fread("data/xp-f.txt")
xp_m <- fread("data/xp-m.txt")

#create matrix of only gene expression, trims line and starvation
X <- as.matrix(xp_m[,3:13577])
rownames(X) <- xp_m[,line]
W <- scale(X)

y_temp <- xp_m[,starvation]
dat <- data.frame(id=xp_m[,line], y=y_temp)
y <- adjustPheno(dat, "starvation")

#model to solve for, vector of ones
mu <- matrix(rep(1, length(y)), ncol=1)
#names(mu) <- paste0("line", 1:length(mu))
rownames(mu) <- xp_m[,line]
TRM <- tcrossprod(W)/ncol(W)

# k-fold parameters
n <- length(y)
fold <- 5

ids <- readRDS("data/id_bank.Rds")

corLoop <- foreach(i=1:iter) %dopar% {
  
  start <- Sys.time()
  #result holder
  corResult <- rep(0, 4)
  
  #setup train and test sets with trait vectors
  #test_IDs <- sample(1:n, as.integer(n / fold))
  test_IDs <- unlist(ids[i])
  
  W_train <- W[-test_IDs,]
  W_test <- W[test_IDs,]
  y_train <- y[-test_IDs]
  y_test <- y[test_IDs]
  
  
  ### QGG::GREML
  fitGreml <- qgg::greml(y=y, X=mu, GRM=list(A=TRM), validate = matrix(test_IDs,ncol=1), verbose=FALSE)
  
  corResult[1] <- fitGreml$accuracy$Corr
  
  ### QGG::GBAYES-C
  #fitC <- qgg::gbayes(y=y_train, W=W_train, method="bayesC", scaleY=FALSE, nit=50000, nburn=20000)
  
   ### VARBVS
  fitVarb <- varbvs::varbvs(X = W_train, NULL, y=y_train, family = "gaussian", logodds=seq(-3.5,-1,0.1), sa = 1, verbose=TRUE)
  
  y_calc <- predict(fitVarb, X=W_test)
  corResult[2] <- cor(y_test, y_calc)
  
  ### GLMNET::LASSO
  fitlm <- glmnet::cv.glmnet(x=W_train, y=y_train, alpha=1)
  
  y_calc <- predict(fitlm, W_test, s="lambda.min")
  corResult[3] <- cor(y_test, y_calc)
  
  #Overall result
  corResult
  
}


saveRDS(corLoop, "/data2/morgante_lab/nklimko/rep/dgrp-starve/data/corLoop-m-Minus.rds")
