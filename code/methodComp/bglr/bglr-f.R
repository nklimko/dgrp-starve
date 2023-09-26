setwd("/data2/morgante_lab/nklimko/rep/dgrp-starve")

#regular
library(dplyr)
library(data.table)
#library(ggplot2)
#library(cowplot)
library(qqman)
library(doParallel)

#models tested
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
xp_f <- fread("data/xp-f.txt")
# xp_m <- fread("data/xp-m.txt")

#create matrix of only gene expression, trims line and starvation
X <- as.matrix(xp_f[,3:11340])
rownames(X) <- xp_f[,line]
W <- scale(X)

y_temp <- xp_f[,starvation]
dat <- data.frame(id=xp_f[,line], y=y_temp)
y <- adjustPheno(dat, "starvation")

ids <- readRDS("data/id_bank.Rds")

bglrLoop <- foreach(i=1:iter) %dopar% {
  
  #result holder
  corResult <- rep(0, 5)
  
  #setup train and test sets with trait vectors
  #test_IDs <- sample(1:n, as.integer(n / fold))
  test_IDs <- unlist(ids[i])
  
  #copy vector
  y_train <- y
  
  #scrub data for prediction
  y_train[test_IDs] <- NA
  
  #W is the matrix of line by genes
  #y must have NAs only
  
  ### BGLR
  fitBGLR <- BGLR::BGLR(y_train, response_type = "gaussian", ETA = list(list(X=W, model="BayesC")), nIter = 130000, burnIn = 30000, thin = 50)
  #fitBGLR <- BGLR::BGLR(y_train, response_type = "gaussian", ETA = list(list(X=W, model="BayesC")), nIter = 10, burnIn = 2)
  
  #y_calc <- W_test %*% fitBGLR$ETA[[1]]$b + fitBGLR$mu
  
  refill <- predict(fitBGLR)
  corResult[5] <- cor(y[test_IDs], refill[test_IDs])
  
  #Overall result
  corResult
  
}

saveRDS(bglrLoop, "/data2/morgante_lab/nklimko/rep/dgrp-starve/data/bglr-f-130k.rds")

