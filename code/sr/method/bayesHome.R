#!/usr/bin/env Rscript

#setup----
library(data.table)
library(dplyr)
library(argparse)
library(BGLR)

options(error = function() traceback(3))
set.seed(123)

#function----
center_vec <- function(x) {x - mean(x)}

#params----

if(0){
  #BGLR
  nIter <- 130000
  burnIn <- 30000
  thin <- 50
  R2 <- 0.2
  saveAt <- 'data/bglr/h2_'
  model <- 'BayesC'
  
  #data simulation
  n <- 200
  p1 <- 10
  p2 <- 10000
  p2_causal <- 80
  h2_1 <- 0.2
  h2_2 <- 0.001
  
  
  saveAt <- 'data/bglr/{R2_max}/{R2_GO}/ETA_{1,2}_b.bin'
  
  saveTo <- 'data/bglr/{R2_max}/{R2_GO}/{GO, nonGO}/h2.Rds'
  
  
  
  
  inPath <- 'data/sr/10_matched/f_starvation.Rds'
  idPath <- 'data/sr/02_ids/f/ids_2.Rds'
  goPath <- 'data/sr/03_goterms/f/2.Rds'
  outPath <- 'bayesTest.Rds'
  nIter <- 130000
  burnIn <- 30000
  thin <- 50
  R2_max <- 0.9
  R2_GO <- 0.01
  model <- 'BayesC'
  saveAt <- 'data/bglr/f/falsehood_'
  
  
  
  
  #simulate----
  
  ###Simulate genetic value based on variables in group 1
  X1 <- matrix(rnorm(n*p1), n, p1)
  b1 <- rnorm(p1, sd=1)
  g1 <- drop(X1 %*% b1)
  coeff1 <- sqrt(h2_1) / sd(g1)
  g1 <- center_vec(g1 * coeff1)
  
  ###Simulate genetic value based on variables in group 2
  X2 <- matrix(rnorm(n*p2), n, p2)
  b2 <- c(rnorm(p2_causal, sd=1), rep(0, p2-p2_causal))
  g2 <- drop(X2 %*% b2)
  coeff2 <- sqrt(h2_2) / sd(g2)
  g2 <- center_vec(g2 * coeff2)
  
  ###Simulate residuals
  e <- rnorm(n, sd = sqrt(1 - h2_1 - h2_2))
  
  ###Simulate phenotype
  y <- g1 + g2 + e
}

#bglr call----
homemadeBGLR <- function(inPath,
                         idPath,
                         goPath,
                         outPath,
                         nIter,
                         burnIn,
                         thin,
                         R2_max,
                         R2_GO,
                         model,
                         saveAt){
  xp <- readRDS(inPath)
  
  
  X <- xp[, 2:dim(xp)[2]]
  y <- unlist(xp[, 1])
  
  W <- scale(X)
  
  #go_ids <- seq(1,100,1)
  go_ids <- readRDS(goPath)
  
  WGO <- W[,go_ids]
  WNON <- W[,-go_ids]
  
  R2_nonGO <- R2_max - R2_GO
  
  if(R2_GO==0){
    customETA <- list(list(X = W, model = model, saveEffects=FALSE, R2=R2_max))
  } else{
    customETA <- list(list(X = WGO, model = model, saveEffects=FALSE, R2=R2_GO),
                      list(X = WNON, model = model, saveEffects=FALSE, R2=R2_nonGO))
  }
  
  #read in test/train ids
  ids <- readRDS(idPath)
  
  #copy vector for prediction later
  Ytrain <- y
  
  #remove data for prediction later
  Ytrain[ids] <- NA
  
  print('check #1')
  ### BGLR input model from function list(G=GRM)
  runtime <- system.time(fitBGLR <-
                           BGLR::BGLR(
                             Ytrain,
                             response_type = "gaussian",
                             ETA = customETA,
                             nIter = nIter,
                             burnIn = burnIn,
                             thin = thin,
                             saveAt=saveAt,
                             verbose = FALSE
                           )
  )
  
  refill <- predict(fitBGLR)
  
  corResult <- cor(y[ids], refill[ids])
  
  print('cor:')
  print(corResult)
  
  #Overall result
  #final <- list(cor=corResult, time=runtime, fit=fitBGLR)
  final <- list(cor=corResult, time=runtime) 
  
  saveRDS(final, outPath)
}


#h2 calculation----
if(0){
  # X1
  h2_0 <- 0.2
  #B=readBinMat('data/bglr/h2_ETA_1_b.bin')
  B=readBinMat(h2_goPath)
  h2_new=rep(NA,nrow(B))
  varU_new=h2_new
  varE_new=h2_new
  for(i in 1:length(h2_new)){
    u=X1%*%B[i,]
    varU_new[i]=var(u)
    varE_new[i]=var(y-u)
    h2_new[i]=varU_new[i]/(varU_new[i]+varE_new[i])
  }
  
  # X2
  h2_0 <- 0.001
  B2=readBinMat('data/bglr/h2_ETA_2_b.bin')
  h2_new2=rep(NA,nrow(B))
  varU_new2=h2_new2
  varE_new2=h2_new2
  for(i in 1:length(h2_new2)){
    u2=X2%*%B2[i,]
    varU_new2[i]=var(u2)
    varE_new2[i]=var(y-u2)
    h2_new2[i]=varU_new2[i]/(varU_new2[i]+varE_new2[i])
  }
}

#h2 results----
if(0){
  #mean h2 for each eta
  mean(h2_new)
  mean(h2_new2)
  
  
  sd(h2_new)
  
  sd(h2_new2)
  
  quantile(h2_new, probs = 0.025)
  quantile(h2_new, probs = 0.5)
  quantile(h2_new, probs = 0.975)
  
  quantile(h2_new2, probs = 0.025)
  quantile(h2_new2, probs = 0.5)
  quantile(h2_new2, probs = 0.975)
  
  #plots
  plot(h2_new,type='o',cex=.5,col=4);abline(h=c(h2_0,mean(h2_new)),lty=2,col=c(1,2),lwd=2, title('Trace of heritability for GO terms'))
  plot(h2_new2,type='o',cex=.5,col=4);abline(h=c(h2_0,mean(h2_new2)),lty=2,col=c(1,2),lwd=2, title('Trace of heritability for non-GO terms'))
  
  #true b1 to model b1, etc
  cor(b1, fitBGLR$ETA[[1]]$b)
  cor(b2, fitBGLR$ETA[[2]]$b)
  
  #unsure
  sqrt(var(fitBGLR$ETA[[1]]$b))
  sqrt(var(fitBGLR$ETA[[2]]$b))
  
  hist(h2_new)
  hist(h2_new2)
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
parser$add_argument('--R2_GO', type='double')
parser$add_argument('--R2_max', type='double')
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
             snake$R2_max,
             snake$R2_GO,
             snake$model,
             snake$saveAt)



