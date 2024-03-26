#regular
library(dplyr)
library(data.table)
library(doParallel)
#model tested
library(BGLR)

#options
options(bitmapType = "cairo")
options(
  error = function()
    traceback(3)
)

#seed
set.seed(1)



homemadeBayesC <- function(inPath, outPath, threads, niter, burn) {
  
  
  temp <- readRDS(inPath)
  
  X <- temp[, trait]
  y <- temp[, -trait]
  
  W <- scale(X)
  
  ids <- readRDS("data/id_bank.Rds")
  
  registerDoParallel(cores = snakemake@threads)
  
  iter <- 50
  
  corLoop <- foreach(i = 1:iter) %dopar% {
    #result holder
    corResult <- 9999
    
    #setup train and test sets with trait vectors
    test_IDs <- unlist(ids[i])
    
    W_train <- W[-test_IDs, ]
    W_test <- W[test_IDs, ]
    y_train <- y[-test_IDs]
    y_test <- y[test_IDs]
    
    ### QGG::GBAYES-C
    fitC <- qgg::gbayes(y=y_train, W=W_train, method="bayesC", scaleY=FALSE, nit=niter, nburn=burn)
    
    # \hat{y}_test = W_{test} * \hat{b} + \hat{mu}
    y_calc <- W_test %*% fitC$b + mean(y_train)
    corResult[2] <- cor(y_test, y_calc)
    
    corResult
    
  }
  
  final  <- unlist(corLoop)
  
  saveRDS(final, outPath)
  
}



homemadeBayesC(snakemake@input[[1]],
             snakemake@output[[1]],
             snakemake@threads,
             500,
             100)



