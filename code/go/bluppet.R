#!/usr/bin/env Rscript

#setup----
if(1){
  library(data.table)
  library(dplyr)
  library(argparse)
  library(BGLR)
  
  options(error = function() traceback(3))
  set.seed(123)
}
#function----
center_vec <- function(x) {x - mean(x)}

#params----
if(0){
  
  inPath <- 'data/sr/01_matched/f_starvation.Rds'
  idPath <- 'data/sr/02_ids/f/ids_2.Rds'
  goPath <- 'data/sr/03_goterms/f/2.Rds'
  outPath <- 'bayesTest.Rds'
  
  inPath <- 'data/01_matched/f_starvation.Rds'
  idPath <- 'data/02_ids/f/ids_1.Rds'
  goPath <- 'data/go/03_goterms/sexf/GO.0000002.Rds'
  
  
  model <- 'RKHS'
  saveAt <- 'data/bglr/sexf/falsehood_'
  
  R2_max <- 0.8
  R2_GO <- 0.01
  
  
  inPath <- 'data/01_matched/f_starvation.Rds'
  goPath <- 'data/go/03_goterms/sparsef100blup/GO.0000022.Rds'
  
}

#bglr call----
homemadeBGLR <- function(inPath,
                         goPath,
                         outPath,
                         goterm,
                         sex,
                         repCount,
                         R2_max,
                         R2_GO,
                         model){
  xp <- readRDS(inPath)
  
  X <- xp[,-1]
  y <- unlist(xp[, 1])
  
  W <- scale(X)
  go_ids <- readRDS(goPath)
  
  WGO <- W[,go_ids]
  WNON <- W[,-go_ids]
  
  R2_nonGO <- R2_max - R2_GO
  
  #GRMs created using GO subsets
  GGO <- tcrossprod(WGO)/ncol(WGO)
  GNON <- tcrossprod(WNON)/ncol(WNON)
  
  #custom model of two BLUP effect terms
  ##each RKHS list uses covariance structure
  customETA <- list(list(K = GGO, model = model, saveEffects=TRUE), list(K=GNON, model=model, saveEffects=TRUE))
  
  sex <- 'f'
  goterm <- 'GO.0000022'
  i <- 1

  
  GGO[1:4,1:4]
  GNON[1:4,1:4]
  
  
  #function to handle each replicate as a number from 1 to max rep count
  gblupHandler <- function(i, goterm, sex, y, customETA){
    
    set.seed(123)
    
    #iter marker
    print(paste0('loop', i))
    
    #id specific paths i/o
    idPath <- paste0('data/02_ids/', sex, '/ids_', i, '.Rds')
    saveAt <- paste0('data/bglr/sex', sex, '/gblup/', goterm, '/goset_', i,'_')
    midPath <- paste0('data/go/24_goCor/', sex, '/0.8/0.01/', goterm,'/gblup_',i,'.Rds')
    
    #read in test/train ids
    ids <- readRDS(idPath)
    
    #copy vector for prediction later
    Ytrain <- y
    
    #remove data for prediction later
    Ytrain[ids] <- NA
    
    ### BGLR input model from function list(G=GRM)
    runtime <- system.time(fitBGLR <-
                             BGLR::BGLR(
                               Ytrain,
                               response_type = "gaussian",
                               R2= 0.8,
                               ETA = customETA,
                               saveAt = saveAt,
                               verbose = FALSE
                             )
    )
    
    str(fitBGLR)
    
    
    refill <- predict(fitBGLR)
    
    corResult <- cor(y[ids], refill[ids])
    
    #final <- list(cor=corResult, time=runtime, fit=fitBGLR)
    final <- list(cor=corResult, time=runtime) 
    
    #check cor debug
    print(paste0('cor: ', final$cor))
    
    #save all cor and runtime by subset to separate outPath 
    saveRDS(final, midPath)
    
    return(final)
  }
  
  #declare number of reps to use from snakemake args
  reps <- 1:repCount
  
  #corResults contains cor and runtime for all reps in list/sapply type
  corResults <- sapply(reps, gblupHandler, goterm = goterm, sex=sex, y=y, customETA = customETA)
  
  #extract cors
  cors <- unlist(corResults[1,])
  
  #save term and mean correlation coefficient as data table row
  finalRow <- data.table(term=goterm, cor=mean(cors))
  
  saveRDS(finalRow, outPath)
}



#args----
parser <- ArgumentParser(description= 'snakemake transfer')

parser$add_argument('--inPath')
parser$add_argument('--goPath')
parser$add_argument('--outPath')
parser$add_argument('--goterm')
parser$add_argument('--sex')
parser$add_argument('--repCount', type='integer')
parser$add_argument('--R2_GO', type='double')
parser$add_argument('--R2_max', type='double')
parser$add_argument('--model')


snake <- parser$parse_args()
print(str(snake))

#call----
homemadeBGLR(snake$inPath,
             snake$goPath,
             snake$outPath,
             snake$goterm,
             snake$sex,
             snake$repCount,
             snake$R2_max,
             snake$R2_GO,
             snake$model)

 G=tcrossprod(X)
 fm2=BGLR( y=y,ETA=list(G=list(K=G,model='RKHS')),
	   nIter=nIter,burnIn=burnIn,saveAt='eig_'
	 )
 varE=scan( 'eig_varE.dat')
 varU=scan('eig_ETA_G_varU.dat')
 h2_2=varU/(varU+varE)



Wset <- W[1:4,1:8]


Gset2 <- cov(Wset)
Gset <- tcrossprod(Wset)/ncol(Wset)



go2GRMs <- function(GO_num=GO_num, GO_data=GO_data, W_data=W_data, phenoLines=phenoLines){
	variants <- unique(unlist(GO_data[[GO_num]]))
    W_data_GO <- W_data[, which(colnames(W_data) %in% variants)]
    GRM_GO <- tcrossprod(W_data_GO)/ncol(W_data_GO)
    GRM_GO <- GRM_GO[phenoLines, phenoLines]
			
    W_data_notGO <- W_data[, -which(colnames(W_data) %in% variants)]
	GRM_notGO <- tcrossprod(W_data_notGO)/ncol(W_data_notGO)
	GRM_notGO <- GRM_notGO[phenoLines, phenoLines]

	return(list(GO=GRM_GO, notGO=GRM_notGO))
}


regress(yNa ~ 1, ~GRM_GO+GRM_notGO, pos=c(T, T, T), maxcyc = 10000)

fitz <- regress(Ytrain ~ 1, ~GGO+GNON, pos=c(T, T, T), maxcyc = 10000)

predict(fitz)


###Source custom functions
source(file="../Software/myRfunctions.R")
load("Data/adjustData.RData")


