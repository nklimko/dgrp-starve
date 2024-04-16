

library(dplyr)
library(data.table)
library(stringr)
library(ggplot2)

simpleBind <- function(name){
  name <- str_remove(name, ".Rds")
  piece <- strsplit(name, split = "_")
  dataRow <- data.table(method=piece[[1]][1],
                        sex=piece[[1]][2])
  return(dataRow)
}

bayesBind <- function(name){
  name <- str_remove(name, ".Rds")
  piece <- strsplit(name, split = "_")
  dataRow <- data.table(method=piece[[1]][1],
                        R2=piece[[1]][2],
                        sex=piece[[1]][3])
  return(dataRow)
}



allBind <- function(name){
  name <- str_remove(name, ".Rds")
  piece <- strsplit(name, split = "_")
  dataRow <- data.table(method=piece[[1]][1],
                        nfolds=as.numeric(piece[[1]][2]),
                        dfmax=as.numeric(piece[[1]][3]),
                        relax=piece[[1]][4],
                        sex=piece[[1]][5])
  return(dataRow)
}

filt <- function(dataPath, method){
  
  data <- readRDS(dataPath)
  
  if(method=='cor4'){
    col <- 1
    start <- 1
    skip <- 4
  } else if(method=='cor'){
    col <- 1
    start <- 1
    skip <- 16
  } else if(method=='time'){
    col <- 2
    start <- 3
    skip <- 5
  } else if(method=='lasso'){
    col <- 1
    start <- 1
    skip <- 1
  } else{
    print('Method not recognized.')
    return(NA)
  }
  
  raw <- unlist(data[col, ])
  trim <- raw[seq(start, length(raw), by=skip)]
  
  return(trim)
}

pathFinder <- function(home, base, pattern){
  setwd(home)
  setwd(base)
  pathList <- list.files(path='.', pattern=pattern)
  setwd(home)
  return(pathList)
}

pathHunter <- function(home, base, pathList, method){
  setwd(home)
  setwd(base)
  filtered <- lapply(pathList, filt, method)
  setwd(home)
  return(filtered)
}

dataTitleTag <- function(data, title, tagChar){
  return(ggplot(data, aes(x=method, y=cor, fill=method)) +
           geom_violin(color = NA, width = 0.65) +
           geom_boxplot(color='#440154FF', width = 0.15) +
           theme_minimal() +
           stat_summary(fun=mean, color='#440154FF', geom='point', 
                        shape=18, size=3, show.legend=FALSE) +
           labs(x=NULL,y=title,tag=tagChar) +
           theme(legend.position='none',
                 axis.text.x = element_text(angle = -45, size=10),
                 text=element_text(size=10),
                 plot.tag = element_text(size=15)) +
           scale_fill_viridis(begin = 0.4, end=0.9,discrete=TRUE)
  )
}

home <- "/data2/morgante_lab/nklimko/rep/dgrp-starve"
setwd(home)



if(original){
  
  
  base <- 'snake/data/20_cor'
  pattern <- "starvation.Rds"
  method <- 'cor4'
  
  
  pathList <- pathFinder(home, base, pattern)
  
  pathList <- pathList[-3:-14]
  
  pathList
  
  trial <- lapply(pathList, soloRead){
    
    temp <- readRDS(pathList)
    
    
    
    
    
  }
  
  trial[[3]]
  
  
  topCor <- pathHunter(home, base, pathList, method)
  topTable <- as.data.table(matrix(unlist(topCor), ncol=length(topCor)))
  topMeanCor <- colMeans(topTable, na.rm=TRUE)
  
  topTime <- pathHunter(home, base, pathList, "time")
  topTable <- as.data.table(matrix(unlist(topTime), ncol=length(topTime)))
  topMeanTime <- colMeans(topTable, na.rm=TRUE)
  
  
  trial <- lapply(pathList, simpleBind)
  params <- rbindlist(trial)
  
  topFinal <- cbind(params, cor=topMeanCor, time=topMeanTime)
  rownames(topFinal) <- pathList
  
  topFinal <- topFinal[order(sex, method),]
  
  topFinal
  
  saveRDS(topFinal, "snake/data/zigure/topFinal.Rds")
  
  
  
  
}


if(most_other){
  
  base <- 'snake/data/sr/30_summary'
  pattern <- ".Rds"
  method <- 'cor'
  
  
  pathList <- pathFinder(home, base, pattern)
  
  topCor <- pathHunter(home, base, pathList, method)
  topTable <- as.data.table(matrix(unlist(topCor), ncol=length(topCor)))
  topMeanCor <- colMeans(topTable, na.rm=TRUE)
  
  topTime <- pathHunter(home, base, pathList, "time")
  topTable <- as.data.table(matrix(unlist(topTime), ncol=length(topTime)))
  topMeanTime <- colMeans(topTable, na.rm=TRUE)
  
  
  trial <- lapply(pathList, simpleBind)
  params <- rbindlist(trial)
  
  topFinal <- cbind(params, cor=topMeanCor, time=topMeanTime)
  rownames(topFinal) <- pathList
  
  topFinal <- topFinal[order(sex, method),]
  
  topFinal
  
  saveRDS(topFinal, "snake/data/zigure/topFinal.Rds")
  
  
  
}

if(bayesop){
  
  
  base <- 'snake/data/sr/30_summary'
  pattern <- "bayes"
  method <- 'cor'
  
  
  pathList <- pathFinder(home, base, pattern)
  
  setwd(base)
  
  bayesFix <- function(dataPath, col){
    temp <- readRDS(dataPath)
    fin <- unlist(temp[col,])
    return(fin)
  }
  
  
  trial <- lapply(pathList, bayesFix, col=1)
  corData <- as.data.table(matrix(unlist(trial), ncol=length(trial)))
  corFin <- colMeans(corData)
  
  
  trial <- lapply(pathList, bayesFix, col=2)
  raw <- unlist(trial)
  timeData <- raw[seq(3, length(raw), by=5)]
  timeAll <- as.data.table(matrix(timeData, ncol = length(trial)))
  
  timeFin <- colMeans(timeAll)
  
  trial <- lapply(pathList, bayesBind)
  params <- rbindlist(trial)
  
  bayesFinal <- cbind(params, cor=corFin, time=timeFin)
  
  bayesFinal <- bayesFinal[order(sex, R2),]
  
  
  saveRDS(bayesFinal, "snake/data/zigure/bayesFinal.Rds")
  
  
  
  
  
}


if(top3){
  
  base <- 'snake/data/top3/30_summary'
  pattern <- ".Rds"
  method <- 'cor'
  
  
  pathList <- pathFinder(home, base, pattern)
  
  topCor <- pathHunter(home, base, pathList, method)
  topTable <- as.data.table(matrix(unlist(topCor), ncol=length(topCor)))
  topMeanCor <- colMeans(topTable, na.rm=TRUE)
  
  topTime <- pathHunter(home, base, pathList, "time")
  topTable <- as.data.table(matrix(unlist(topTime), ncol=length(topTime)))
  topMeanTime <- colMeans(topTable, na.rm=TRUE)
  
  
  trial <- lapply(pathList, simpleBind)
  params <- rbindlist(trial)
  
  topFinal <- cbind(params, cor=topMeanCor, time=topMeanTime)
  rownames(topFinal) <- pathList
  
  topFinal <- topFinal[order(sex, method),]
  
  topFinal
  
  saveRDS(topFinal, "snake/data/zigure/topFinal.Rds")
  
  pathList <- c("datadrive_f.Rds", "datadrive_m.Rds") 
  
  base <- "snake/data/top3/30_summary/"
  
  setwd(base)
  
  
  
  temp2 <- filt(pathList[1], "cor")
  
  
  
  temp3 <- filt(pathList[2], "cor")
  
  
  
  temp <- readRDS(pathList[2])
  
  forlist2 <- unlist(temp[1,])
  
  label <- c(rep("f", length(temp2)), rep("m", length(temp3)))
  
  
  
  test <- data.table(sex=label, cor=as.numeric(temp2, temp3))
  
  saveRDS(test, "snake/data/zigure/dd.Rds")
  
  test
  
  
  data <- test
  
  gg[[11]] <- ggplot(data, aes(x=sex, y=cor, fill=sex)) +
    geom_violin(color = NA, width = 0.65) +
    geom_boxplot(color='#440154FF', width = 0.15) +
    theme_minimal() +
    stat_summary(fun=mean, color='#440154FF', geom='point', 
                 shape=18, size=3, show.legend=FALSE) +
    labs(x=NULL,y=title,tag=tagChar) +
    theme(legend.position='none',
          axis.text.x = element_text(angle = -45, size=10),
          text=element_text(size=10),
          plot.tag = element_text(size=15)) +
    scale_fill_viridis(begin = 0.4, end=0.9,discrete=TRUE)
  
  
  
  
  
}

# lasso_9_6000_1_m_49.Rds
# lasso_9_6000_1_m_4.Rds
# lasso_9_6000_1_m_50.Rds
# lasso_9_6000_1_m_5.Rds
# lasso_9_6000_1_m_6.Rds
# lasso_9_6000_1_m_7.Rds
# lasso_9_6000_1_m_8.Rds
# lasso_9_6000_1_m_9.Rds

if(lasso){
  
  base <- 'snake/data/sr/30_summary'
  pattern <- "lasso"
  method <- 'lasso'
  
  pathList <- pathFinder(home, base, pattern)
  
  lassoCor <- pathHunter(home, base, pathList, method)
  lassoTable <- as.data.table(matrix(unlist(lassoCor), ncol=length(lassoCor)))
  lassoMeanCor <- colMeans(lassoTable, na.rm=TRUE)
  
  lassoTime <- pathHunter(home, base, pathList, "time")
  lassoTable <- as.data.table(matrix(unlist(lassoTime), ncol=length(lassoTime)))
  lassoMeanTime <- colMeans(lassoTable, na.rm=TRUE)
  
  trial <- lapply(pathFinder(home, base, 'lasso'), allBind)
  lassoParam <- rbindlist(trial)
  
  lassoFinal <- cbind(lassoParam, cor=lassoMeanCor, time=lassoMeanTime)
  lassoFinal <- lassoFinal[order(sex, lassoFinal$nfolds, relax, dfmax),]
  
  saveRDS(lassoFinal, "data/zigure/lassoFinal.Rds")
  
  f <- mf[1:32,]
  m <- mf[33:64,]
  
  
  f <- f[order(f$nfolds, relax, dfmax)]
  m <- m[order(m$nfolds, relax, dfmax)]
  
  
  saveRDS(m, "data/zigure/lassoM.Rds")
  saveRDS(f, "data/zigure/lassoF.Rds")
  
}


lm <- readRDS('zz_lost/lassoM.Rds')
lf

base <- "data/sr/30_summary/"

pathList <- c("rf_f.Rds","rf_m.Rds")


temp <- readRDS(pathList[1])


forlist <- unlist(temp[1,])


temp <- readRDS(pathList[2])


forlist2 <- unlist(temp[1,])


label <- c(rep("f", length(forlist)), rep("m", length(forlist2)))



test <- data.table(sex=label, cor=as.numeric(forlist, forlist2))

title <- "Correlation"
tagChar <- '.'

saveRDS(test, "data/zigure/rf.Rds")

data <- test

gg[[11]] <- ggplot(data, aes(x=sex, y=cor, fill=sex)) +
  geom_violin(color = NA, width = 0.65) +
  geom_boxplot(color='#440154FF', width = 0.15) +
  theme_minimal() +
  stat_summary(fun=mean, color='#440154FF', geom='point', 
               shape=18, size=3, show.legend=FALSE) +
  labs(x=NULL,y=title,tag=tagChar) +
  theme(legend.position='none',
        axis.text.x = element_text(angle = -45, size=10),
        text=element_text(size=10),
        plot.tag = element_text(size=15)) +
  scale_fill_viridis(begin = 0.4, end=0.9,discrete=TRUE)

#FILTER HEADER
filt <- function(data, method){
  
  if(method=="sr"){
    col <- 1
    return(unlist(data[col, ]))
  } else if(method=="nested"){
    hold <- vector(length=50)
    for(i in 1:length(data)){
      hold[i] <- data[[i]]$cor 
    }
    return(hold)
  } else if(method=='top3'){
    col <- 1
    start <- 1
    skip <- 16
    
    raw <- unlist(data[col, ])
    trim <- raw[seq(start, length(raw), by=skip)]
    
    
    return(trim)
  } else if(method=='cor4'){
    col <- 1
    start <- 1
    skip <- 4
  } else if(method=='time'){
    col <- 2
    start <- 3
    skip <- 5
  } else if(method=='lasso'){
    col <- 1
    start <- 1
    skip <- 1
  } else{
    print('Method not recognized.')
    return(NA)
  }
  return(0)
  raw <- unlist(data[col, ])
  trim <- raw[seq(start, length(raw), by=skip)]
  
}

data <- allData
i <- 1

ggTidy <- function(data){
  
  for(i in 1:dim(data)[2]){
    
    name <- colnames(data)[i]
    temp <- cbind(rep(name, dim(data)[1]), data[,i, with=FALSE])
    
    if(i==1){
      hold <- temp
    } else{
      hold <- rbind(hold, temp, use.names=FALSE)
    }
  }
  colnames(hold) <- c("method", "cor")
  
  return(hold)
}


#recover lasso parop runs