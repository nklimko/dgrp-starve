#setup----
library(data.table)

#test----
if(0){
  wd <- "/data2/morgante_lab/nklimko/rep/dgrp-starve/snake/data/sr/30_summary/"
  method <- "lasso_"
  outPath <- "/data2/morgante_lab/nklimko/rep/dgrp-starve/snake/data/sr/lasso_final.Rds"
  superFunk(wd, method, outPath)
}


#cor----
meanCor <- function(name){
  temp <- unlist(readRDS(name))
  return(mean(temp[seq(1,length(temp),6)]))
}

#time----
meanTime <- function(name){
  temp <- unlist(readRDS(name))
  return(mean(temp[seq(4,length(temp),6)]))
}

#params----
allBind <- function(name){
  name <- str_remove(name, ".Rds")
  piece <- strsplit(name, split = "_")
  dataRow <- data.table(method=piece[[1]][1],
                        nfolds=piece[[1]][2],
                        dfmax=piece[[1]][3],
                        relax=piece[[1]][4],
                        sex=piece[[1]][5])
  return(dataRow)
}

#main----
superFunk <- function(wd, method, outPath){
  #set spawn and grab all method files
  setwd(wd)
  dataList <- list.files(pattern=method)
  
  #mean cor and time lapply for all files
  #param splitter
  cor <- lapply(dataList, meanCor)
  time <- lapply(dataList, meanTime)
  param <- lapply(dataList, allBind)
  
  #convert list to vector
  cor <- as.numeric(unlist(cor))
  time <- as.numeric(unlist(time))
  
  #list to data table
  param <- rbindlist(param)
  
  #merge all
  final <- cbind(param, cor, time)
  
  #save output
  saveRDS(final, outPath)
  
}


#args----
parser <- ArgumentParser(description= 'snakemake transfer')

parser$add_argument('--wd')
parser$add_argument('--method')
parser$add_argument('--output')

snake <- parser$parse_args()
print(str(snake))

#call----
superFunk(snake$wd,
          snake$method,
          snake$output)






