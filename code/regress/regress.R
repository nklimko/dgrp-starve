
library(dplyr)
library(data.table)
library(doParallel)

registerDoParallel(cores = 12)


filePath <- "/data/morgante_lab/nklimko/rep/dgrp-starve/data/mRegress.txt"

mMeans <- fread("/data/morgante_lab/nklimko/rep/dgrp-starve/data/mMeans.txt")

start <- 3
end <- 13577

#mPart <- mMeans[,c(1,2,start,end),with=FALSE]

id <- start:end

pvalList <- foreach(i=start:end) %dopar% {
  
  temp <- summary(lm(mMeans[,c(2,i), with=FALSE], na.action=na.omit))[[4]][8]  
}

part <- unlist(pvalList, use.names = FALSE)

part <- data.table(id,pvalList)

fwrite(part, filePath)
