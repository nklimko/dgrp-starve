
library(dplyr)
library(data.table)
library(doParallel)

registerDoParallel(cores = 12)


filePath <- "/data/morgante_lab/nklimko/rep/dgrp-starve/data/fRegress.txt"

fMeans <- fread("/data/morgante_lab/nklimko/rep/dgrp-starve/data/fMeans.txt")

start <- 3
end <- 11340
#f 11340
#m 13577

id <- start:end

pvalList <- foreach(i=start:end) %dopar% {
  
  temp <- summary(lm(fMeans[,c(2,i), with=FALSE], na.action=na.omit))[[4]][8]  
}

part <- unlist(pvalList, use.names = FALSE)

part <- data.table(id,pvalList)

fwrite(part, filePath)
