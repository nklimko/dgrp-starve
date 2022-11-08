

### Libraries
library("data.table")
library("dplyr")


##change per job
dataPath <- "/data/morgante_lab/data/dgrp/genotypes/dgrp2_tgeno_filtered_meanimputed.txt"
targetPath <- "/data/morgante_lab/nklimko/rep/dgrp-starve/data/INPUT.txt"
finalPath <- "/data/morgante_lab/nklimko/rep/dgrp-starve/data/CHANGE.txt"

#number of results to save
finalCount <- 200

#files read in
data <- fread(dataPath) 
inGroup <- fread(targetPath)

#modification of inGroup to vector type, clunky
inGroup <- inGroup[,line]

#IDs saved and removed
var_id <- data[,var_id]
data <- data[, var_id:=NULL]

#data partitioned by column
inData <- data[,colnames(data) %in% inGroup, with=FALSE]
outData <- data[,!(colnames(data) %in% inGroup), with=FALSE]

#means calculated for every row
inMean <- rowMeans(inData)
outMean <- rowMeans(outData)
  
#table constructed of marker IDs and group means
phaseA <- data.table(var_id, inMean, outMean)

#calculates difference of group means
trueData <-  phaseA[, result:=inMean - outMean]
#trueData <-  phaseA[, result:=inMean / outMean]

#orders data by absolute value of difference, largest first
trueSort <- trueData[order(-abs(result))]

#index controls number of top results to save
finalSet<- trueSort[1:finalCount]

#writes top (finalCount) to designated path
fwrite(finalSet, finalPath)

