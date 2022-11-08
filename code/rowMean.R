### Libraries
library("data.table")
library("dplyr")



rawData <- fread("/data/morgante_lab/data/dgrp/genotypes/dgrp2_tgeno_filtered_meanimputed.txt")
#rawDataTemp <- fread("/data/morgante_lab/nklimko/rep/dgrp-starve/data/testData.txt")

print(dim(rawData))

#rawData <- rawData[1:5]

#print(rawData)


#rawData <- as.data.table(rawDataTemp)

inGroup <- fread("/data/morgante_lab/nklimko/rep/dgrp-starve/data/group1.txt")
inGroup <- inGroup[,line]


ids <- rawData[,var_id]

rawData <- rawData[, var_id:=NULL]

print(inGroup)
print(is.vector(inGroup))

inData <- rawData[,colnames(rawData) %in% inGroup, with=FALSE]
outData <- rawData[,!(colnames(rawData) %in% inGroup), with=FALSE]

#print(inData[1:2])
#print(outData[1:2])

print("extract complete")

  #means calculated for every numerical column 
  inMean <- rowMeans(inData)
  outMean <- rowMeans(outData)
  
  #id is gene/snp, table created of id with means from both groups
  
  meanDif <- data.table(ids, inMean, outMean)
  
 # return(meanDif)
#}

  
#print(meanDif)

print("break, mean Dif success")
#print(outMean)

###This function computes difference in means and returns highest absolute value SNPs
#computeDif <- function(data, index) {
  
  #calculates difference of group means
  trueData <-  meanDif[, trueDif:=inMean - outMean]
  
  #orders data by absolute value of difference, largest first
  finalSet <- trueData[order(-abs(trueDif))]

  #index controls number of top results to save
  result <- finalSet[1:200]
  
 # return(result)

#}
  
  
print("fwriting")
  
  fwrite(result, "/data/morgante_lab/nklimko/rep/dgrp-starve/data/g1-result.txt")
