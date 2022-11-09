#######################################
#
#  Noah Klimkowski
#  combineSNP.R
#  11/9/2022
#
#  Walkthrough of data cleaning after
#  snp extraction from all trials 
#
#######################################

### Libraries
library(dplyr)
library(data.table)


# Set max number of results
index <- 50

#read in "index" number of results, set above
dM <- fread("../data/difMinus-result.txt", nrows = index)
dP <- fread("../data/difPlus-result.txt", nrows = index)
aM <- fread("../data/avgMinus-result.txt", nrows = index)
aP <- fread("../data/avgPlus-result.txt", nrows = index)

#mark origin of SNP
dM[,ori:="difMinus"]
dP[,ori:="difPlus"]
aM[,ori:="avgMinus"]
aP[,ori:="avgPlus"]

#combine tables
snpList <- rbind(dM, dP, aM, aP)

#remove data columns by assigning to NULL
snpList[,':='(inMean=NULL, outMean=NULL, result=NULL)]

#Save file
fwrite(snpList, "/data/morgante_lab/nklimko/rep/dgrp-starve/data/snpList.txt")











