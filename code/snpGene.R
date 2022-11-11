
### Libraries
library(data.table)
library(dplyr)
library(stringr)

#input paths
genePath <- "/data/databases/flybase/fb-r5.57/fb-r5.57.clean.gene.bound"
snpPath <- "/data/morgante_lab/nklimko/rep/dgrp-starve/data/snpList.txt"

#read data in
genBank <- fread(genePath, col.names = c("leg","start", "stop", "strand","gene"))
snpList <- fread(snpPath)

#split SNP tag to search gene data correctly
snpList[, arm := tstrsplit(var_id, split="_",fixed = TRUE)[1]]
snpList[, pos := tstrsplit(var_id, split="_",fixed = TRUE)[2]]
snpList[, pos := as.numeric(pos)]

#set start and stop to numeric type
genBank[, ':='(start=as.numeric(start), stop=as.numeric(stop))]

#extract gene where position contained and arm matches
geneHits <- genBank[snpList, on = .(start <= pos, stop >= pos,leg==arm), .(ori, geneFind = gene)][!is.na(geneFind)]

#write file
fwrite(geneHits, "/data/morgante_lab/nklimko/rep/dgrp-starve/data/geneHits.txt")


