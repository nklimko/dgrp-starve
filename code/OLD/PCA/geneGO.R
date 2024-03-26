### Libraries
library(data.table)
library(dplyr)

#input paths
goPath <- "/data/databases/flybase/fb-r5.57/gene_go.table"
genePath <- "/data/morgante_lab/nklimko/rep/dgrp-starve/data/geneHits.txt"

#read data in
genList <- fread(genePath, col.names = c("ori", "gene"))
goBank <- fread(goPath, col.names = c("gene", "mf", "bp", "cc"))

#match go terms to gene
genList <- goBank[genList, on = .(gene)]

#save
fwrite(genList, "./goGroups.txt")


#No origin
table(genList[,mf])
table(genList[,cc])
table(genList[,bp])

##This one
table(genList[,gene, by=ori])
table(genList[,mf, by=ori])
table(genList[,cc, by=ori])
table(genList[,bp, by=ori])

