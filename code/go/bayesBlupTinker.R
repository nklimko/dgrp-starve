library(data.table)

#temp <- readRDS('data/go/40_all/sexf/blup_allData.Rds')
#dim(temp)
#temp[,cor]
#plot(x=1:dim(temp)[1], y=temp[,cor])
#plot(h=0.315)

#blup
allDataF <- readRDS('data/go/40_all/sexf/blup_allData.Rds')
allDataM <- readRDS('data/go/40_all/sexm/blup_allData.Rds')

#allDataF[,2] <- as.numeric(allDataF[,2])
#allDataM[,2] <- as.numeric(allDataM[,2])

allDataF_sorted <- allDataF[order(-cor),]
allDataM_sorted <- allDataM[order(-cor),]

blupF_top <- cbind(blupRank=1:20, allDataF_sorted[1:20])
blupM_top <- cbind(blupRank=1:20, allDataM_sorted[1:20])


#bayes
bayesF <- readRDS('data/go/40_all/sexf/partData.Rds')
bayesF <- data.table(term=bayesF[,4], cor = as.numeric(bayesF[,5]))
bayesF_sorted <- bayesF[order(-cor),]
bayesF_top <- cbind(bayesRank=1:20, bayesF_sorted[1:20])

bayesM <- readRDS('data/go/40_all/sexm/allData.Rds')
bayesM <- data.table(term=bayesM[,4], cor = as.numeric(bayesM[,5]))
bayesM_sorted <- bayesM[order(-cor),]
bayesM_top <- cbind(bayesRank=1:20, bayesM_sorted[1:20])

allTopF <- cbind(blupF_top, bayesF_top)
allTopM <- cbind(blupM_top, bayesM_top)

finF <- blupF_top[bayesF_top, on=.(term), nomatch=NULL]
finM <- blupM_top[bayesM_top, on=.(term), nomatch=NULL]

saveRDS(allTopF, 'data/go/50_tables/allTopF.Rds')
saveRDS(allTopM, 'data/go/50_tables/allTopM.Rds')
saveRDS(finF, 'data/go/50_tables/finF.Rds')
saveRDS(finM, 'data/go/50_tables/finM.Rds')

scrambleF <- finF[,c(5,3,2,4,1)]
names(scrambleF) <- c('BayesC_Cor', 'TBLUP_Cor', 'Term', 'BayesC_Rank', 'TBLUP_Rank')

scrambleM <- finM[,c(5,3,2,4,1)]
names(scrambleM) <- c('BayesC_Cor', 'TBLUP_Cor', 'Term', 'BayesC_Rank', 'TBLUP_Rank')

finF <- scrambleF
finM <- scrambleM

#in dgrp-starve/
save(allDataF, allDataM, allTopF, allTopM, finF, finM, file='snake/data/go/50_tables/saveTables.Rdata')
#in snake/
save(allDataF, allDataM, allTopF, allTopM, finF, finM, file='data/go/50_tables/saveTables.Rdata')




unlist(allDataF[,2])


percentModder <- function(dataKable){
  
  dataKable[,5] <- dataKable[,5]*2
  dataKable[,8] <- dataKable[,8]*2
  
  
  
  
  colnames(dataKable) <- rep(c('Flybase Gene', 'Percent', 'Gene Name'), 3)

  
  
  setcolorder(dataKable, c(1,3,2,4,6,5,7,9,8))
  
  
  return(dataKable)
  
}

percentModder(bayesF_Kable, 'TEST')

dataKable <- bayesF_KableMod 
