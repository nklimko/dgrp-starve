#sr_analysis



#'snake/data/sr/40_all/f/cor/all.Rds'

save(femaleFinal, maleFinal, file='data/rawFinal.Rdata')
load('data/rawFinal.Rdata')


fMeans <- colMeans(femaleFinal)
mMeans <- colMeans(maleFinal)

fRank <- names(fMeans[order(-fMeans)])
mRank <- names(mMeans[order(-mMeans)])
rank <- 1:10

mean_F <- fMeans[order(-fMeans)]
mean_M <- mMeans[order(-mMeans)]


fRankVar <- c(1,1,1,1,0,1,0,0,0,0)
mRankVar <- c(0,0,1,0,0,1,1,1,1,0)



rankTable <- data.table(Rank = rank, Female=fRank, VSF=fRankVar, Mean_F=mean_F, Male=mRank, VSM=mRankVar, Mean_M=mean_M)
rankTable <- data.table(Rank = rank, Female=fRank, VSF=fRankVar, Mean_F=fMeans, Male=mRank, VSM=mRankVar, Mean_M=mMeans)
saveRDS(rankTable, 'data/rankTable.Rds')

sum(fitPCR$Xvar)/fitPCR$Xtotvar

rankTable <- readRDS('data/rankTable.Rds')


finalRanks <- rankTable %>% mutate_if(is.numeric, round, digits=4)


saveRDS(finalRanks, 'data/finalRanks.Rds')

finalRanks <- finalRanks[,c(1,2,4,5,7), with=FALSE]

kable(finalRanks, "latex")

