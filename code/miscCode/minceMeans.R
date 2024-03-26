temp <- readRDS('data/go/40_all/sexf/allData.Rds')
temp2 <- readRDS('data/go/50_tables/corTable.Rds')

save(maleFinal,femaleFinal, file='WORKINGERE.Rdata')

data <- maleFinal

mMeans <- colMeans(maleFinal, na.rm=1)
fMeans <- colMeans(femaleFinal, na.rm=1)

mince <- cbind(mMeans, fMeans)

mince[order(-mMeans),]

#groups
lmm <- 3
bayes <- c(2, 6, 10)
penal <- c(7, 8)
ml <- c(1, 5)
pca <- c(4, 9)

lmmMean <- mince[lmm,]
bayesMean <- colMeans(mince[bayes,])
penalMean <- colMeans(mince[penal,])
mlMean <- colMeans(mince[ml,])
pcaMean <- colMeans(mince[pca,])

allMeans <- rbind(lmmMean, bayesMean, penalMean, mlMean, pcaMean)

allMeans




temp

