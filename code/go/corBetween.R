#checks correlation between blup and bayesC for all go terms

#female
bayesPath <- 'data/go/40_all/sexf/allData.Rds'
blupPath <- 'data/go/40_all/sexf/blup_allData.Rds'

bayesData <- readRDS(bayesPath)
blupData <- readRDS(blupPath)

bayes <- as.numeric(bayesData[,5])
blup <- as.numeric(unlist(blupData[,2]))

fCor <- cor(blup, bayes)
#blupData[1:5]
#bayesData[1:5,]


#Male
bayesPath <- 'data/go/40_all/sexm/allData.Rds'
blupPath <- 'data/go/40_all/sexm/blup_allData.Rds'

bayesData <- readRDS(bayesPath)
blupData <- readRDS(blupPath)

bayes <- as.numeric(bayesData[,5])
blup <- as.numeric(unlist(blupData[,2]))

mCor <- cor(blup, bayes)
#blupData[1:5]
#bayesData[1:5,]


fCor
mCor