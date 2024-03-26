temp <- list.dirs('data/go/24_goCor/m/randSparse')
                  

#Pre fit removal quality terms
midpoint1 <- lapply(list.files(path='data/go/24_goCor/m/randSparse/GO.0045924', full.names = TRUE), readRDS)
midpoint2 <- sapply(list.files(path='data/go/24_goCor/m/randSparse/GO.0035003', full.names = TRUE), readRDS)
midpoint3 <- sapply(list.files(path='data/go/24_goCor/m/randSparse/GO.0008320', full.names = TRUE), readRDS)
midpoint4 <- sapply(list.files(path='data/go/24_goCor/m/randSparse/GO.0006730', full.names = TRUE), readRDS)
midpoint5 <- sapply(list.files(path='data/go/24_goCor/m/randSparse/GO.0002814', full.names = TRUE), readRDS)

temp <- list.files(path='data/go/24_goCor/m/randSparse/GO.0045924', full.names = TRUE)

temp4 <- temp[9:11]

tempRead <- sapply(temp4, readRDS)

str(tempRead)

tempRead[[1]]
tempRead[[4]]
tempRead[[7]]
tempRead[[10]]


"Controlled matings not possible(no test crosses!) - Eugenics"

mean(c(0.42,
0.39,
0.72,
0.80,
0.48,
0.59,
0.44,
0.61,
0.35,
0.57,
0.43))

0.52

#mean is good, least squares
#
#"I think the ceo of american airlines should be severely tortured, don't you think?"
#
#
#


bayesPath <- 'data/go/40_all/sexf/allData.Rds'
blupPath <- 'data/go/40_all/sexf/blup_allData.Rds'

bayesData <- readRDS(bayesPath)
blupData <- readRDS(blupPath)

bayes <- as.numeric(bayesData[,5])
blup <- as.numeric(unlist(blupData[,2]))

fCor <- cor(blup, bayes)
blupData[1:5]
bayesData[1:5,]

fAll <- data.table(blup=blup, bayes=bayes)

fALL_blup <- fAll[order(-blup),]
fALL_bayes <- fAll[order(-bayes),]

fall_base <- cor(fAll[,1], fAll[,2])
fall_bayesTop <- cor(fALL_bayes[1:250,1], fALL_bayes[1:250,2])
fall_blupTop <- cor(fALL_blup[1:250,1], fALL_blup[1:250,2])


 cor(fAll[,1], fAll[,2], method = 'spearman')

bayesPath <- 'data/go/40_all/sexm/allData.Rds'
blupPath <- 'data/go/40_all/sexm/blup_allData.Rds'

bayesData <- readRDS(bayesPath)
blupData <- readRDS(blupPath)

bayes <- as.numeric(bayesData[,5])
blup <- as.numeric(unlist(blupData[,2]))

mAll <- data.table(blup=blup, bayes=bayes)

mALL_blup <- mAll[order(-blup),]
mALL_bayes <- mAll[order(-bayes),]

mall_base <- cor(mAll[,1], mAll[,2])
mall_bayesTop <- cor(mALL_bayes[1:250,1], mALL_bayes[1:250,2])
mall_blupTop <- cor(mALL_blup[1:250,1], mALL_blup[1:250,2])

mCor <- cor(blup, bayes)
blupData[1:5]
bayesData[1:5,]

fCor
mCor


corTable <- data.table(Corr = c('All', 'Top 250 Bayes', 'Top 250 Blup', 'Spearman'), Female=c(fall_base, fall_bayesTop, fall_blupTop, fSpear), Male=c(mall_base, mall_bayesTop, mall_blupTop, mSpear))

saveRDS(corTable, 'data/go/50_tables/corTable.Rds')

yW <- data.table(trait=y_train, W_train)

fit <- lm(trait ~ ., data = yW) 


W_testFrame <- data.table(W_test)

predict(fit, newdata = W_testFrame)

temp <- na.omit(fit$coefficients)

temp[1:179]

library(olsrr)


fit <- ols_regress(trait ~ ., data=yW)


lm fails to include anything more than number of lines

alimi alimi alimi alimi alima

mSpear <- cor(mAll[,1], mAll[,2], method = 'spearman')

fSpear <- cor(fAll[,1], fAll[,2], method = 'spearman')

newRow <- c('Spearman', fSpear, mSpear)

rbind(corTable, newRow)


cor

corTable <- data.table(Corr = c('All', 'Top 250 Bayes', 'Top 250 Blup', 'Spearman'), Female=c(fall_base, fall_bayesTop, fall_blupTop, fSpear), Male=c(mall_base, mall_bayesTop, mall_blupTop, mSpear))

saveRDS(corTable, 'data/go/50_tables/corTable.Rds')

mall_bayesTop20 <- cor(mALL_bayes[1:20,1:2], mALL_bayes[1:20,2])


