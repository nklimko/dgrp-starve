


mBase <- readRDS('snake/data/go/40_all/sexm/allData.Rds')
mFree <- readRDS('snake/data/go/40_all/sexm/bayesFREEfinal.Rds')

fBase <- readRDS('snake/data/go/40_all/sexf/allData.Rds')
fFree <- readRDS('snake/data/go/40_all/sexf/bayesFREEfinal.Rds')

allBayes <- data.table(mBase, mFree, fBase, fFree)

cindex <- c(5,10,15,20)

finalBayes <- allBayes[, cindex, with=FALSE]

colNames(finalBayes) <- c('Male 0.8', 'Male Free', 'Female 0.8', 'Female Free')

temp9 <- as.numeric(unlist(finalBayes))
restore <- data.table(matrix(temp9, ncol=4))

bayesMeans <- colMeans(restore)

names(bayesMeans) <- c('Male 0.8', 'Male Free', 'Female 0.8', 'Female Free')

bayesMeans





meat <- list.files('snake/data/sr/33_metric/f/cor', full.names=TRUE)

fData <- sapply(meat, readRDS)




tofu <- list.files('snake/data/sr/33_metric/m/cor', full.names=TRUE)

mData <- sapply(tofu, readRDS)

finale <- cbind(names=c('lasso', 'mr.ash', 'varbvs'),f = as.numeric(fData[c(2,4,6)]), m = as.numeric(mData[c(2,4,6)]))
