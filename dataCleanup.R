#Data cleanup
library(dplyr)
rm(list=ls())
par(mfrow=c(2,1))


#Male case
csvPath <- tibble(read.csv("/data/morgante_lab/data/dgrp/phenotypes/eQTL_traits_males.csv"))




dataSet <- "Male"

#Female case
csvF <- "/data/morgante_lab/data/dgrp/phenotypes/eQTL_traits_females.csv"
colInd <- 10
dataSet <- "Female"





allRaw <- read.csv(csvPath)

starveRaw <- allRaw[,c(1,colInd)]
colnames(starveRaw) <- c("line", )

#record size of table
starveDim <- dim(starveRaw)[1]

#grab trait column
sCol <- starveFinal[,2]


#historgram and summary statistics
hist(sCol, main=paste("Histogram of",dataSet,"Starvation"))
summary(sCol)

mFin <- starveFinal
mSum <- summary(sCol)
mNone <- none
mTrait <- trait
colnames(mFin) <- c("m-line", "m-starve")


fFin <- starveFinal
fSum <- summary(sCol)
fNone <- none
fTrait <- trait
colnames(fFin) <- c("f-line", "f-starve")


write.csv(mFin,"/home/nklimko/R/dgrp-starve/mFin.csv")
write.csv(fFin,"/home/nklimko/R/dgrp-starve/fFin.csv")

mFin <- read.csv("/home/nklimko/R/dgrp-starve/mFin.csv")
fFin <- read.csv("/home/nklimko/R/dgrp-starve/fFin.csv")

mFin
fFin


mDim <- dim(mFin)[1]
fDim <- dim(fFin)[1]
dim(fFin)

matchInd <- 0

matchNet <- matrix(rep(0,mDim*3), ncol=3)

#matchNet[1,]

newInd <- 1

for(m in 1:mDim){
  for(f in 1:fDim){
    if(mFin[m,1]==fFin[f,1]){
      matchNet[newInd,1] <- mFin[m,1]
      matchNet[newInd,2] <- mFin[m,2]
      matchNet[newInd,3] <- fFin[f,2]
      newInd <- newInd + 1
    }
  }
}


mFin
fFin

sameLines <- cbind(mFin[,1], fFin[,1])
sameLines

matchNet








