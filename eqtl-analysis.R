#use this to save plots to secretariat somehow
library(ggplot2)


rm(list=ls())
par(mfrow=c(2,1))


#Male case
csvPath <- "/data/morgante_lab/data/dgrp/phenotypes/eQTL_traits_males.csv"
colInd <- 11
dataSet <- "Male"

#Female case
csvPath <- "/data/morgante_lab/data/dgrp/phenotypes/eQTL_traits_females.csv"
colInd <- 10
dataSet <- "Female"





allRaw <- read.csv(csvPath)
#allRaw
#dim(allRaw)
#colnames(allRaw)
#allRaw[,colInd]
starveRaw <- allRaw[,c(1,colInd)]
#starveRaw

#record size of table
starveDim <- dim(starveRaw)[1]

#set index to zero for loop
indMark <- 0
#look at every element, check for NA, and record index if NA
for(i in 1:starveDim){
  if(is.na(starveRaw[i,2])){
    indMark <- c(indMark, i)
    #print("null detect")
  } else{
    #nothing occurs
  }
}

#remove initial index(zero) from vector
indFin <- indMark[2:length(indMark)]

#Number of lines with no data/data
none <- length(indFin)
trait <- starveDim - none

#extract all columns EXCEPT where NA
starveFinal <- starveRaw[-indFin,]

#dim(starveFinal)

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








