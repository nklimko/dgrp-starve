rm(list=ls())

#libraries
library(ggplot2)
library(dplyr)
library(tidyr)
#library("workflowr")


#install.packages("workflowr")

rm(list=ls())

par(mfrow=c(2,3))

#create csv from hist
histCsv <- function(csvPath, dataSet){
  
  #Read in table
  allRaw <- tibble(read.csv(csvPath))
  
  starveRaw <- allRaw %>% select(line,starvation) %>% filter(!is.na(starvation))
  
  #histogram
  hist(starveRaw$starvation, main=paste("Histogram of",dataSet,"Starvation"), xlab="Starvation Resistance")
  return(starveRaw)
}

#save tibble
saveTib <- function(fileName, tib){
  fullName <- paste("/home/nklimko/R/dgrp-starve/", fileName, ".csv", sep = "")
  write.csv(tib,fullName)
  return()
}

#restore tibble
restore <- function(fileName){
  fullName <- paste("/home/nklimko/R/dgrp-starve/", fileName, ".csv", sep = "")
  tib <- tibble(read.csv(fullName))
  return(tib)
}

#Male case
csvPathM <- "/data/morgante_lab/data/dgrp/phenotypes/eQTL_traits_males.csv"
dataSetM <- "Male"
starveRawM <- histCsv(csvPathM, dataSetM)

#Female case
csvPathF <- "/data/morgante_lab/data/dgrp/phenotypes/eQTL_traits_females.csv"
dataSetF <- "Female"
starveRawF <- histCsv(csvPathF, dataSetF)

#Both case
starveBoth <- tibble(rbind(starveRawM, starveRawF))
hist(starveBoth$starvation, main=paste("Histogram of Starvation"), xlab="Starvation Resistance")

#Comparative Boxplot
boxplot(starveRawM$starvation, starveRawF$starvation, starveBoth$starvation, names=c("Male", "Female", "Both"), main="Sex Comparison of Starvation", xlab="Starvation Resistance", ylab="Sex", horizontal = TRUE)

#Summary stats
summary(starveRawM$starvation)
summary(starveRawF$starvation)
summary(starveBoth$starvation)

colnames(starveRawF) <- c("line", "fStarve")
colnames(starveRawM) <- c("line", "mStarve")


starveAll <- starveRawF %>% mutate(mStarve = starveRawM$mStarve)

starveDiff <- starveAll %>% select(line, fStarve, mStarve) %>% mutate(diffStarve = fStarve- mStarve) 

hist(starveDiff$diffStarve, main=paste("Histogram of Starvation Diff"), xlab="Starvation Difference")


saveTib("starveDiff", starveDiff)
dFin <- restore("starveDiff")
starveDiff <- dFin %>% select(line, fStarve, mStarve, diffStarve)

#starveDiff

