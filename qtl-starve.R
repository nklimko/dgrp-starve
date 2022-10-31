#use this to save plots to secretariat somehow
library(ggplot2)
library(dplyr)
library(tidyr)
rm(list=ls())

par(mfrow=c(2,2))

histBox <- function(csvPath, dataSet){
  
  #Read in table
  allRaw <- tibble(read.csv(csvPath))
  
  starveRaw <- allRaw %>% select(line,starvation) %>% filter(!is.na(starvation))
  
  #histogram and boxplot
  hist(starveRaw$starvation, main=paste("Histogram of",dataSet,"Starvation"), xlab="Starvation Resistance")
  #boxplot(starveRaw$starvation, , horizontal = TRUE)
  return(starveRaw)
}


#Male case
csvPathM <- "/data/morgante_lab/data/dgrp/phenotypes/eQTL_traits_males.csv"
dataSetM <- "Male"

#Female case
csvPathF <- "/data/morgante_lab/data/dgrp/phenotypes/eQTL_traits_females.csv"
dataSetF <- "Female"


starveRawM <- histBox(csvPathM, dataSetM)
starveRawF <- histBox(csvPathF, dataSetF)

boxplot(starveRawM$starvation, starveRawF$starvation, names=c("Male", "Female"), main="Sex Comparison of Starvation Resistance", xlab="Starvation Resistance", ylab="Sex", horizontal = TRUE)

summary(starveRawM$starvation)
summary(starveRawF$starvation)

colnames(starveRawF) <- c("line", "fStarve")
colnames(starveRawM) <- c("line", "mStarve")


starveAll <- starveRawF %>% mutate(mStarve = starveRawM$mStarve)

starveAll

write.csv(starveAll,"/home/nklimko/R/dgrp-starve/starveAll.csv")
write.csv(starveRawM,"/home/nklimko/R/dgrp-starve/starveM.csv")
write.csv(starveRawF,"/home/nklimko/R/dgrp-starve/starveF.csv")

mFin <- tibble(read.csv("/home/nklimko/R/dgrp-starve/starveM.csv"))
fFin <- tibble(read.csv("/home/nklimko/R/dgrp-starve/starveF.csv"))
aFin <- tibble(read.csv("/home/nklimko/R/dgrp-starve/starveAll.csv"))