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
  boxplot(starveRaw$starvation, horizontal = TRUE)
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




summary(starveRawM$starvation)
summary(starveRawF$starvation)


colnames(starveRawF) <- c("line", "f-starve")
colnames(starveRawM) <- c("line", "m-starve")

starveAll <- tibble(cbind(starveRawF$line, starveRawM$m-starve, starveRawM$m-starve))

starveAll
starveAll <- starveRawM %>% select(line,starvation) %>% mutate(starveRawM$f-starve = starveRawF$starvation)

colnames(starveAll) <- c("line", "m-starve", "f-starve")




write.csv(mFin,"/home/nklimko/R/dgrp-starve/mFin.csv")
write.csv(fFin,"/home/nklimko/R/dgrp-starve/fFin.csv")

mFin <- read.csv("/home/nklimko/R/dgrp-starve/mFin.csv")
fFin <- read.csv("/home/nklimko/R/dgrp-starve/fFin.csv")

