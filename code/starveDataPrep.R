#######################################
#  Noah Klimkowski
#  starveDataPrep.R
#  11/7/2022
#
#  Walkthrough of data cleaning and
#  column calculation for starvation
#  resistance in female/male lines
#
#######################################

 
##### Libraries
library(dplyr)
library(tidyr)

#create csv from hist, deprecated
histCsv <- function(csvPath, dataSet){
  
  #Read in table
  allRaw <- tibble(read.csv(csvPath))
  
  starveRaw <- allRaw %>% select(line,starvation) %>% filter(!is.na(starvation))
  
  #histogram
  #hist(starveRaw$starvation, main=paste("Histogram of",dataSet,"Starvation"), xlab="Starvation Resistance")
  return(starveRaw)
}

#plot space for hist
#par(mfrow=c(1,2))

#Female case
csvPathF <- "/data/morgante_lab/data/dgrp/phenotypes/eQTL_traits_females.csv"
dataSetF <- "Female"
starveF <- histCsv(csvPathF, dataSetF)

#Male case
csvPathM <- "/data/morgante_lab/data/dgrp/phenotypes/eQTL_traits_males.csv"
dataSetM <- "Male"
starveM <- histCsv(csvPathM, dataSetM)

#rename columns for combination
colnames(starveF) <- c("line", "f")
colnames(starveM) <- c("line", "m")

#Combine mf
starve <- starveF %>% mutate(m = starveM$m)

#compute difference and average
starve <- starve %>% select(line, f, m) %>% mutate(dif = f - m, avg = (f+m)/2)

#Sort by difference, cosmetic
starve <- arrange(starve, starve$dif)

#Save
write.csv(starve, "/data/morgante_lab/nklimko/rep/dgrp-starve/data/starve.csv")
