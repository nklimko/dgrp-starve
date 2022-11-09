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

### Function: create tibble from data path
antiNull <- function(csvPath){
  
  #Read in table
  allRaw <- tibble(read.csv(csvPath))
  
  #filter for non-null values
  starveRaw <- allRaw %>% select(line,starvation) %>% filter(!is.na(starvation))
  
  return(starveRaw)
}

#Female data
csvPathF <- "/data/morgante_lab/data/dgrp/phenotypes/eQTL_traits_females.csv"
starveF <- antiNull(csvPathF)

#Male data
csvPathM <- "/data/morgante_lab/data/dgrp/phenotypes/eQTL_traits_males.csv"
starveM <- antiNull(csvPathM)

#rename columns for combination
colnames(starveF) <- c("line", "f")
colnames(starveM) <- c("line", "m")

#Combine data
starve <- starveF %>% mutate(m = starveM$m)

#compute difference and average
starve <- starve %>% select(line, f, m) %>% mutate(dif = f - m, avg = (f+m)/2)

#Sort by difference, cosmetic
starve <- arrange(starve, starve$dif)

#Save, colnames are lin,f,m,dif,avg
write.csv(starve, "/data/morgante_lab/nklimko/rep/dgrp-starve/data/starve.csv")
