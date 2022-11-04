rm(list=ls())

#libraries
library(ggplot2)
library(dplyr)
library(tidyr)
#library("workflowr")


#install.packages("workflowr")


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


###Difference between male and female

#rename columns for combination
colnames(starveRawF) <- c("line", "fStarve")
colnames(starveRawM) <- c("line", "mStarve")

#Combine mf
starveAll <- starveRawF %>% mutate(mStarve = starveRawM$mStarve)

#compute difference: F - Ma means difference is quantity of F above M
starveDiff <- starveAll %>% select(line, fStarve, mStarve) %>% mutate(diff = fStarve- mStarve) 

#histogram
hist(starveDiff$diff, main=paste("Histogram of Starvation Diff"), xlab="Starvation Difference")

#progress function
saveTib("starveDiff", starveDiff)
dFin <- restore("starveDiff")
starveDiff <- dFin %>% select(line, fStarve, mStarve, diff)

#Summary stats
summary(starveBoth$starvation)
summary(starveDiff$fStarve)
summary(starveDiff$mStarve)
summary(starveDiff$diff)
        

?dnorm
?mids

x <- starveDiff$fStarve
y <- starveDiff$mStarve

plot(x,y, abline(reg=lm(y~x), col="red"))


par(mfrow=c(2,2))

qqnorm(x)
qqline(x)

qqnorm(y)
qqline(y)



hist(x)

?seq
xMod <- seq(min(x), max(x), length = 100)
yMod <- dnorm(xMod, mean=mean(x), sd=sd(x))


?diff
yMod <- yMod * diff(hist_data$mids[1:2]) * length(x) 

xMod
lines(xMod, yMod)

#not sure what this means, not helpful yet
ggplot(starveAll, aes(x)) +
  geom_histogram(aes(y = ..density..), fill='lightgray', col='black') +
  stat_function(fun = dnorm, args = list(mean=mean(x), sd=sd(x)))




lm(formula = y~x)

#scatterplot, qq/normality

wflow_git_push()
install.packages("git2r")
par(mfrow=c(1,2))

#Comparative Boxplot
boxplot(starveDiff$mStarve, starveDiff$fStarve, names=c("Male", "Female"), main="Sex Comparison of Starvation Resistance", xlab="Starvation Resistance", ylab="Sex", horizontal = TRUE)
hist(starveDiff$diff, main=paste("Histogram of Starvation Diff"), xlab="Starvation Difference")
