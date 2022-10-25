rm(list=ls())

mdata <- read.csv("/data/morgante_lab/data/dgrp/phenotypes/eQTL_traits_males.csv")
mdata
dim(mdata)
colnames(mdata)
mdata[,11]
mstarve <- mdata[,c(1,11)]
mstarve

fdata <- read.csv("/data/morgante_lab/data/dgrp/phenotypes/eQTL_traits_females.csv")
fdata
dim(fdata)
colnames(fdata)
fdata[,10]
fstarve <- fdata[,c(1,10)]
fstarve

colnames(mstarve) <- c("m-line", "m-starvation")
colnames(fstarve) <- c("f-line", "f-starvation")

mstarve
fstarve

mdim <- dim(mstarve)[1]



indMark <- 0
for(i in 1:mdim){
  if(is.na(mstarve[i,2])){
    indMark <- c(indMark, i)
    #print("null detect")
  } else{
    #nothing occurs
  }
}

indFin <- indMark[2:length(indMark)]
mfinal <- mstarve[-indFin,]
dim(mfinal)


hist(mfinal[,2])
