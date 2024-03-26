a <- ggplot(mtcars, aes(x=wt, y=mpg)) + geom_point() +
  ggtitle("Fuel Efficiency of 32 Cars") +
  xlab("Weight (x1000 lb)") + ylab("Miles per Gallon") +
  theme(text=element_text(size=16,  family="Arial Nova MS"))
print(a)

choose_font("Arial Nova MS", quiet=TRUE)
font_import(pattern="LiberationSans")


source('/data2/morgante_lab/nklimko/rep/dgrp-starve/code/setup.R')

source('code/setup.R')


#the error of my ways cannot continue here
#



dataOldPre <- list.files(path='snake/data/go/24_goCor/m/0.8/0.01/GO.0035008', full.names = TRUE, pattern = 'bayesC')
dataNew <- list.files(path='snake/data/go/24_goCor/m/bayesFREE/GO.0035008', full.names = TRUE)

oddOut <- c(12,20,23)

#2 4 7
#12 20 23

dataOld <- dataOldPre[-oddOut]



oldForm <- sapply(dataOld, readRDS)


newForm <- sapply(dataNew, readRDS)
compTab <- data.table(old=as.numeric(oldForm[1,]), new=as.numeric(newForm[1,]))
compTab
colMeans(compTab)

goterm <- 'GO.0007485'

# GO.1900075
# 
goRead <- function(goterm){
  oldPath <- paste0('snake/data/go/24_goCor/m/0.8/0.01/', goterm)
  newPath <- paste0('snake/data/go/24_goCor/m/bayesFREE/',goterm)
  
  dataOld <- list.files(path=oldPath, full.names = TRUE, pattern = 'bayesC')
  dataNew <- list.files(path=newPath, full.names = TRUE)
  if(length(dataOld) != length(dataNew)){
    print('dimFail')
    return(c(old=0,new=0))
  }else{
    
    oldForm <- sapply(dataOld, readRDS)
    newForm <- sapply(dataNew, readRDS)
    compTab2 <- data.table(old=as.numeric(oldForm[1,]), new=as.numeric(newForm[1,]))
    #print(compTab2)
    print(colMeans(compTab2))
    return(colMeans(compTab2))
  }
}




 #top 3-8, random 5
allTerms <- c('GO.0042461',
              'GO.0033500',
              'GO.0007485',
              'GO.0005811',
              'GO.0045819',
              'GO.0035556',
              'GO.0101005',
              'GO.0040014',
              'GO.0090163',
              'GO.0071577')



tempRes <- sapply(allTerms, goRead)
#rowMeans(tempRes)

freeComp <- data.table(old=tempRes[1,], new=tempRes[2,])

saveRDS(freeComp, file='snake/data/go/50_tables/freeComp.Rds')

freeComp <- readRDS('snake/data/go/50_tables/freeComp.Rds')

# R=0.05 vs. R2 undefined
freeComp

# top 5 means
colMeans(freeComp[1:5,])

# random term means
colMeans(freeComp[6:10,])
