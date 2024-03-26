#hmm today i will gblup graph anlysis first 122 histpgrams tp ;lppok for patterns
#
#

library(viridis)
library(ggplot2)
library(data.table)
gg <- vector(mode='list', length=12)

#
#GO-TBLUP does not perform better than GO-BayesC. 
#Total runtime was around an hour to run all male/female jobs.
#Most of runtime was snakemake overhead, drastically shortened runtime by running sapply across replicates to reduce job count by factor of 25.
#Next step to compare top hits against bayesC top hits. 
#Reran sparsegl with nlambda=500 to same CV minimum warning.
#
#
#Call:  cv.sparsegl(x = W_train, y = y_train, group = groupBase, nlambda = 500)
#
#
#Error measure:  Mean squared error
#
#Warning: the CV minimum occurred at the smallest lambda in the path.
#
#              lambda index   cvm  cvsd nnzero active_grps
#Max.	   0.0707610     1 147.9 18.64      0           0
#lambda.1se 0.0707610     1 147.9 18.64      0           0
#lambda.min 0.0060208   268 134.3 17.06  10809           2
#Min.	   0.0007076   500 134.5 16.98  10828           2

dataList <- list.files(path='data/go/33_metric/sexf/gblup', full.names = TRUE)
dataList <- list.files(pattern = 'gblup', path='data/go/24_goCor/f/0.8/0.01/GO.0000002', full.names = TRUE)

temp2 <- sapply(temp, readRDS)
temp3 <- bind_rows(temp2)

pdata <- readRDS('data/go/33_metric/sexf/gblup/GO.0000002.Rds')
allData <- readRDS('data/go/40_all/sexf/blup_allData.Rds')

temp <- readRDS( "data/go/03_goterms/sparsef100blup/GO.0000922.Rds")

fdata <- readRDS("data/01_matched/f_starvation.Rds")

nullblup <- readRDS("data/sr/40_all/f/cor/all.Rds")

colMeans(nullblup)[3]

geom_hline(yintercept = 0.315)

mBlup <- mNull[,gblup]
fBlup <- fNull[,gblup]

mMean <- mean(mBlup[1:25])
fMean <- mean(fBlup[1:25])

mMean <- mean(mBlup)
fMean <- mean(fBlup)

f25 <- readRDS('data/go/32_nullSum/sexf/nullblup.Rds')
m25 <- readRDS('data/go/32_nullSum/sexm/nullblup.Rds')

fMean <- as.numeric(f25[2])
mMean <- as.numeric(m25[2])


#useful
if(1){
  fNull <- readRDS('data/sr/40_all/f/cor/all.Rds')
  fMean <- mean(fNull[,gblup])
  
  mNull <- readRDS('data/sr/40_all/m/cor/all.Rds')
  mMean <- mean(mNull[,gblup])
  
  allDataF <- readRDS('data/go/40_all/sexf/blup_allData.Rds')
  allDataM <- readRDS('data/go/40_all/sexm/blup_allData.Rds')
  
  
  gg[[1]] <- ggplot(allDataF, aes(x=term, y=cor)) +
    geom_point(color=viridis(1, begin=0.5), size=1.5)+
    geom_hline(yintercept = fMean) +
    labs(x=NULL,y='Correlation', tag='F', title='GO-TBLUP Results Female') 
  
  
  gg[[2]] <- ggplot(allDataM, aes(x=term, y=cor)) +
    geom_point(color=viridis(1, begin=0.5), size=1.5)+
    geom_hline(yintercept = mMean) +
    labs(x=NULL,y='Correlation', tag='M', title='GO-TBLUP Results Male') 
  
  
  plot_grid(gg[[1]], gg[[2]], ncol=2)
  
  #total runtime around 1 hour,
  #
  #
#allDataF_sorted <- allDataF[order(-cor),]
#
#allDataF_sorted[1:20]
#
#allDataM_sorted <- allDataM[order(-cor),]
#
#allDataM_sorted[1:20]

  
}




#SPARSEGL Scratch
#
#
#

temp <- readRDS('data/go/sparse/f/GO.0000002/null_1.Rds')
