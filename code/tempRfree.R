#Standard

data <- test

ggTidy <- function(data){
  for(i in 1:dim(data)[2]){
    name <- colnames(data)[i]
    temp <- cbind(rep(name, dim(data)[1]), data[,i, with=FALSE])
    if(i==1){
      hold <- temp
    } else{
      hold <- rbind(hold, temp, use.names=FALSE)
    }
  }
  colnames(hold) <- c("method", "cor")
  hold$method <- factor(hold$method, levels=unique(hold$method))
  return(hold)
}


library(data.table)


mblup <- readRDS('snake/data/go/40_all/sexm/blup_allData.Rds')
fblup <- readRDS('snake/data/go/40_all/sexf/blup_allData.Rds')

mBase <- readRDS('snake/data/go/40_all/sexm/allData.Rds')
mFree <- readRDS('snake/data/go/40_all/sexm/bayesFREEfinal.Rds')

fBase <- readRDS('snake/data/go/40_all/sexf/allData.Rds')
fFree <- readRDS('snake/data/go/40_all/sexf/bayesFREEfinal.Rds')

allBayes <- data.table(mBase, mFree, fBase, fFree)
mBayes <- data.table(mBase, mFree)
fBayes <- data.table(fBase, fFree)

cindex <- c(5,10)
cindex <- c(5,10,15,20)

finalBayes <- allBayes[, cindex, with=FALSE]

names(finalBayes) <- c('Male.8', 'Male.Free', 'Female.8', 'Female.Free')

temp9 <- as.numeric(unlist(finalBayes))
restore <- data.table(matrix(temp9, ncol=4))

names(restore) <- c('m8', 'mFree', 'f8', 'fFree')


mFilt <- mBayes[, cindex, with=FALSE]
fFilt <- fBayes[, cindex, with=FALSE]

temp8 <- as.numeric(unlist(mFilt))
mData <- data.table(matrix(temp8, ncol=2))

temp7 <- as.numeric(unlist(fFilt))
fData <- data.table(matrix(temp7, ncol=2))



malf <- ggTidy(mData)
falf <- ggTidy(fData)


mAll <- cbind(mData, mblup[,2])
names(mAll) <- c('R2_0.8', 'R2_Unlimited', 'GO-TBLUP')
malf <- ggTidy(mAll)


fAll <- cbind(fData, fblup[,2])
names(fAll) <- c('R2_0.8', 'R2_Unlimited', 'GO-TBLUP')
falf <- ggTidy(fAll)


#plotmaker function
ggMake3 <- function(data, sex, yint, psize, custom.title, custom.Xlab, custom.Ylab){
  plothole <- ggplot(data,aes(y=cor,x=method, fill=method))+
    geom_hline(yintercept = yint)+
    labs(x=custom.Xlab,y=custom.Ylab, tag=sex, title=custom.title) +
    geom_violin(color = 'blue', width = 0.65) +
    geom_boxplot(color='#440154FF', width = 0.15) +
    theme_minimal()+
    scale_fill_viridis(discrete=TRUE, begin=0.2)+
    theme(axis.text.x = element_text(angle = 90),
          text=element_text(size=25),
          plot.tag = element_text(size=20),
          legend.position = 'none') +
    stat_summary(fun=mean, color='#440154FF', geom='point', 
                 shape=18, size=3, show.legend=FALSE)
  return(plothole)
}


ggMake3(malf, 'M', 0.25, 1, 'Prediction Accuracy by Method in Females', 'Method', 'Prediction Accuracy')

ggMake3(falf, 'F', 0.2, 1, 'Prediction Accuracy by Method in Females', 'Method', 'Prediction Accuracy')




library(ggplot2)
library(data.table)



##############
#cov checking

start <- 50
stop <- 100
increment <- 50
data <- mAll

corCheck <- function(data, start, stop, increment){
  
  rindex <- seq(start, stop, increment)
  dorder <- data[order(-R2_Unlimited), ]
  hold <- c(0,0,0,0)
  names(hold) <- c('top', 'free:8', 'free:blup', '8:blup')
  
  
  for(i in rindex){
    subSpace <- 1:i
    
    rSub <- dorder[subSpace,]
    c1 <- cor(rSub[,2], rSub[,1])
    c2 <- cor(rSub[,2], rSub[,3])
    c3 <- cor(rSub[,1], rSub[,3])
    
    corce <- c(i, c1, c2, c3)
    
    print(i)
    print(corce)
    
    hold <- rbind(hold, corce)
  }
  
  
  final <- hold[-1,]
  return(final)
}

corCheckMute <- function(data, start, stop, increment){
  
  rindex <- seq(start, stop, increment)
  dorder <- data[order(-Free), ]
  hold <- c(0,0,0,0)
  names(hold) <- c('top', 'free:8', 'free:blup', '8:blup')
  for(i in rindex){
    subSpace <- 1:i
    
    rSub <- dorder[subSpace,]
    c1 <- cor(rSub[,2], rSub[,1])
    c2 <- cor(rSub[,2], rSub[,3])
    c3 <- cor(rSub[,1], rSub[,3])
    
    corce <- c(i, c1, c2, c3)
    #print(i)
    #print(corce)
    
    hold <- rbind(hold, corce)
  }
  
  final <- hold[-1,]
  return(final)
}


test <- corCheck(mAll, 50, 2550, 100)


test2 <- corCheckMute(mAll, 25, 2575, 25)
tt2 <- data.table(test2)
ttF2 <- tidy2(tt2)
ggplot(ttF2, aes(x=method, y=cor, color=index)) +
  geom_point() +
    scale_color_viridis(discrete = TRUE, labels=c('Bayes Unlimited : Bayes 0.8','Bayes 0.8 : TBLUP','Bayes Unlimited : TBLUP')) +
  theme_minimal()



test3 <- corCheckMute(fAll, 25, 2625, 25)
tt3 <- data.table(test3)
ttF3 <- tidy2(tt3)
ggplot(ttF3, aes(x=method, y=cor, color=index)) +
  geom_point() +
    scale_color_viridis(discrete = TRUE, labels=c('Bayes Unlimited : Bayes 0.8','Bayes 0.8 : TBLUP','Bayes Unlimited : TBLUP')) +
  theme_minimal()


ggplot(data, aes(x=method, y=cor, color=index)) +
  geom_point() +
    scale_color_viridis(discrete = TRUE, labels=c('Bayes Unlimited : Bayes 0.8','Bayes 0.8 : TBLUP','Bayes Unlimited : TBLUP')) +
  theme_minimal()



mPreds <- malf
fPreds <- falf
mCors <- ttF2
fCors <- ttF3

save(mPreds, fPreds, mCors, fCors, file='data/rmdTables/goRcomp/allTables.Rdata')



tt <- data.table(test)

#colnames(test) <- c('x', 'y', 'z', 'a')

testTable

data <- tt

tidy2 <- function(data){
  len <- dim(data)[1]  
  raw <- unlist(data)
  
  method <- rep(raw[1:len], 3)
  cor <- raw[seq(len+1,length(raw))]
  
  index <- unlist(c(rep('a', len), rep('b', len), rep('c', len)))
  
  final <- data.table(method, index, cor)
  return(final)  
}

ttF <- tidy2(tt)

unlist(tt)

mesh <- c(unlist(rep(tt[,1], 3)), rbind(unlist(tt[,2]), unlist(tt[,3]), unlist(tt[,4])))


testTable <- ttF

testUp <- ggTidy(testTable)

saveRDS(ttF, 'zz_lost/now.Rds')

ttF <- readRDS('zz_lost/now.Rds')

ggplot(ttF, aes(x=method, y=cor, color=index)) +
  geom_point() +
    scale_color_viridis(discrete = TRUE, labels=c('Bayes Unlimited : Bayes 0.8','Bayes 0.8 : TBLUP','Bayes Unlimited : TBLUP')) +
  theme_minimal()

  
  
  r1 <- restore[order(-V1),]
rSub <- r1[1:250]
cor(r1[,1], r1[,2])
cor(rSub[,1], rSub[,2])


names(restore) <- c('Male.8', 'Male.Free', 'Female.8', 'Female.Free')

bayesMeans <- colMeans(restore)

names(bayesMeans) <- c('Male 0.8', 'Male Free', 'Female 0.8', 'Female Free')

bayesMeans

meat <- list.files('snake/data/sr/33_metric/f/cor', full.names=TRUE)

fData <- sapply(meat, readRDS)

tofu <- list.files('snake/data/sr/33_metric/m/cor', full.names=TRUE)

mData <- sapply(tofu, readRDS)

finale <- data.table(names=c('lasso', 'mr.ash', 'varbvs'),f = as.numeric(fData[c(2,4,6)]), m = as.numeric(mData[c(2,4,6)]))



a <- matrix(seq(1:12), ncol=3)


a %*% transpose(a)

cov(a, t(a))

G <- tcrossprod(a)/ncol(a)
