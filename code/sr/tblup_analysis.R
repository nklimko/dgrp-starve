#tblup_analysis.R

library(BGLR)
h2_goPath <- 'snake/data/bglr/m/tblup_nbt/30000_10000_25_1ETA_1_varU.dat'
#30000_10000_25_1ETA_1_varU.dat

B <- readBinMat('snake/data/bglr/m/tblup_nbt/30000_10000_25_1ETA_1_varU.dat')

#h2 calculation----
if(0){
  # X1
  h2_0 <- 0.2
  B=readBinMat('data/bglr/h2_ETA_1_b.bin')
  B <- scan(h2_goPath)
  B=readBinMat(h2_goPath)
  h2_new=rep(NA,nrow(B))
  varU_new=h2_new
  varE_new=h2_new
  for(i in 1:length(h2_new)){
    u=X1%*%B[i,]
    varU_new[i]=var(u)
    varE_new[i]=var(y-u)
    h2_new[i]=varU_new[i]/(varU_new[i]+varE_new[i])
  }
  
  # X2
  h2_0 <- 0.001
  B2=readBinMat('data/bglr/h2_ETA_2_b.bin')
  h2_new2=rep(NA,nrow(B))
  varU_new2=h2_new2
  varE_new2=h2_new2
  for(i in 1:length(h2_new2)){
    u2=X2%*%B2[i,]
    varU_new2[i]=var(u2)
    varE_new2[i]=var(y-u2)
    h2_new2[i]=varU_new2[i]/(varU_new2[i]+varE_new2[i])
  }
}

#h2 results----
if(0){
  #mean h2 for each eta
  mean(h2_new)
  mean(h2_new2)
  
  
  sd(h2_new)
  
  sd(h2_new2)
  
  quantile(h2_new, probs = 0.025)
  quantile(h2_new, probs = 0.5)
  quantile(h2_new, probs = 0.975)
  
  quantile(h2_new2, probs = 0.025)
  quantile(h2_new2, probs = 0.5)
  quantile(h2_new2, probs = 0.975)
  
  #plots
  plot(h2_new,type='o',cex=.5,col=4);abline(h=c(h2_0,mean(h2_new)),lty=2,col=c(1,2),lwd=2, title('Trace of heritability for GO terms'))
  plot(h2_new2,type='o',cex=.5,col=4);abline(h=c(h2_0,mean(h2_new2)),lty=2,col=c(1,2),lwd=2, title('Trace of heritability for non-GO terms'))
  
  #true b1 to model b1, etc
  cor(b1, fitBGLR$ETA[[1]]$b)
  cor(b2, fitBGLR$ETA[[2]]$b)
  
  #unsure
  sqrt(var(fitBGLR$ETA[[1]]$b))
  sqrt(var(fitBGLR$ETA[[2]]$b))
  
  hist(h2_new)
  hist(h2_new2)
}



library(BGLR)
data(wheat)
y=wheat.Y[,1] ; X=scale(wheat.X)
dir.create('test_saveEffects')
setwd('test_saveEffects')
fm=BGLR(y=y,ETA=list(list(X=X,model='BayesB',saveEffects=TRUE)),nIter=12000,thin=2,burnIn=2000)
B=readBinMat('test_saveEffects/ETA_1_b.bin')




#fm=BGLR(y=y,ETA=list(list(X=X,model='BRR',saveEffects=T)),nIter=6000,burnIn=1000,verbose=F)
h2_0 <- 0.8
#h2_goPath <- 'snake/data/bglr/m/tblup_nbt/30000_10000_25_1ETA_1_varU.dat'
Upath <- 'snake/data/bglr/m/tblup_nbt/55000_10000_25_1ETA_1_varU.dat'
Epath <- 'snake/data/bglr/m/tblup_nbt/55000_10000_25_1varE.dat'

varU=scan(Upath)
varE=scan(Epath)
h2=varU/(varU+varE)

jpeg(outPath)

plot(h2,type='o',cex=.5,col=4, main=title);abline(h=c(h2_0,mean(h2[-c(1:200)])),lty=2,col=c(1,2),lwd=2)

dev.off()



temp <- readRDS('snake/data/sr/40_all/m/srQuarterFinal.Rds')

    tblupDat <- readRDS('snake/data/sr/33_metric/m/cor/tblup.Rds')
    
    
temp[order(temp[,2]),]   
