

### BGLR

start <- Sys.time()

fitBGLR2 <- BGLR::BGLR(y_train, response_type = "gaussian", ETA = list(list(X=W_train, model="BayesC")), nIter = 130000, burnIn = 30000, thin = 50)

lap <- Sys.time()

#y_calc <- predict(fitBGLR, W_test)
#corResult <- cor(y_test, y_calc)

#stop <- Sys.time()

lap - start

#stop - start


#fm$ETA[[1]]$plotIn

#saveRDS(fm, "data/fm.burglar")

fm <- fitBGLR2

#head("ETA_1_parBayesC.dat")

#4# Trace plots
#list.files()

# Residual variance
varE<-scan('varE.dat')
plot(varE,type='o',col=2,cex=.5,ylab=expression(var[e]));
abline(h=fm$varE,col=4,lwd=2);
abline(v=fm$burnIn/fm$thin,col=4)

# Samples...
TMP=read.table('ETA_1_parBayesC.dat',header=T)
plot(TMP[,1],type='o',col=4, cex=0.5)
abline(h=fm$ETA[[1]]$probIn,col=4,lwd=2)
abline(v=fm$burnIn/fm$thin,col=4)

data