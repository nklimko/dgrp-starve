temp <- readRDS('data/sr/36_hplot/f/0.4/0.01/1/hplot.Rds')
plot(temp[,1])
plot(temp[,2])

plot(temp[,1],type='o',cex=.5,
     col=4, xlab="Thinned Step", ylab="Heritability",
      main='Trace of heritability for GO terms')
abline(h=c(0.01,mean(temp[,1])),
       lty=2,col=c(1,2),lwd=2)

plot(temp[,2],type='o',cex=.5,
     col=4, xlab="Thinned Step", ylab="Heritability",
     main='Trace of heritability for nonGO terms')
abline(h=c(0.39,mean(temp[,2])),
       lty=2,col=c(1,2),lwd=2)


plot(temp[,2],type='o',cex=.5,col=4);abline(h=c(0.39,mean(temp[,2])),lty=2,col=c(1,2),lwd=2, title('Trace of heritability for GO terms'))



#mkdir -p max{0.4,0.6,0.8}/term1/id{1..25}
#mkdir -p sex{f,m}/go0.01/max0.8/term{501..1000}/id{1..25}
#
#
#


temppropo



read in row data


feed intercept to 


temp <- readRDS('snake/data/sr/33_metric/go/sexf/rmax0.8/rgo0/term1/rowData.Rds')
temp2 <- readRDS('snake/data/sr/33_metric/go/sexm/rmax0.8/rgo0/term1/rowData.Rds')
