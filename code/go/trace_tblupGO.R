#!/usr/bin/env Rscript

#setup----
library(data.table)
library(dplyr)
library(argparse)
library(ggplot2)
library(BGLR)

options(error = function() traceback(3))
set.seed(123)
#ggplot holder list
gg <- vector(mode='list', length=4)

if(0){
  march <- list.files(path='snake/data/bglr/m/nbt_tblup/GO.0035008/250000_50000_25/1', full.names = T)

  vGO <- scan(march[1])
  vNON <- scan(march[2])
  vE <- scan(march[4])
  MU <- scan(march[3])
}

h2Plot <- function(varGO,
                   varNON,
                   mu,
                   varE,
                   title,
                   root){

  vGO <- scan(varGO)
  vNON <- scan(varNON)
  MU <- scan(mu)
  vE <- scan(varE)


  ggDataMake <- function(input){
    data <- data.table(index=1:length(input), cust=input)
    return(data)
  }

  goData <- ggDataMake(vGO)
  nonData<- ggDataMake(vNON)
  eData<- ggDataMake(vE)
  muData <- ggDataMake(MU)


  ggTrace <- function(data, sex, custom.title, custom.Xlab, custom.Ylab){
    plothole <- ggplot(data=data, aes(y=cust,x=index))+
      labs(x=custom.Xlab, y=custom.Ylab, tag=sex, title=custom.title) +
      geom_line()+
      theme_minimal()
    return(plothole)
  }


  title1 <- paste0('GO_', title)
  title2 <- paste0('NON_', title)
  title3 <- paste0('MU_', title)
  title4 <- paste0('varE_', title)

  gg[[1]] <- ggTrace(goData,  'M', title1, 'Index', 'GO')
  gg[[2]] <- ggTrace(nonData, 'M', title2, 'Index', 'NON')
  gg[[3]] <- ggTrace(muData,  'M', title3, 'Index', 'MU')
  gg[[4]] <- ggTrace(eData,   'M', title4, 'Index', 'varE')


  print('here')

  ggsave('go.png', path=root, plot=gg[[1]])
  ggsave('non.png', path=root, plot=gg[[2]])
  ggsave('mu.png', path=root, plot=gg[[3]])
  ggsave('e.png', path=root, plot=gg[[4]])


  print('here')
  #temp <- getwd()
  #print(temp)
  #print(outPath)
  #ggsave(outFile, path=outPath, plot=gg[[1]])
  print('there')

  #png(file=outPath)
  #plot(h2,type='o',cex=.5,col=4, main=title);abline(h=c(h2_0,mean(h2[-c(1:200)])),lty=2,col=c(1,2),lwd=2)
  #dev.off()


}

#args----
parser <- ArgumentParser(description= 'snakemake transfer')

#All inputs from snakemake config file
parser$add_argument('--varGO')
parser$add_argument('--varNON')
parser$add_argument('--mu')
parser$add_argument('--varE')
parser$add_argument('--title')
parser$add_argument('--root')


snake <- parser$parse_args()
print(str(snake)) #debug print

#call----
h2Plot(snake$varGO,
       snake$varNON,
       snake$mu,
       snake$varE,
       snake$title,
       snake$root)
