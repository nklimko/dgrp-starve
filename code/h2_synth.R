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
gg <- vector(mode='list', length=2)


h2Plot <- function(vU,
                   vE,
                   title,
                   h2_0,
                   outPath,
                   outFile){
  
  #outPath <- 'snake/data/bglr/plots/m/tblup_nbt/30000_5000_50.png'
  
  varU=scan(vU)
  varE=scan(vE)
  h2=varU/(varU+varE)
  
  data <- data.table(index=1:length(h2), h2=h2)
  
  ggMakeH2 <- function(data, sex, h2_0, custom.title, custom.Xlab, custom.Ylab){
    plothole <- ggplot(data=data, aes(y=h2,x=index))+
      labs(x=custom.Xlab,y=custom.Ylab, tag=sex, title=custom.title) +
      geom_line()+
      geom_hline(yintercept = h2_0) +
      theme_minimal()
    return(plothole)
  }
  
  gg[[1]] <- ggMakeH2(data, 'M', h2_0, title, 'Index', 'h2')
  
  print('here')
  temp <- getwd()
  print(temp)
  print(outPath)
  ggsave(outFile, path=outPath, plot=gg[[1]])
  print('there')
  
  #png(file=outPath)
  #plot(h2,type='o',cex=.5,col=4, main=title);abline(h=c(h2_0,mean(h2[-c(1:200)])),lty=2,col=c(1,2),lwd=2)
  #dev.off()
}

#args----
parser <- ArgumentParser(description= 'snakemake transfer')

#All inputs from snakemake config file
parser$add_argument('--varU')
parser$add_argument('--varE')
parser$add_argument('--title')
parser$add_argument('--h2_0', type='double')
parser$add_argument('--outPath')
parser$add_argument('--outFile')


snake <- parser$parse_args()
print(str(snake)) #debug print

#call----
h2Plot(snake$varU,
       snake$varE,
       snake$title,
       snake$h2_0,
       snake$outPath,
       snake$outFile)
