library(data.table)
library(ggplot2)
library(argparse)
library(gridExtra)
library(viridisLite)


#options
options(bitmapType = 'cairo')
options(error = function() traceback(3))

#seed
set.seed(123)

#ggplot holder list
gg <- vector(mode='list', length=12)

#functions----
ggMakeCorPaper <- function(data, tag, psize, custom.Xlab, custom.Ylab, limStart, limStop, color, corLabel){
  plothole <- ggplot(data, aes(x=Bayes, y=TBLUP))+
    labs(x=custom.Xlab,y=custom.Ylab, tag=tag) +
    geom_point(color=viridis(1, begin=color, option='turbo'), size=psize) +
    geom_smooth(method=lm, color='black')+
    xlim(limStart, limStop)+
    ylim(limStart, limStop)+
    annotate("text", x=limStop-0.1, y=limStart+0.05, label=corLabel) +
    theme_minimal()+
    theme(text=element_text(size=10),
          plot.tag = element_text(size=15))
  return(plothole)
}


fig3Make <- function(bayesPathF, bayesPathM, tblupPathF, tblupPathM, output){

  bayesF <- readRDS(bayesPathF)
  bayesM <- readRDS(bayesPathM)
  tblupF <- readRDS(tblupPathF)
  tblupM <- readRDS(tblupPathM)

  fAll <- bayesF[tblupF, on=.(term)]
  fData <- fAll[,c(1,2,4), with=FALSE]
  names(fData) <- c('Term', 'Bayes', 'TBLUP')

  mAll <- bayesM[tblupM, on=.(term)]
  mData <- mAll[,c(1,2,4), with=FALSE]
  names(mData) <- c('Term', 'Bayes', 'TBLUP')

  corF <- cor(fData[,Bayes], fData[,TBLUP])
  corM <- cor(mData[,Bayes], mData[,TBLUP])
  corLabelF <- paste0('r = ', round(corF, digits = 4))
  corLabelM <- paste0('r = ', round(corM, digits = 4))


  gg[[1]] <- ggMakeCorPaper(fData, 'A', 0.25, 'GO-BayesC Prediction Accuracy', 'GO-TBLUP Prediction Accuracy', 0.15, 0.5, 0.9, corLabelF)
  gg[[2]] <- ggMakeCorPaper(mData, 'B', 0.25, 'GO-BayesC Prediction Accuracy', 'GO-TBLUP Prediction Accuracy', 0.25, 0.55, 0.1, corLabelM)

  #plot_grid(gg[[1]], gg[[2]], ncol=2)

  scatter <- arrangeGrob(gg[[1]], gg[[2]], nrow=1)

  ggsave(scatter, path='output/figs', filename = output, device = 'pdf', units='in', width=7.5, height=3.75)
  #ggsave(go2x2, filename = 'go_2x2_paper_0.25.tiff', device = 'tiff', units='px', width=2000, height=1500, dpi=300)

}


#args----
parser <- ArgumentParser(description= 'snakemake transfer')

parser$add_argument('--bayesPathF')
parser$add_argument('--bayesPathM')
parser$add_argument('--tblupPathF')
parser$add_argument('--tblupPathM')
parser$add_argument('--output')

snake <- parser$parse_args()
print(str(snake))

#call----
fig3Make(snake$bayesPathF,
         snake$bayesPathM,
         snake$tblupPathF,
         snake$tblupPathM,
         snake$output)
