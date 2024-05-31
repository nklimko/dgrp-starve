
library(ggplot2)
library(data.table)
library(viridisLite)
library(cowplot)
library(argparse)
library(gridExtra)

#options
options(bitmapType = 'cairo')
options(error = function() traceback(3))

#seed
set.seed(123)

#ggplot holder list
gg <- vector(mode='list', length=12)


maxGrid <- function(dataPath, ordered, sex, psize, custom.title, custom.Xlab, custom.Ylab, yint, cutoff, color, lim.low, lim.high){
  core <- readRDS(dataPath)
  len <- dim(core)[1]

  index <- 1:len
  cutdex <- as.integer(len*cutoff)

  orderBase <- core[order(-cor),]

  trueCutoff <- as.numeric(orderBase[cutdex, 2, with=FALSE])

  if(ordered){ core <- orderBase }

  data <- data.table(index, core)
  plothole <- ggplot(data, aes(x=index, y=cor, label=term))+
    geom_point(color=viridis(1, begin=color, option='turbo'), size=psize)+
    theme_minimal() +
    ylim(lim.low,lim.high)+
    geom_hline(yintercept = yint)+
    geom_hline(yintercept = trueCutoff, linetype="dashed")+
    labs(x=custom.Xlab, y=custom.Ylab, tag=sex, title=custom.title) +
    theme(text=element_text(size=10), plot.tag = element_text(size=15), axis.text.x = element_blank())
  return(plothole)
}


fig2Make <- function(bayesPathF, bayesPathM, tblupPathF, tblupPathM, output){

  allYintF <- readRDS('snake/data/sr/40_all/f/meanData.Rds')
  allYintM <- readRDS('snake/data/sr/40_all/m/meanData.Rds')

  bayesYintF <- as.numeric(allYintF[5,2])
  bayesYintM <- as.numeric(allYintM[5,2])
  tblupYintF <- as.numeric(allYintF[8,2])
  tblupYintM <- as.numeric(allYintM[8,2])

  gg[[1]] <- maxGrid(bayesPathF, 0, 'A', 0.25, NULL, 'GO Term', 'Prediction Accuracy', bayesYintF, 0.01, color=0.9, 0.19, 0.49)
  gg[[2]] <- maxGrid(tblupPathF, 0, 'B', 0.25, NULL, 'GO Term', 'Prediction Accuracy', tblupYintF, 0.01, color=0.9, 0.19, 0.49)

  gg[[3]] <- maxGrid(bayesPathM, 0, 'C', 0.25, NULL, 'GO Term', 'Prediction Accuracy', bayesYintM, 0.01, color=0.1, 0.275, 0.54)
  gg[[4]] <- maxGrid(tblupPathM, 0, 'D', 0.25, NULL, 'GO Term', 'Prediction Accuracy', tblupYintM, 0.01, color=0.1, 0.275, 0.54)

  #plot_grid(gg[[1]], gg[[2]], gg[[3]], gg[[4]], ncol=2)
  go2x2 <- arrangeGrob(gg[[1]], gg[[2]], gg[[3]], gg[[4]], nrow=2)

  ggsave(go2x2, path='output/figs', filename = output, device = 'pdf', units='in', width=7.5, height=6)
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
fig2Make(snake$bayesPathF,
         snake$bayesPathM,
         snake$tblupPathF,
         snake$tblupPathM,
         snake$output)

