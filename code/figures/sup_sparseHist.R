

if(1){
  #regular
  library(dplyr)
  library(data.table)
  library(ggplot2)
  library(cowplot)
  library(viridis)
  library(ggplot2)
  library(cowplot)
  library(scales)
  library(tidyverse)
  library(ggcorrplot)
  library(melt)
  library(reshape2)
  library(knitr)
  library(gridExtra)
  library(argparse)

  #options
  options(bitmapType = 'cairo')
  options(error = function() traceback(3))

  #seed
  set.seed(123)

  #ggplot holder list
  gg <- vector(mode='list', length=6)
}

ggMakePaper <- function(data, sex, yint, psize, custom.title, custom.Xlab, custom.Ylab){
  plothole <- ggplot(data, aes(factor(term), cor))+
    geom_hline(yintercept = yint)+
    labs(x=custom.Xlab,y=custom.Ylab, tag=sex, title=custom.title) +
    geom_violin(fill = 'pink', width = 0.7) +
    geom_boxplot(color='#440154FF', width = 0.2) +
    theme_minimal()+
    ylim(-0.1, 0.75)+
    theme(axis.text.x = element_blank(),
          text=element_text(size=15),
          legend.position="none",
          plot.tag = element_text(size=10))
  return(plothole)
}

#main call----
plotMake <- function(data, output){
  data <- readRDS(data)
  gg[[1]] <- ggMakePaper(data, 'A', 0, 1, NULL, 'GO Term', 'Prediction Accuracy')

  sparseFig <- arrangeGrob(gg[[1]], nrow=1) #generates g

  ggsave(sparseFig, path='output/figs', filename = output, device = 'pdf', units='in', width=8, height=5)

}


#args----
parser <- ArgumentParser(description= 'snakemake transfer')

parser$add_argument('--data')
parser$add_argument('--output')

snake <- parser$parse_args()
print(str(snake))

#call----
plotMake(snake$data,
         snake$output)

