

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

histmake <- function(path, sex, custom.title, custom.Xlab, custom.Ylab, cutoff){
  data <- readRDS(path)

  final <- ggplot(data, aes(x=count))+
    geom_bar()+
    geom_vline(xintercept = cutoff)+
    labs(x=custom.Xlab, y=custom.Ylab, tag=sex, title=custom.title) +
    theme(text=element_text(size=10), plot.tag = element_text(size=15))
  theme_minimal()
  return(final)
}

suphistMake <- function(bayesPathF, bayesPathM, tblupPathF, tblupPathM, output){

  gg[[1]] <- histmake(bayesPathF, "A", NULL, "Hits", "Count", 3.5)
  gg[[2]] <- histmake(tblupPathF, "B", NULL, "Hits", "Count", 4.5)

  gg[[3]] <- histmake(bayesPathM, "C", NULL, "Hits", "Count", 3.5)
  gg[[4]] <- histmake(tblupPathM, "D", NULL, "Hits", "Count", 3.5)

  #plot_grid(gg[[1]], gg[[2]], gg[[3]], gg[[4]], ncol=2)
  suphist <- arrangeGrob(gg[[1]], gg[[2]], gg[[3]], gg[[4]], nrow=2)

  ggsave(suphist, path='output/figs', filename = output, device = 'pdf', units='in', width=7.5, height=6)
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
suphistMake(snake$bayesPathF,
            snake$bayesPathM,
            snake$tblupPathF,
            snake$tblupPathM,
            snake$output)

