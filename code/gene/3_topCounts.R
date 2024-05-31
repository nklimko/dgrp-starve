#!/usr/bin/env Rscript

#setup----
library(dplyr)
library(data.table)
library(tidyverse)
library(argparse)

#options
options(bitmapType = "cairo")
options(error = function() traceback(3))

#seed
set.seed(123)


topCounts <- function(input, cutoff, hrPath, output){
  data <- readRDS(input)
  
  final <- data[which(count > cutoff),]
  
  write(unlist(final[,1]), file=hrPath)
  saveRDS(final, output)
  
}

#args----
parser <- ArgumentParser(description= 'snakemake transfer')

parser$add_argument('--input')
parser$add_argument('--cutoff', type="integer")
parser$add_argument('--hrPath')
parser$add_argument('--output')

snake <- parser$parse_args()
print(str(snake))

topCounts(snake$input,
          snake$cutoff,
          snake$hrPath,
          snake$output)
