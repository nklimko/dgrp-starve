#!/usr/bin/env Rscript
#aggregates Column Rds files into a data.table
#formerly 30_top3_summary.R

#setup----
library(dplyr)
library(data.table)
library(argparse)

options(error = function() traceback(3))
set.seed(1)

#test----
if(0){
  dataList <- c('data/01_isoadjust/m_cafe.Rds','data/01_isoadjust/m_free.glucose.Rds')
  outPath <- 'hjunk.Rds'
  xpPath <- 'data/00_raw/xp_f'
  
  multimatch(dataList, xpPath, outPath)
}

#function----
corSummary <- function(dataList, outPath){
  
  hold <- readRDS(dataList[1])
  
  for (i in 2:length(dataList)) {
    print(i)
    hold <- cbind(hold, readRDS(dataList[i]))
  }

  saveRDS(hold, outPath)
  
}

parser <- ArgumentParser(description= 'snakemake transfer')

parser$add_argument('--dataList', nargs='+')
parser$add_argument('--outPath')

snake <- parser$parse_args()
print(str(snake))

#call----
corSummary(snake$dataList,
           snake$outPath)
