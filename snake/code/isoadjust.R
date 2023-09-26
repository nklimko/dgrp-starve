library(dplyr)
library(data.table)
options(error = function() traceback(3))
set.seed(1)

if(0){
  phenoPath <- 'data/00_raw/pheno_m'
  outPath <- 'hjunk.Rds'
  trait <- 'cafe'
  isoAdjust(phenoPath, outPath, trait)
}

#load adjustData environment with adjustPheno function
load("/data2/morgante_lab/data/dgrp/misc/adjustData.RData")

#isolates trait and adjusts phenotype
isoAdjust <- function(phenoPath, outPath, trait){
  
  #load phenotype csv
  pheno <- fread(phenoPath)
  
  #extract line and trait
  dat <- data.frame(id=pheno[,line], pheno[,trait, with=FALSE])
  
  #adjust phenotype for inversion and wolbachia resistance by line
  final <- adjustPheno(dat, trait)
  
  #save result to snakemake output
  saveRDS(final, outPath)
}

isoAdjust(snakemake@input[[1]],
          snakemake@output[[1]],
          snakemake@params[['trait']])

