library(dplyr)
library(data.table)
options(error = function() traceback(3))
set.seed(1)

if(FALSE){
  xpPath <- '/data2/morgante_lab/data/dgrp/expression/combined_samples_known_novel_fpkm_VR_NoWol_F_line_means_rename_noflag_transp_common.txt'
  phenoPath <- '/data2/morgante_lab/data/dgrp/phenotypes/eQTL_traits_females.csv'
  outPath <- 'rawCheck2.Rds'
  trait <- 'starvation'
  
  adjustRDS(xpPath, phenoPath, trait, outPath)
}

rawToFinal <- function(xpPath, phenoPath, outPath, trait){
  
  pheno <- fread(phenoPath)
  xp <- fread(xpPath)
  
  dat <- data.frame(id=pheno[,line], pheno[,trait, with=FALSE])
  
  y <- adjustPheno(dat, trait)
  
  ydt <- data.table(line=names(y),trait=y)
  
  merged <- merge(ydt, xp, by.x='line', by.y='line', all=FALSE)
  
  final <- merged[,-1]
  
  saveRDS(final, outPath)
}


print(c(snakemake@input[[1]],
        snakemake@input[[2]],
        snakemake@output[[1]],
        snakemake@resources[['trait']]))

rawToFinal(snakemake@input[[1]],
           snakemake@input[[2]],
           snakemake@output[[1]],
           snakemake@params[['trait']])

