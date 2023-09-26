library(dplyr)
library(data.table)
options(error = function() traceback(3))
set.seed(1)

if(0){
  dataList <- c('data/01_isoadjust/m_cafe.Rds','data/01_isoadjust/m_free.glucose.Rds')
  outPath <- 'hjunk.Rds'
  xpPath <- 'data/00_raw/xp_f'
  
  multimatch(dataList, xpPath, outPath)
}


multimatch <- function(dataList, xpPath, outPath){
  pheno <- data.table(line = names(readRDS(dataList[1])))
  
  for (i in 1:length(dataList)) {
    #print(dataList[i])
    pheno <- cbind(pheno, readRDS(dataList[i]))
    names(pheno)[i+1] <- paste0('trait',i)
  }
  
  xp <- fread(xpPath)
  
  merged <- merge(pheno, xp, by.x = 'line', by.y = 'line', all = FALSE)
  
  lineTrim <- merged[,-1]
  
  
  #clear NA values
  final <- na.omit(lineTrim)
  
  saveRDS(final, outPath)
  
}



print(paste0('in: ',snakemake@input[['data']]))
print(paste0('out: ',snakemake@output[[1]]))

multimatch(snakemake@input[['data']],
           snakemake@input[['xp']],
           snakemake@output[[1]])
