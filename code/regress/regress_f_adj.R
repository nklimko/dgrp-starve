library(dplyr)
library(data.table)
library(doParallel)

registerDoParallel(cores = 12)

# file path for final table
filePath <- "/data/morgante_lab/nklimko/rep/dgrp-starve/data/fRegress_adj.txt"

# gene expression data from earlier bound to line and starvation
f_adj <- fread("/data/morgante_lab/nklimko/rep/dgrp-starve/data/f_adj.txt")
start <- 3 
end <- 11340
#f 11340 m 13577

# foreach creates a list with index indicating position in loop function grabs the 
# p-value from correlation of starvation to expression of trait
pvalList <- foreach(i=start:end) %dopar% {
  temp <- summary(lm(f_adj[,c(2,i), with=FALSE], na.action=na.omit))[[4]][8]
}

# converts list to vector
part <- unlist(pvalList, use.names = FALSE)

# binds p-vals to column index
id <- start:end 
part <- data.table(id,pvalList)

# write table to file
fwrite(part, filePath)
