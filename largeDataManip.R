


test <- tibble(read.csv("/data/morgante_lab/data/dgrp/genotypes/dgrp2_tgeno_filtered_meanimputed.txt"))


colnames(test)
print(test[1,1])



library(data.table)
imputed <- "/data/morgante_lab/data/dgrp/genotypes/dgrp2_tgeno_filtered_meanimputed.txt"

?fread
testFive <- fread(file=imputed, nrows=5)
testAll <- fread(file=imputed)
testFive

dim(testAll)
dim(testFive)



multiply genotype value of each member of ingroup and compare to outgroup. 
strict cutoff would be multiplication to map 




for gene expression, look at fold change of interest group 




janky numbers are fragments of imputation, 
imputation is performed by finding mean of !Null values, gives representative effect




0.117021276595745 * 188

22/188
205 - 17
188
24/205
22/188
