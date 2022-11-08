# this is going to be the script for looking at difMinus

library("data.table")
library(dplyr)

starve <- tibble(read.csv("data/starveAll.csv")) %>% select(-X)

test <- fread("data/starveAll.csv")

group1 <- test[dif < 0,line]


group1 <- data.table(group1)
colnames(group1) <- "line"
line
group1

fwrite(group1, "/data/morgante_lab/nklimko/rep/dgrp-starve/data/group1.txt")

newG <- fread("/data/morgante_lab/nklimko/rep/dgrp-starve/data/group1.txt")
group1 <- newG[,line]

group1

newG




colnames(group1) <- "line"
length(group1)

product[,line]
product
starve





read in index file
all <- fread("/data/morgante_lab/data/dgrp/genotypes/dgrp2_tgeno_filtered_meanimputed.txt")

dim(all)
subsetDF <- all[1:200]
dim(subsetDF)


fwrite(subsetDF, "/data/morgante_lab/nklimko/rep/dgrp-starve/data/testData.txt")




Grouping model:
group 1: filter genotype by lines of interest
group 2: filter genotype by lines not of interest

for all four main groups()

calculate mean of SNP for every SNP for each subgroup -> 2x1.8mil table


ratio (group1)/(group2+1e-9) so any zero doesn''t throw an error

arrange ratios and grab top/bottom 100 and save 



0.00000000001
1e-9

any value greater than 1 has higher SNP variation?? unsure of what data indicates
value close to zero indicates knockout in group of interest


?SNPRelate

GENE EXPRESSION DATA

read in index file

Grouping model:
  group 1: filter genotype by lines of interest
group 2: filter genotype by lines not of interest

for all four main groups()

calculate mean of SNP for every SNP for each subgroup -> 2x1.8mil table


ratio (group1)/(group2+1e-9) so any zero doesn''t throw an error


arrange ratios and grab top/bottom 100 and save




