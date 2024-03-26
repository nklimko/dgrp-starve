#######################################
#
#  Noah Klimkowski
#  fourLinePrep.R
#  11/8/2022
#
#  Walkthrough of line selection
#  for SNP and ingroup creation
#
#######################################


##### Libraries
library(dplyr)
library(tidyr)
library(data.table)

#Read in data
starve <- tibble(read.csv("data/starve.csv")) %>% select(-X)

#Manual inspection of data to create cutoffs for ingroups
print(arrange(starve, starve$dif), n=205)
print(arrange(starve, starve$avg), n=205)

# Lines with male SR high than female SR
difMinus <- starve %>% filter(dif < 0) %>% arrange(line) %>% select(line)
difMinus <- as.data.table(difMinus)
fwrite(difMinus, "../data/difMinus.txt")

# Lines with largest gap from male SR to female SR
difPlus <- starve %>% filter(dif >= 30) %>% arrange(line) %>% select(line)
difPlus <- as.data.table(difPlus)
fwrite(difPlus, "../data/difPlus.txt")

# Lowest average SR between males and females
avgMinus <- starve %>% filter(avg < 40) %>% arrange(line) %>% select(line)
avgMinus <- as.data.table(avgMinus)
fwrite(avgMinus, "../data/avgMinus.txt")

# Highest average SR between males and females
avgPlus <- starve %>% filter(dif >= 30) %>% arrange(line) %>% select(line)
avgPlus <- as.data.table(avgPlus)
fwrite(avgPlus, "../data/avgPlus.txt")


