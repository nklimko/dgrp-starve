
a <- readRDS('snake/data/gene/3_topCounts/f/bayes/topCounts.Rds')
b <- readRDS('snake/data/gene/3_topCounts/f/tblup/topCounts.Rds')
c <- readRDS('snake/data/gene/3_topCounts/m/bayes/topCounts.Rds')
d <- readRDS('snake/data/gene/3_topCounts/m/tblup/topCounts.Rds')

names(a) <- c('Gene', 'Count')
names(b) <- c('Gene', 'Count')
names(c) <- c('Gene', 'Count')
names(d) <- c('Gene', 'Count')

write.csv(a, file.path(path='output/tabs', file='geneBayesF.csv'))
write.csv(b, file.path(path='output/tabs', file='geneTblupF.csv'))
write.csv(c, file.path(path='output/tabs', file='geneBayesM.csv'))
write.csv(d, file.path(path='output/tabs', file='geneTblupM.csv'))
