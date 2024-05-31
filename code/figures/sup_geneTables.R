
a <- readRDS('snake/data/gene/2_goLookup/f/bayes/allCounts.Rds')
b <- readRDS('snake/data/gene/2_goLookup/f/tblup/allCounts.Rds')
c <- readRDS('snake/data/gene/2_goLookup/m/bayes/allCounts.Rds')
d <- readRDS('snake/data/gene/2_goLookup/m/tblup/allCounts.Rds')

names(a) <- c('Gene', 'Count')
names(b) <- c('Gene', 'Count')
names(c) <- c('Gene', 'Count')
names(d) <- c('Gene', 'Count')

write.csv(a, file.path(path='output/tabs', file='sup_geneBayesF.csv'))
write.csv(b, file.path(path='output/tabs', file='sup_geneTblupF.csv'))
write.csv(c, file.path(path='output/tabs', file='sup_geneBayesM.csv'))
write.csv(d, file.path(path='output/tabs', file='sup_geneTblupM.csv'))
