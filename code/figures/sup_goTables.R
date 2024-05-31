
a <- readRDS('snake/data/go/40_all/f/new_bayes.Rds')
b <- readRDS('snake/data/go/40_all/f/new_blup.Rds')
c <- readRDS('snake/data/go/40_all/m/new_bayes.Rds')
d <- readRDS('snake/data/go/40_all/m/new_blup.Rds')

names(a) <- c('Term', 'Accuracy', 'SE')
names(b) <- c('Term', 'Accuracy', 'SE')
names(c) <- c('Term', 'Accuracy', 'SE')
names(d) <- c('Term', 'Accuracy', 'SE')

write.csv(a, file.path(path='output/tabs', file='sup_goBayesF.csv'))
write.csv(b, file.path(path='output/tabs', file='sup_goTblupF.csv'))
write.csv(c, file.path(path='output/tabs', file='sup_goBayesM.csv'))
write.csv(d, file.path(path='output/tabs', file='sup_goTblupM.csv'))
