
f <- readRDS('snake/data/sr/40_all/f/meanData.Rds')
m <- readRDS('snake/data/sr/40_all/m/meanData.Rds')

names(f) <- c('Method', 'Accuracy', 'SE')
names(m) <- c('Method', 'Accuracy', 'SE')

write.csv(f, file.path(path='output/tabs', file='sup_standardF.csv'))
write.csv(m, file.path(path='output/tabs', file='sup_standardM.csv'))
