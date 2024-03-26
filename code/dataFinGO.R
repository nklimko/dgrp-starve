
temp <- na.omit(readRDS('snake/data/sr/40_all/go/sexf/allData.Rds'))

facs <- matrix(as.factor(unlist(temp[,1:4])), ncol=4)
cors <- as.numeric(unlist(temp[,5]))

data <- data.table(facs, cors)
colnames(dataFin) <- c('sex', 'rmax', 'rgo', 'term', 'cor')

dataM <- dataFin[dataFin$sex=='m',]
