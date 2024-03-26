#another day another titled
#
#

temp <- readRDS()

list.dirs(path='data/go/33_metric/sexm/sparse')

points <- list.files(path='data/go/24_goCor/m/sparse/GO.0005811', full.names = TRUE)

holder <- lapply(points, readRDS)

hold <- unlist(holder)


temp <- readRDS('data/go/40_all/sexm/sparseData.Rds')


temp2 <- readRDS('data/go/33_metric/sexm/sparse/termGO.0005811/rowData.Rds')


hold <- sapply(holder, '[[', 1)

id <- c(seq(1:25) * 6 - 5)
hist(hold[id], )

terms <- temp[,3]
cors <- as.numeric(temp[,5])


final <- data.table(term = terms, cor = cors)


final


#SPARSE_M = ['GO.0035008', 'GO.0140042', 'GO.0007485', 'GO.0005811', 'GO.0045819',
#            'GO.0007297', 'GO.0034703', 'GO.1905515', 'GO.0090090', 'GO.0009966']
#
#GO.0035008 0.5116379
#GO.0140042 0.5064311
#GO.0007485 0.5062906
#GO.0005811 0.5009641
#GO.0045819 0.5000330
#
#GO.0007297 0.4801591
#GO.0034703 0.4767065
#GO.1905515 0.4669624
#GO.0090090 0.4553949
#GO.0009966 0.4699759

temp1 <- readRDS('data/go/33_metric/sexm/rmax0.8/rgo0.01/termGO.0035008/rowData.Rds')
temp2 <- readRDS('data/go/33_metric/sexm/rmax0.8/rgo0.01/termGO.0140042/rowData.Rds')
temp3 <- readRDS('data/go/33_metric/sexm/rmax0.8/rgo0.01/termGO.0007485/rowData.Rds')
temp4 <- readRDS('data/go/33_metric/sexm/rmax0.8/rgo0.01/termGO.0005811/rowData.Rds')
temp5 <- readRDS('data/go/33_metric/sexm/rmax0.8/rgo0.01/termGO.0045819/rowData.Rds')

temp6 <- readRDS('data/go/33_metric/sexm/rmax0.8/rgo0.01/termGO.0007297/rowData.Rds')
temp7 <- readRDS('data/go/33_metric/sexm/rmax0.8/rgo0.01/termGO.0034703/rowData.Rds')
temp8 <- readRDS('data/go/33_metric/sexm/rmax0.8/rgo0.01/termGO.1905515/rowData.Rds')
temp9 <- readRDS('data/go/33_metric/sexm/rmax0.8/rgo0.01/termGO.0090090/rowData.Rds')
temp10 <- readRDS('data/go/33_metric/sexm/rmax0.8/rgo0.01/termGO.0009966/rowData.Rds')

tempAll <- rbind(temp1, temp2, temp3, temp4, temp5,temp6,temp7,temp8,temp9, temp10)

termsC <- tempAll[,4]

corsC <- as.numeric(tempAll[,5])

finalBayes <- data.table(term = termsC, cor = corsC)


fB <- cbind(finalBayes, method='bayesC', id=seq(1:10))

fS <- cbind(final, method='sparsegl', id=seq(1:10))

allthod <- rbind(fS, fB)
#allthod <- cbind(final, finalBayes)
#allthod


ggplot(allthod, aes(x=id, y=cor, color=method)) +
  geom_point(size=2)

saveRDS(allthod, file = 'data/go/topCompM.Rds')



allthod[,id]
allthod[,cor]


ggplot




al
