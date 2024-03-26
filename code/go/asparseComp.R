library(data.table)

Row <- readRDS('snake/data/go/33_metric/sexf/sparseFREE/termGO.0007059/rowData.Rds')
Row2 <- rbind(Row, Row)

data <- Row2

sparseNorm <- readRDS('snake/data/go/40_all/sexf/sparseData.Rds')
sparseFREE <- readRDS('snake/data/go/40_all/sexf/sparseFREEData.Rds')

sparseNorm
sparseFREE

#rows of two terms
index <- c(, )

sparseData <- sparseFREE[sparseNorm, on = .(term), nomatch = NULL]

sparseData <- data.table(sparseNorm[index,], sparseFREE)

sparseData

covCheck <- cov(sparseData[,c(2,3)])

covCheck

new 

dataList <- list.files('snake/data/go/24_goCor/f/sparseFREE/GO.0007059', full.names = TRUE)

cors <- sapply(dataList, readRDS)
go1 <- unlist(cors[1,])



dataList2 <- list.files('snake/data/go/24_goCor/f/sparseFREE/GO.0045819', full.names = TRUE)

cors2 <- sapply(dataList2, readRDS)
go2 <- unlist(cors2[1,])

final <- data.table(GO.0007059=go1, GO.0045819=go2)

saveRDS(final, file='snake/data/go/50_tables/sparse/final1.Rds')

#asparse=0.2
cov(final)



# OLD sparseGL runs, asparse=0.05

dataList3 <- list.files('snake/data/go/24_goCor/f/randSparse/GO.0007059', full.names = TRUE)

cors3 <- sapply(dataList3, readRDS)

go3 <- sapply(cors3,"[[",1)

saveRDS(go3, file='snake/data/go/50_tables/sparse/go3.Rds')

dataList4 <- list.files('snake/data/go/24_goCor/f/randSparse/GO.0045819', full.names = TRUE)

cors4 <- sapply(dataList4, readRDS)

go4 <- sapply(cors4,"[[",1)

saveRDS(go4, file='snake/data/go/50_tables/sparse/go4.Rds')

final2 <- data.table(GO.0007059=go3, GO.0045819=go4)

#asparse=0.05
cov(final2)



finalComp <- data.table(old7059=go3, old45819=go4, new7059=go1,new45819=go2)


saveRDS(finalComp, file='snake/data/go/50_tables/sparse/allGO.Rds')

covComp <- cov(finalComp)

saveRDS(covComp, file='snake/data/go/50_tables/sparse/allGO_cov.Rds')

# 0.009 cor improvement, <3% difference
colMeans(finalComp)
covComp



Higher asparse slightly improves prediction accuracy(0.009 improvement)

Covariance slightly decreases with improved asparse(beneficial)

Variance within asparse sets still equivalent to covariance(var = cov for asparse = 0.05; var = cov for asparse = 0.2) 




