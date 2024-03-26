temp <- readRDS(path)

path <- 'data/go/24_goCor/f/sparse/GO.0000002/sparsegl_2.Rds'


fit <- temp$fit
fit
str(fit)

dataList <- list.files(path='data/go/24_goCor/m/sparse/GO.0005811', full.names = TRUE)



hold <- sapply(holder, '[[', 1)

fiery <- 

'data/go/33_metric/sexf/sparse/term{goterm}/rowData.Rds'
du -csh



temp <- readRDS('data/go/40_all/sexm/allData.Rds')




rowReader <- function(dataList, cutoff, allPath, topPath){
  
  #read in all data
  rowsRead <- lapply(dataList, readRDS)
  
  #bind using dplyr binds rows to condense list to data.table)
  data <- data.table(bind_rows(rowsRead))
  
  #columns to set as factors
  #change cor from char to numeric type
  facs <- c('sex','rmax','rgo')
  data[, (facs) := lapply(.SD, as.factor), .SDcols=facs]
  data[, cor := as.numeric(cor)]
  
  saveRDS(data, allPath)
  
  data[order(-cor),]
  
}

dataList <- list.dirs(path='data/go/33_metric/sexf/sparseComp', full.names=TRUE)
dataList <- paste0(dataList, '/rowData.Rds')
dataList <- dataList[-1]
bayesComp <- lapply(dataList, readRDS)
bayesData <- data.table(bind_rows(bayesComp))


bayesData <- bayesData[,4:5] 

saveRDS(bayesTopComp, file='data/go/bayesComp.Rds')


'data/go/33_metric/sexf/sparse/term{goterm}/rowData.Rds'
dataList <- list.files(path='data/go/33_metric/sexf/sparse', full.names=TRUE)
dataLis#dataList <- paste0(dataList, '/rowData.Rds')
dataList <- dataList[-1]
sparseComp <- lapply(dataList, readRDS)
sparseData <- data.table(bind_rows(sparseComp))




merge_test <- merge(dataA, dataB, by="A", all.data=TRUE)
sparseData[bayesData, on = 'term', bayescor := i.cor]



plot(sparseData[,cor])

sparseData[bayesData, ]

ggplot(allthod, aes(x=id, y=cor, color=method)) +
  geom_point(size=2)




time2 <- readRDS("data/go/24_goCor/f/sparse/GO.0000022/sparsegl_2.Rds")
time3 <- readRDS("data/go/24_goCor/f/sparse/GO.0000022/sparsegl_3.Rds")
time4 <- readRDS("data/go/24_goCor/f/sparse/GO.0000022/sparsegl_4.Rds")
time5 <- readRDS("data/go/24_goCor/f/sparse/GO.0000022/sparsegl_5.Rds")

time2 <- readRDS("data/go/24_goCor/f/sparse/GO.0000014/sparsegl_2.Rds")
time3 <- readRDS("data/go/24_goCor/f/sparse/GO.0000014/sparsegl_3.Rds")
time4 <- readRDS("data/go/24_goCor/f/sparse/GO.0000014/sparsegl_4.Rds")
time5 <- readRDS("data/go/24_goCor/f/sparse/GO.0000014/sparsegl_5.Rds")

time2 <- readRDS("data/go/24_goCor/f/sparse/GO.0000002/sparsegl_2.Rds")
time3 <- readRDS("data/go/24_goCor/f/sparse/GO.0000002/sparsegl_3.Rds")
time4 <- readRDS("data/go/24_goCor/f/sparse/GO.0000002/sparsegl_4.Rds")
time5 <- readRDS("data/go/24_goCor/f/sparse/GO.0000002/sparsegl_5.Rds")

allTime <- c(time2$time[3]/60,time3$time[3]/60,time4$time[3]/60,time5$time[3]/60)

mean(allTime)



debug <- list.files(path='data/go/24_goCor/f/sparse/GO.0000014', full.names=TRUE)

horror <- sapply(debug, readRDS)




fit <- horror[3]



###sparseFilter xtras
###
###
###terms <- list.dirs("data/go/24_goCor/f/sparse", full.names=TRUE)
terms <- terms[-1]

for(i in terms){
  ids <- list.files(path=i, full.names=TRUE)
  print(ids)
  tabl <- lapply(ids, readRDS)
  
  tableFinal <- lapply(tabl, `[[`, 1)   
  meanCor <- mean(unlist(tableFinal))
  
  
  term <- unlist(strsplit(i, '/'))[6]
  
  outPath <- paste0('data/go/33_metric/sexf/sparse/', term,'.Rds')
  
  final <- data.table(term=term, cor=meanCor)
  saveRDS(final, outPath)
  
        }