


allPath <- 'data/go/24_goCor/m/sparseCatch'

allDirs <- list.dirs(allPath)
allDirs <- allDirs[-1]#trim header

gg <- vector(mode='list', length=12)

i <- allDirs[1]


i <- allDirs[2:3]


allDirsTemp <- allDirs[2:3]


corAttempt <- function(input){
  fileList <- list.files(input, full.names = TRUE)
  dataBlob <- lapply(fileList, readRDS)
  allCors <- 0
  for(i in 1:length(dataBlob)){
    allCors[i] <- dataBlob[[i]][1]
  }
  meanCor <- mean(unlist(allCors))
  return(meanCor)
}

dataBlob[[1]][1]

final <- sapply(allDirs, corAttempt)

final

saveRDS(final, 'data/rawGL.Rds')


corAttempt(i)


temp <- readRDS("data/go/24_goCor/m/sparseCatch/GO.0000014/1.Rds") 
#data.table::rbindlist(lapply(setNames(rds_list, rds_list), readRDS), idcol = "file")

temp$cor

temp[1]
