


bayesDataF

blupDataF


load('snake/data/go/50_tables/saveTables.Rdata')

genF <- data.table(cbind(allDataF_sorted, bayesF_sorted))

genM <- data.table(cbind(allDataM_sorted, bayesM_sorted))

data2 <- cbind(index=c(1:dim(allDataF)[1]), allDataF)

blup_topF <- cbind(index=1:100, allDataF_sorted[1:100,])
blup_topM <- cbind(index=1:100, allDataM_sorted[1:100,])

bayes_topF <-cbind(index=1:100, bayesF_sorted[1:100,])
bayes_topM <-cbind(index=1:100, bayesM_sorted[1:100,])

save(blup_topF, blup_topM, bayes_topF, bayes_topM, file='data/go/50_tables/enrichenrichTables.Rdata')

#bayesM <- bayes_topM
#bayesF <- bayes_topF

blupM <- allDataM_sorted
blupF <- allDataF_sorted

bayesF <- bayesF_sorted
bayesM <- bayesM_sorted


#blupM <- blup_topM
#blupF <- blup_topF

blupF <- cbind(index=1:2628, blupF)
blupM <- cbind(index=1:2580, blupM)

bayesF <-cbind(index=1:2628, bayesF)
bayesM <-cbind(index=1:2580, bayesM)


save(blupF, blupM, bayesF, bayesM, file='data/go/50_tables/enrichment.Rdata')


topGrid <- function(data, sex, psize, custom.title, custom.Xlab, custom.Ylab){
  plothole <- ggplot(data, aes(x=index, y=cor, label=term))+
    geom_point(color=viridis(1, begin=0.5), size=psize)+
    theme_minimal() +
    labs(x=custom.Xlab, y=custom.Ylab, tag=sex, title=custom.title) +
    theme(text=element_text(size=10), plot.tag = element_text(size=15))
  return(plothole)
}

gg[[1]] <- topGrid(blup_topF, 'F', 1, 'Top Female Results: TBLUP', 'Rank', 'Correlation')
gg[[2]] <- topGrid(blup_topM, 'M', 1, 'Top Male Results: TBLUP', 'Rank', 'Correlation')
gg[[3]] <- topGrid(bayes_topF, 'F', 1, 'Top Female Results: BayesC', 'Rank', 'Correlation')
gg[[4]] <- topGrid(bayes_topM, 'M', 1, 'Top Male Results: BayesC', 'Rank', 'Correlation')

plot_grid(gg[[1]], gg[[2]], gg[[3]], gg[[4]], ncol=2)

#save(gg)
for(i in 1:3){
  
  num <- 25 * (2^i)
  
  pathBase <- 'code/go/enrichment/blup/f/top'
  pathEnd <- '/topHits.txt'
  
  write(blup_topF[1:num,term], file='temp.txt')
  write(blup_topF[,term], file='temp.txt')
  write(blup_topF[,term], file='temp.txt')
  write(blup_topF[,term], file='temp.txt')
  
  
}

savior <- function(data, sex, method){
  
  pathBase <- paste0('code/go/enrichment/', method, '/', sex, '/top')
  pathEnd <- '/topHits.txt'
  
  for(i in 0:2){
    
    num <- 25 * (2^i)
    
    truePath <- paste0(pathBase, num, pathEnd)
    
    write(data[1:num, term], file=truePath)
    
  }
}


savior(blup_topF, 'f', 'blup')
savior(blup_topM, 'm', 'blup')
savior(bayes_topF, 'f', 'bayesC')
savior(bayes_topM, 'm', 'bayesC')

options(knitr.kable.NA = '')


  ulrich <- list.files(path = 'code/go/enrichment', pattern='*finalData.Rds', recursive=TRUE, full.names = TRUE)
  
  
  jonas <- lapply(ulrich, readRDS)

kableWrap <- function(big, med, small, title)  {
  
  #find gap size
  fill50 <- dim(big)[1] - dim(med)[1]
  fill25 <- dim(big)[1] - dim(small)[1]
  #fill gap with NA
  filler50 <- data.table(matrix(rep(NA, 3*fill50), ncol=3))
  filler25 <- data.table(matrix(rep(NA, 3*fill25), ncol=3))
  #bind stopper to real data
  fullMed <- rbind(med, filler50, use.names=FALSE)
  fullSmall <- rbind(small, filler25, use.names=FALSE)
  #join 'equal length tables'
  dataKable <- cbind(big, fullMed, fullSmall)
  colnames(dataKable) <- rep(c('Flybase Gene', 'Count', 'Gene'), 3)
  #print side by side comparison
  kable(dataKable, caption = title, "simple")
  
}

kableWrap(jonas[[1]], jonas[[3]], jonas[[2]], "Female BayesC")

kableWrap(jonas[[4]], jonas[[6]], jonas[[5]], "Male BayesC")

kableWrap(jonas[[7]], jonas[[9]], jonas[[8]], "Female TBLUP")

kableWrap(jonas[[10]], jonas[[12]], jonas[[11]], "Male TBLUP")

ulrich <- list.files(path = 'code/go/enrichment', pattern='*finalData.Rds', recursive=TRUE, full.names = TRUE)
  
jonas <- lapply(ulrich, readRDS)

kableWrap <- function(big, med, small, title)  {
  
  #find gap size
  fill50 <- dim(big)[1] - dim(med)[1]
  fill25 <- dim(big)[1] - dim(small)[1]
  #fill gap with NA
  filler50 <- data.table(matrix(rep(NA, 3*fill50), ncol=3))
  filler25 <- data.table(matrix(rep(NA, 3*fill25), ncol=3))
  #bind stopper to real data
  fullMed <- rbind(med, filler50, use.names=FALSE)
  fullSmall <- rbind(small, filler25, use.names=FALSE)
  #join 'equal length tables'
  dataKable <- cbind(big, fullMed, fullSmall)
  colnames(dataKable) <- rep(c('Flybase Gene', 'Count', 'Gene'), 3)
  #print side by side comparison
  #kable(dataKable, caption = title, "simple")
  return(dataKable)
}

bayesF_Kable <- kableWrap(jonas[[1]], jonas[[3]], jonas[[2]], "Female BayesC")

bayesM_Kable <- kableWrap(jonas[[4]], jonas[[6]], jonas[[5]], "Male BayesC")

blupF_Kable <- kableWrap(jonas[[7]], jonas[[9]], jonas[[8]], "Female TBLUP")

blupM_Kable <- kableWrap(jonas[[10]], jonas[[12]], jonas[[11]], "Male TBLUP")
