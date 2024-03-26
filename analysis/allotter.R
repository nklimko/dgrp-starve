

pipLoop <- function(sex){
  
  xpPath <- paste0('data/01_matched/', sex,'_starvation.Rds')
  xp <- readRDS(xpPath)
  genes <- colnames(xp[,-1])
  
  plotTerms <- list.dirs(path=paste0('data/go/25_fit/sex', sex), full.names = FALSE)
  
  plotTerms <- plotTerms[-1]
  
  for(term in plotTerms){
    print(paste0('working on term ', term))
    
    idPath <- paste0('data/go/03_goterms/sex', sex,'/', term, '.Rds')
    fitPath <- paste0('data/go/25_fit/sex', sex,'/', term, '/bayesFull.Rds')
    
    outPathGO <- paste0('data/go/26_pip/sex', sex,'/', term, '_GO.Rds')
    outPathNON<- paste0('data/go/26_pip/sex', sex,'/', term, '_NON.Rds')
    
    ids <- readRDS(idPath)
    raw <- readRDS(fitPath)
    
    pipGO <- raw$fit$ETA[[1]]$d
    pipNON <- raw$fit$ETA[[2]]$d
    
    
    goData <- data.table(index=c(1:length(pipGO)), pip=pipGO, gene=genes[ids])
    nonData <- data.table(index=c(1:length(pipNON)), pip=pipNON, gene=genes[-ids])
    
    GOplot <- plotPIP(goData, toupper(sex), 0.5, 1, 0.4, paste0(term, " PIP"), 'Index', 'PIP')
    NONplot <- plotPIP(nonData, toupper(sex), 0.7, 0.1, 0.6, paste0("Non-", term, " PIP"), 'Index', 'PIP')
    
    saveRDS(GOplot, outPathGO)
    saveRDS(NONplot, outPathNON)
    
    goFilt <- goData[pip > 0.5]
    nonFilt <- goData[pip > 0.7]
    
    filtPathGO <- paste0('data/go/27_pipoff/sex', sex,'/GO/', term, '.Rds')
    filtPathNON<- paste0('data/go/27_pipoff/sex', sex,'/NON/', term, '.Rds')
    
    saveRDS(goFilt, filtPathGO)
    saveRDS(nonFilt, filtPathNON)
    
  }
}

plotPIP <- function(data, sex, cutoff, pointSize, colorVal, custom.title, custom.Xlab, custom.Ylab){
  plothole <- ggplot(data, aes(x= index, y=pip, label=gene))+
    geom_point(color=viridis(1, begin=colorVal), size=pointSize)+
    geom_text(aes(label=ifelse(pip>cutoff, as.character(gene),'')),hjust=1,vjust=0, angle=90, size=2)+
    theme_minimal() +
    labs(x=custom.Xlab,y=custom.Ylab, tag=sex, title=custom.title) +
    theme(text=element_text(size=8),
          plot.tag = element_text(size=15))
  return(plothole)
}

#sex <- 'm'
#term <- 'GO.0140042'



  #sex <- 'f'
  #sex <- 'm'
pipLoop('f')
pipLoop('m')
