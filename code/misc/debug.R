
#col join all Rds's separetely, similar to all.Rds
#extract cor from all, combine all mean cor

male <- readRDS("data/sr/03_goterms/m/1.Rds")
female <- readRDS("data/sr/03_goterms/f/1.Rds")


temp <- readRDS("data/sr/24_goCor/f/bayesC_1_1.Rds")

700/24#lapply functions----

columnSearch <- function(term, data){
  data[data[,1]==term,2]
}


keel2 <- function(term, data){
  data[term]
}

filtLength <- function(data, filtNum){
  
  if(length(data) >= filtNum){
    return(1)
  }else{
    return(0)
  }
}

#name/read data
inPath <- "data/go/uniqCols"
listPath <- "data/go/uniqGO"

goData <- read.delim(inPath, sep=' ')
goList <- read.delim(listPath)

#split by go term: all fbgn per term
final <- lapply(unlist(goList), columnSearch, data=goData)

#names of 
final2 <- lapply(final, filtLength, filtNum=5)
step1 <- unlist(final2)
havoc <- data.frame(gene=names(step1), pass=step1)
query <- havoc[havoc[,2]==1,1]

saveRDS(query, "data/go/query.Rds")


malware <- lapply(query, keel2, data=final)

thatone <- function(term, data){
  
  sublevel <- unlist(term)
  termHits <- lapply(sublevel, keel, data=data)
  return(termHits)
}


xpPath <- "data/sr/10_matched/m_starvation.Rds"
xpPath1 <- "data/sr/10_matched/f_starvation.Rds"

yX <- readRDS(xpPath)

X <- yX[,-1]

geneCols <- colnames(X)
#1:length(geneCols)


geneMap <- cbind(term=geneCols, index=1:length(geneCols))


giorno <- lapply(malware, thatone, data=geneMap)

#boolean gated
if(0){
  #female
  for(i in 1:length(giorno)){
    ids <- as.numeric(as.vector(unlist(giorno[i])))
    go_subset <- i
    type <- 'f'
    filename <- paste0("data/sr/03_goterms/", type, "/", go_subset, '.Rds')
    saveRDS(ids, file=filename)
  }
  
  #male
  for(i in 1:length(giorno)){
    ids <- as.numeric(as.vector(unlist(giorno[i])))
    go_subset <- i
    type <- 'm'
    filename <- paste0("data/sr/03_goterms/", type, "/", go_subset, '.Rds')
    saveRDS(ids, file=filename)
  }
}




if(old){
  
  dim(Xtrain)
  dim(Ytrain)
  length(S0)
  length(S0_base)
  length(w0_init)
  
  y_calc[1]
  
  temp <- matrix(y_calcBase, ncol=2)
  
  
  #SAMPLE:
  #> dim(Xtrain)
  #[1] 100 800
  #> dim(Ytrain)
  #[1] 100   5
  #> length(S0)
  #[1] 151
  #> length(S0_base)
  #[1] 10
  #> length(w0_init)
  #[1] 151
  
  
  #REAL
  #> dim(Xtrain)
  #[1]   159 13575
  #> dim(Ytrain)
  #[1] 159   2
  #> length(S0)
  #[1] 151
  #> length(S0_base)
  #[1] 10
  #> length(w0_init)
  #[1] 151
  
  
  
  
  
  #4:27 start
  
  
  
  Xtrain <- scale(X)
  Ytrain <- matrix(unlist(y), ncol=traitCount)
  
  Xtest <- scale(X[test_IDs,])
  Ytest <- matrix(unlist(y[test_IDs]), ncol=traitCount)
  
  
  #residual covariance mr.mash
  fit$V
  
  #genetic covariance mr.mash
  fit$G
  
  
  giorno
  
  
  
  length(passed)
  
  #match bool to columns
  temp <- cbind(final, final2)
  
  toyGO <- data.frame(term=c('GOhelp', 'GOgettim'))
  
  toyGo <- list(toyGO)
  
  termCol <- rep(unlist(toyGO), 2)
  
  toyData <- data.frame(termCol, ind=seq(1,4))
  
  
  toyGO <- c('GOhelp', 'GOgettim')
  
  #match query to genes and pull genes per set
  
  #bind colnames to a sequence of 1:lengthcolnames
  
  lapply(toyGO, keel, data=toyData)
  
  
  
  
  
  
  lapply(goMap, keel, data=geneMap)
  
  
  
  bigTest <- rep(seq(1,5),5)
  bigData <- seq(1,25)
  
  bigTable <- cbind(bigTest, bigData)
  
  query <- seq(1,5)
  
  lapply(query, keel, data=bigTable)
  
  
  function:
    recover genes for term
  
  match column indices
  
  save numbers to id File
  
  
  
  
  
  
  #manually index
  
  #filter by >= 5
  
  #take indices
  
  filter go terms
  
  extract to list
  
  
  
  
}










