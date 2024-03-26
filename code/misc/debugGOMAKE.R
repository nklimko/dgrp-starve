
#col join all Rds's separetely, similar to all.Rds
#extract cor from all, combine all mean cor
#
#
#
#

df <- data.table(
  id = c(10,11,12,13,14,15,16,17),
  name = c('sai','ram','deepika','sahithi','kumar','scott','Don','Lin'),
  gender = c('M','M','F','F','M','M','M','F'),
  dob = as.Date(c('1990-10-02','1981-3-24','1987-6-14','1985-8-16',
                  '1995-03-02','1991-6-21','1986-3-24','1990-8-26')),
  state = c('CA','NY',NA,NA,'DC','DW','AZ','PH'),
  row.names=c('r1','r2','r3','r4','r5','r6','r7','r8')
)

df[unique(df$gender), state]

df[gender=='M', state]

unique(df$gender)

lore <- c('M', 'F')


lapply()

temp9 <- function(i, df){
  hits <- df[gender==i, state]
  final <- list(i, hits)
  return(final)
  
}



novice <- lapply(lore, temp9, df=df)


hits <- df[gender==i, state]


# Using df[]
df[df$gender == 'M', 'id']

# Using subset()
subset(df,gender == 'M',select = 'id')

# Create example data frame 
data <- data.frame(x1 = c(10,20,50,10,90,30,40),  
                   x2 = c(50,20,10,80,90,100,60), 
                   x3 = c(0,10,30,50,70,40,80))     

#  print the dataframe 
data 

# Convert rows to list 
list <- split(data,seq(nrow(data)))    

# print the list 
list 



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
#inPath <- "data/go/uniqCols"
#listPath <- "data/go/uniqGO"

inPath <- "/data2/morgante_lab/nklimko/rep/dgrp-starve/data/fb/go/test/uniqCols"
listPath <- "/data2/morgante_lab/nklimko/rep/dgrp-starve/data/fb/go/test/uniqGO"

goData <- read.delim(inPath, sep=' ')
goList <- read.delim(listPath, header = FALSE)

colnames(goData) <- c('term', 'gene')
colnames(goList) <- 'term'

goData[1:5,]
goList[1:5,]



temp8 <- function(i, df){
  hits <- df[1==i, 2]
  final <- list(i, hits)
  return(final)
  
}

#####THIS WORKS
splitList <- split(goData[,2], goData[,1])

#I need to match list hits to column ids of expression per sex
xpPath <- "data/sr/10_matched/m_starvation.Rds"
yX <- readRDS(xpPath)
X <- data.table(yX[,-1])

geneCols <- colnames(X)
geneMap <- cbind(gene=geneCols, index=as.numeric(1:length(geneCols)))

tempgo <- splitList[[1]]


avem <- function(tempgo, geneMap){
  matching_rows <- geneMap[,1] %in% tempgo
  result_df <- geneMap[matching_rows, 2]
  
  return(result_df)
  
}

#are we gonna have solar on the new building? yes maam, i BEGGED for it
#$https://www.raspberrypi.com/tutorials/cluster-raspberry-pi-tutorial/
growth <- lapply(splitList, avem, geneMap=geneMap)

result_df



# find non-complete elements
ids.to.remove <- sapply(growth, function(i) length(i) < 5)
# remove found elements
longmire <- growth[!ids.to.remove]
# create data.frame


lastcall <- function(ids, term){
    type <- 'm'
    filename <- paste0("data/sr/03_gotermsTEST/", type, "/", term, '.Rds')
    saveRDS(ids, file=filename)
  
}

#for i in 1:length(longmire){
 type <- 'm'
 
for(i in 1:length(longmire)){
    term <- names(longmire)[i]
    term <- chartr(':', '.', term)
    ids <- as.numeric(longmire[[i]])
    
    filename <- paste0("data/sr/03_gotermsTEST/", type, "/", term, '.Rds')
    print(filename)
    print(ids)
    saveRDS(ids, file=filename)
}


wrath <- readRDS('data/sr/03_gotermsTEST/m/GO.0000014.Rds')

X[,..wrath]

lapply(longmire, lastcall, term=names(longmire))


df <- do.call(rbind, longmire)



temp <- as.vector(goList)

final <- lapply(goList, temp8, df=goData)

#split by go term: all fbgn per term
#final <- lapply(unlist(goList), columnSearch, data=goData)
final <- lapply(goList, columnSearch, data=goData)

#names of 
final2 <- lapply(final, filtLength, filtNum=5)
step1 <- unlist(final2)
havoc <- data.frame(gene=names(step1), pass=step1)
query <- havoc[havoc[,2]==1,1]

#saveRDS(query, "data/go/query.Rds")
saveRDS(query, "data/go/queryTEST.Rds")


malware <- lapply(query, keel2, data=final)

thatone <- function(term, data){
  
  sublevel <- unlist(term)
  termHits <- lapply(sublevel, keel, data=data)
  return(termHits)
}

thatone <- function(term, data){
  
  sublevel <- unlist(term)
  termHits <- lapply(sublevel, columnSearch, data=data)
  return(termHits)
}


xpPath <- "data/sr/10_matched/m_starvation.Rds"
xpPath1 <- "data/sr/10_matched/f_starvation.Rds"

yX <- readRDS(xpPath)

X <- yX[,-1]

geneCols <- colnames(X)
#1:length(geneCols)


geneMap <- cbind(gene=geneCols, index=as.numeric(1:length(geneCols)))

#yup
giorno <- lapply(malware, thatone, data=geneMap)

#boolean gated
if(0){
  #female
  for(i in 1:length(giorno)){
    ids <- as.numeric(as.vector(unlist(giorno[i])))
    go_subset <- i
    type <- 'f'
    #filename <- paste0("data/sr/03_goterms/", type, "/", go_subset, '.Rds')
    filename <- paste0("data/sr/03_gotermsTEST/", type, "/", go_subset, '.Rds')
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










