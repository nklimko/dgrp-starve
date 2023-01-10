


#read in expression data
fMeans <- fread("data/fMeans.txt")

fMeans[1:5,1:5]

fStarve <- fread("data/starve-f.txt")

fMeans <- fMeans[order(line)]
fStarve <- fStarve[order(line)]

dim(fMeans)
dim(fStarve)

fGoal <- fMeans[,1:2,with=FALSE]

fGoal
fStarve


fFin <- fStarve[fGoal, on=.(line)]

dim(fFin)

fFin


fMeans[1:5,1:5]
#test

fLine <- fMeans[,line]

fVector <- fStarve[,line]

length(fVector)
length(fLine)

fFinal <- fLine[fVector, on=line]

fFinal <- fVector[fLine, on=line]

length(fFinal)
dim(fFinal)

fStarve[1:5,1:2]

round2 <- fStarve[order(line)]

round2[1:5,1:2]


dim(fMeans)
dim(fStarve)



dim(Y)
dim(b)

?rnorm


Y%*%b

Y <- t(Y)


transpose(b)



if(TRUE){
  
  #dimensions and number of fixed effect genes
  #n <- dim(Y)[1]
  #p <- dim(Y)[2]
  
  #keep
  
  #error is seeded rnorm, number of rows(lines)
  #e <- rnorm(n, 1, 0.25)
  #e <- rnorm(n)
  
  # add gene names to p val list
  #geneNames <- colnames(fMeans)[3:11340]
  #fReg <- fReg[, gene:=geneNames]
  
  ###sorted p values
  
  # LOW pval
  #pSort <- fReg[order(pvalList)]
  
  #HIGH pval
  #pSort <- fReg[order(-pvalList)]
  
  # fixed effect vector USELESS, must be matched to certain values 
  #b <- c(rnorm(p_effect, mean=6), rep(0, p-p_effect))
  
  #affix vector to sorted p values
  #pSort[,b:=b]
  
  #restore sort order to id/alphabetical gene, matches expression data order
  #fFin <- pSort[order(id)]
  
  # PROPER fixed effect vector PROPER with proper indexing
  #b <- fFin[,b]
}
