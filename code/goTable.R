



a <- readRDS('snake/data/go/40_all/sexf/bayesFREEformatted.Rds')
b <- readRDS('snake/data/go/40_all/sexm/bayesFREEformatted.Rds')
c <- readRDS('snake/data/go/40_all/f/blup.Rds')
d <- readRDS('snake/data/go/40_all/m/blup.Rds')

data <- a

miniSort <- function(data, topHits, title){
  sorted <- data[order(-cor),]
  terms <- unlist(sorted[1:topHits, 1, with=FALSE])
  
  subbed <- sub(pattern='.', replacement=':', x=terms, fixed=TRUE) # replace . with : for all terms
  
  final <- data.table(subbed)
  names(final) <- title
  
  return(final)
}


bayesF <- miniSort(a, 5, 'term')
bayesM <- miniSort(b, 5, 'GO-BayesC Male')
blupF  <- miniSort(c, 5, 'GO-TBLUP Female')
blupM  <- miniSort(d, 5, 'GO-TBLUP Male')


allTerms <- unlist(rbind(bayesF, blupF, bayesM, blupM, use.names=F))




write(allTerms, file='code/go/enrichment/top5.txt', sep='\n')

#sed 's/^....//' temp9 > temp10


termNames <- read.table(file='code/go/enrichment/termNames', sep='\n')

allt <- data.table('GO Term'=allTerms, 'Description'=unlist(termNames))


malt <- data.table(rank=1:5, matrix(unlist(allt), ncol=8))

setcolorder(malt, c(1,5,2,6,3,7,4,8))

malt <- cbind(rank=1:5, malt)

names(malt) <- c('Rank', rep(c('Term', 'Description'), 4))


rankMake <- function(data, index){
 
  kable()
  
   
  
}


kable(malt[,c(1,2,3), with=FALSE], align=c('r','c','l'), 'latex')
kable(malt[,c(1,4,5), with=FALSE], align=c('r','c','l'), 'latex')
kable(malt[,c(1,6,7), with=FALSE], align=c('r','c','l'), 'latex')
kable(malt[,c(1,8,9), with=FALSE], align=c('r','c','l'), 'latex')


femaleChart <- cbind(allt[1:5], allt[6:10])

kable(femaleChart, align = rep(c('c', 'l'), 2), 'simple')
kable(femaleChart, align = rep(c('c', 'l'), 2), 'latex')


top <- c('GO:0045819',
         'GO:0033500',
         'GO:0055088',
         'GO:0042675',
         'GO:0042277',
         'GO:0035008',
         'GO:0140042',
         'GO:0007485',
         'GO:0005811',
         'GO:0045819',
         'GO:0055088',
         'GO:0008586',
         'GO:0045819',
         'GO:0043066',
         'GO:0017056',
         'GO:0042593',
         'GO:0035008',
         'GO:0001738',
         'GO:0007485',
         'GO:0016327')

topTerms <- data.table(matrix(top, ncol=4))
names(topTerms) <- c('GO_BayesC F', 'GO-BayesC M', 'GO-TBLUP-F', 'GO-TBLUP M')
topTerms
saveRDS(topTerms, 'data/topTerms.Rds')







termNames <- c('positive regulation of glycogen catabolic process',
               'carbohydrate homeostasis',
               'lipid homeostasis',
               'compound eye cone cell differentiation',
               'peptide binding',
               'positive regulation of melanization defense response',
               'lipid droplet formation',
               'imaginal disc-derived male genitalia development',
               'lipid droplet',
               'positive regulation of glycogen catabolic process',
               'lipid homeostasis',
               'imaginal disc-derived wing vein morphogenesis',
               'positive regulation of glycogen catabolic process',
               'negative regulation of apoptotic process',
               'structural constituent of nuclear pore',
               'glucose homeostasis',
               'positive regulation of melanization defense response',
               'morphogenesis of a polarized epithelium',
               'imaginal disc-derived male genitalia development',
               'apicolateral plasma membrane')


all <- c(top, termNames)

topAll <- data.table(matrix(all, ncol=8))
names(topAll) <- c('GO_BayesC F', 'GO-BayesC M', 'GO-TBLUP-F', 'GO-TBLUP M', rep('GO Term', 4))
setcolorder(topAll, c(1,5,2,6,3,7,4,8))
saveRDS(topAll, 'data/topAll.Rds')
