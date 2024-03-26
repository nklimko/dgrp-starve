temp <- readRDS("snake/data/go/40_all/sexf/sparseData.Rds")

interest <- 'snake/data/go/24_goCor/f/randTrim/GO.0005229'
interest <- 'snake/data/go/24_goCor/f/randTrim/GO.0015267'
fileList <- list.files(path=interest, full.names = TRUE)

go1 <- unlist(sapply(fileList, readRDS))

go2 <- unlist(sapply(fileList, readRDS))

tableComp <- data.table(a=go1, b=go2)




#function
partMake <- function(data, sex, nullInt, upperCutoff, lowerCutoff, psize, custom.title, custom.Xlab, custom.Ylab){
  plothole <- ggplot(data, aes(x=term, y=cor, label=term))+
    geom_point(color=viridis(1, begin=0.5), size=psize)+
    geom_text(aes(label=ifelse(cor>upperCutoff, as.character(term),'')), hjust=0, size=2, angle=0)+
    geom_text(aes(label=ifelse(cor<lowerCutoff, as.character(term),'')), hjust=1, size=2, angle=90)+
    geom_hline(yintercept = nullInt) +
    theme_minimal() +
    labs(x=custom.Xlab, y=custom.Ylab, tag=sex, title=custom.title) +
    theme(text=element_text(size=10), plot.tag = element_text(size=15))
  return(plothole)
}

#data
load('snake/data/go/50_tables/saveTables.Rdata')


#Cutoff selection
sigFactor <- 3

sdf <- sd(unlist(allDataF[,2]))
meanf <- mean(unlist(allDataF[,2]))
cutoffF <- meanf + sigFactor*sdf

sdm <- sd(unlist(allDataM[,2]))
meanm <- mean(unlist(allDataM[,2]))
cutoffM <- meanm + sigFactor*sdm


#graphs
gg[[3]] <- partMake(allDataF, 'F', 0.31, cutoffF, 0.2, 1, 'Effect of GO Annotations in TBLUP models', 'GO Term', 'Prediction Accuracy')

gg[[4]] <- partMake(allDataM, 'M', 0.43, cutoffM, 0.2, 1, 'Effect of GO Annotations in TBLUP models', 'GO Term', 'Prediction Accuracy')

subF <- allDataF[which(cor>cutoffF),]
subM <- allDataM[which(cor>cutoffM),]

subF <- subF[order(-cor),]
subM <- subM[order(-cor),]

#write ordered GO terms to table file for enrichment purposes
cat(unlist(subF[,1]), sep = '\n', file='snake/data/go/50_tables/topHitsF.txt')
cat(unlist(subM[,1]), sep = '\n', file='snake/data/go/50_tables/topHitsM.txt')
cat(unlist(subF[,1]), sep = '\n', file='snake/code/go/enrichment/blup/f/topHitsF.txt')
cat(unlist(subM[,1]), sep = '\n', file='snake/code/go/enrichment/blup/m/topHitsM.txt')

topBlupSoloF <- readRDS('snake/code/go/enrichment/blup/f/finalData.Rds')
topBlupSoloM <- readRDS('snake/code/go/enrichment/blup/m/finalData.Rds')

```

### Initial Findings

Comparison of top 20 terms from both BayesC and TBLUP yields familiar results: 11 of top 20 match for females, 10 of top 20 match for males

This suggests the models are both accurate and able to detect GO terms of interest, even with delimited R2.

#### Females

```{r femaleMerge}

kable(finF, caption = 'Female BayesC/BLUP Comparison', "simple")

```

#### Males

```{r maleMerge}

kable(finM, caption = 'Male BayesC/BLUP Comparison', "simple")

```

### Overall Results

For GO-TBLUP, I filtered top terms that were 3 standard deviations above the mean for each sex.

We then translated the top GO terms into human readable categories to assess our findings. Below are the top ten ordered by correlation.


```{r fblup}

kable(topBlupSoloF, caption = 'GO-TBLUP Genes', "simple")

```



#### Males

```{r mblup}


kable(topBlupSoloM, caption = 'GO-TBLUP Genes', "simple")

```

#### Comparison

```{r comp}

allSolo <- cbind(topBlupSoloF[1:7], topBlupSoloM)

names(allSolo) <- c('Female Gene', 'Count', 'Name', 'Male Gene', 'Count', 'Name')

kable(allSolo, caption = 'GO-TBLUP Gene Comparison', "simple")

```

Looking at both sexes together, the only two genes that are found in both rankings are InR and Pdk1. Coincidentally, both are significantly involved genes for both sexes.

+ [InR](http://flybase.org/reports/FBgn0283499) is an insulin receptor.

+ [Pdk](http://flybase.org/reports/FBgn0017558) is a pyruvate dehydrogenase kinase.

Intuitively, both are heavily involved in carbohydrate modification activity.


gg[[1]] <- partMake(dataF, 'F', 0.31, 1, 0.2, 1, 'Effect of GO Annotations in Bayesian models', 'GO Term', 'Prediction Accuracy')
gg[[2]] <- partMake(dataM, 'M', 0.445, 1, 0.2, 1, 'Effect of GO Annotations in Bayesian models', 'GO Term', 'Prediction Accuracy')

gg[[3]] <- partMake(allDataF, 'F', 0.31, 1, 0.2, 1, 'Effect of GO Annotations in TBLUP models', 'GO Term', 'Prediction Accuracy')
gg[[4]] <- partMake(allDataM, 'M', 0.43, 1, 0.2, 1, 'Effect of GO Annotations in TBLUP models', 'GO Term', 'Prediction Accuracy')


plot_grid(gg[[1]], gg[[2]], gg[[3]], gg[[4]], ncol=2)



ggsave("square.png", plot=plot_grid(gg[[1]], gg[[2]], gg[[3]], gg[[4]], ncol=2), height = 10, width = 15, units = 'in')


temp <- readRDS('snake/data/go/24_goCor/f/randSparse/GO.0005229/10.Rds')
