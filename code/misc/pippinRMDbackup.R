
```{r impassm}




plotTerms <- list.dirs(path=paste0('snake/data/go/25_fit/sex', sex))

xpPath <- paste0('snake/data/01_matched/', sex,'_starvation.Rds')

termReadermalleable <- function(term, sex){
  idPath <- paste0('snake/data/go/03_goterms/sex', sex,'/', term, '.Rds')
  fitPath <- paste0('snake/data/go/25_fit/sex', sex,'/', term, '/bayesFull.Rds')
  
  xp <- readRDS(xpPath)
  genes <- colnames(xp[,-1])
  
  id1 <- readRDS(idPath)
  model1 <- readRDS(fitPath)
  
  fit1 <- model1$fit
  
  inlay <- fit1$ETA[[1]]$d
  outlay <- fit1$ETA[[2]]$d
  
  fitDataM <- data.table(index=c(1:length(inlay)), cor=inlay, gene=genes[id1])
  plothole <- dMake(fitDataM, toupper(sex), paste0(term, " PIP"), 'Index', 'PIP', 1)
  return(plothole)
}



termReader <- function(term, sex)
{
  
  idPath <- paste0('snake/data/go/03_goterms/sex', sex,'/', term, '.Rds')
  fitPath <- paste0('snake/data/go/25_fit/sex', sex,'/', term, '/bayesFull.Rds')
  xpPath <- paste0('snake/data/01_matched/', sex,'_starvation.Rds')
  
  xp <- readRDS(xpPath)
  genes <- colnames(xp[,-1])
  
  id1 <- readRDS(idPath)
  model1 <- readRDS(fitPath)
  
  fit1 <- model1$fit
  
  inlay <- fit1$ETA[[1]]$d
  outlay <- fit1$ETA[[2]]$d
  
  fitDataM <- data.table(index=c(1:length(inlay)), cor=inlay, gene=genes[id1])
  plothole <- dMake(fitDataM, toupper(sex), paste0(term, " PIP"), 'Index', 'PIP', 1)
  return(plothole)
}

f1 <- termReader('GO.0045819', 'f')
f2 <- termReader('GO.0033500', 'f')
f3 <- termReader('GO.0055088', 'f')
f4 <- termReader('GO.0042675', 'f')

m1 <- termReader('GO.0035008', 'm')
m2 <- termReader('GO.0140042', 'm')
m3 <- termReader('GO.0007485', 'm')
m4 <- termReader('GO.0005811', 'm')

```




```{r termReader2, warning=FALSE}

nonReader <- function(term, sex){
  
  idPath <- paste0('snake/data/go/03_goterms/sex', sex,'/', term, '.Rds')
  fitPath <- paste0('snake/data/go/25_fit/sex', sex,'/', term, '/bayesFull.Rds')
  xpPath <- paste0('snake/data/01_matched/', sex,'_starvation.Rds')
  
  xp <- readRDS(xpPath)
  genes <- colnames(xp[,-1])
  
  id1 <- readRDS(idPath)
  model1 <- readRDS(fitPath)
  
  fit1 <- model1$fit
  
  inlay <- fit1$ETA[[1]]$d
  outlay <- fit1$ETA[[2]]$d
  
  fitDataM <- data.table(index=c(1:length(outlay)), cor=outlay, gene=genes[-id1])
  
  
  plothole <- dMake(fitDataM, toupper(sex), paste0(term, " PIP"), 'Index', 'PIP', 0.1)
  return(plothole)
}

nf1 <- nonReader('GO.0045819', 'f')
nf2 <- nonReader('GO.0033500', 'f')
nf3 <- nonReader('GO.0055088', 'f')
nf4 <- nonReader('GO.0042675', 'f')

nm1 <- nonReader('GO.0035008', 'm')
nm2 <- nonReader('GO.0140042', 'm')
nm3 <- nonReader('GO.0007485', 'm')
nm4 <- nonReader('GO.0005811', 'm')

```


### Female

#### Posterior Inclusion Probability of GO portion
```{r f3}
plot_grid(f1, f2, f3, f4, ncol=2)
```

#### Posterior Inclusion Probability of non-GO portion
```{r f4}
plot_grid(nf1, nf2, nf3, nf4, ncol=2)
```
