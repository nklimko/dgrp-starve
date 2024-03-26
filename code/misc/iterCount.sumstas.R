


step1 <- readRDS('data/sr/24_goCor/f/bayesC_1_1.Rds')

step2 <- readRDS("data/sr/30_summary/parop/go/f/bayesC_1.Rds")

step3 <- readRDS("data/sr/33_metric/go/f/cor/bayesGO_1.Rds")

allData <- unlist(step3)

dat15 <- data[1:15]
dat25 <- data[1:25]
dat40 <- data[1:40]
dat50 <- data[1:50]

sumstats <- c(mean=mean(dat15), var=var(dat15))
numlist <- seq(5,50,5)

hold <- c(0,0,0)

sumstats <- function(i, data){
  term_IDs <- sample(1:50, i, replace=FALSE)
  hold <- c(nterm=as.integer(i), mean=mean(data[term_IDs]), var=var(data[term_IDs]))
  return(hold)
}

hold <- lapply(numlist, sumstats, data=allData)


for(i in numlist){
  
                
                
}
