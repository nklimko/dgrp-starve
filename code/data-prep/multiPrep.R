library(data.table)
library(dplyr)



# Read in tables
dtf <- fread("data/eQTL_traits_females.csv")
dtm <- fread("data/eQTL_traits_males.csv")

#Change column order to line, starvation, everything else
setcolorder(dtf, c(1,10,2:9,11:19))
#male has aggression moved to end to line up all other column indices
setcolorder(dtm, c(1,11,3:10,12:20,2))

# Bind tables on line
bound <- dtm[dtf, on=.(line)]



x <- (bound[,2,with=FALSE] + bound[,21,with=FALSE]) / 2

par(mfrow=c(6,4))

# Determine average of each trait
for(i in 3:19)
{
  title <- paste("starvation resistance vs",colnames(bound[, i, with=FALSE]))
  print(title)
  
  j <- i + 19
  
  y <- (bound[,i,with=FALSE] + bound[,j,with=FALSE]) / 2
  
  raw <- cbind(x,y)
  
  clean <- setDT(raw)
  
  plot(clean)
  
}

fwrite(bound, "/data/morgante_lab/nklimko"

  
  temp
  
  plot(temp)
  
  class(clean)
  class(clean[,1])
  
  plot(clean)
  clean
  plot(as.vector(clean[ , 1]), as.vector(clean[ , 2]))
  
}




bound
  

ind <- 10
  
  i <- 10
  
  bound[, .(i)]
  
  bound[,ind == get("ind", globalenv())]
  
  ncol(bound)
  
  bound[,2]
  
  for(i in 2:19) {
    print(bound[, i, with=FALSE])
  }
  
  
  for(i in 1:ncol(data1)) {       # for-loop over columns
    data1[ , i] <- data1[ , i] + 10
  }
  
  
  #bound[, title:=(bound[, .I[i]] + bound[, .I[j]])/2]
  
  
  
}

bound



# Determine average of initial trait, starvation  
boundAvg <- bound[,starve:=(starvation+i.starvation)/2]







  
  boundAvg
  
  colnames(dtf)
  colnames(dtm)
  dtm[,3]
  dtf[,2]
  
  dim(dtf)[2]
  
  dtf[,10]
  
  colnames(dtm)
  
  boundAvg[,c(3,22)]
  
  (boundAvg[,3] + boundAvg[,22])/2
  
  boundAvg[,20]
  
  10 in female
  11 in male
  
  dtm[,11]
  
  
`  for(i in 3:dim(dtf)[2])
  {
    
  
    
    print("suh")
    print(i)
    
    
    
}
  
  
  line <- dtf[,1:2]
    f <- dtf[,3]
  m
  
  
cleanScatter <- function(starve, f, m)
  
title <- colnames(f)  
  
base <- setDT(starve,f,m)

baseAvg <- base[,newAvg:=]
  f
m
  
  base <- colnames(f)
  
  colnames(f) <- "f"
  colnames(m) <- "m"

  raw <- setDT(line, f, m)
  clean <- na.omit(raw)


  
  # extract data
  # match to non nulls in both
  # scatter plot
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  