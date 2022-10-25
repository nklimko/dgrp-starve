#removed code jic

for(i in 1:mdim){
  if(!is.na(mstarve[i,2])){
    mblank[index,1] <- mstarve[i,1]
    mblank[index,2] <- mstarve[i,2]
    index <- index + 1
  } else{
    nullCount <- nullCount + 1
    print("null detect")
    print(nullCount)
  }
}


nameR <- c(3,4)

nameR <- c(nameR, 5)

nameR
chemX <- c(chemX, 2)




indMark <- 0

for(i in 1:mdim){
  if(!is.na(mstarve[i,2])){
  indMark <- c(indMark, i)
  } else{
    nullCount <- nullCount + 1
    print("null detect")
    print(nullCount)
  }
}

indFin <- indMark