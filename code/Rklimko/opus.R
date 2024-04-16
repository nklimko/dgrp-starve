ggTidy <- function(data){
  for(i in 1:dim(data)[2]){
    name <- colnames(data)[i]
    temp <- cbind(rep(name, dim(data)[1]), data[,i, with=FALSE])
    if(i==1){
      hold <- temp
    } else{
      hold <- rbind(hold, temp, use.names=FALSE)
    }
  }
  colnames(hold) <- c("method", "cor")
  hold$method <- factor(hold$method, levels=unique(hold$method))
  return(hold)
}
