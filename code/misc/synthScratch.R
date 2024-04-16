
if(0){
  #B=readBinMat('data/bglr/h2_ETA_1_b.bin')
  B=readBinMat(h2_goPath)
  h2_new=rep(NA,nrow(B))
  varU_new=h2_new
  varE_new=h2_new
  for(i in 1:length(h2_new)){
    u=X1%*%B[i,]
    varU_new[i]=var(u)
    varE_new[i]=var(y-u)
    h2_new[i]=varU_new[i]/(varU_new[i]+varE_new[i])
  }
  
  saveRDS(h2_new, outPath)
  
  # X2
  h2_0 <- 0.001
  B2=readBinMat('data/bglr/h2_ETA_2_b.bin')
  h2_new2=rep(NA,nrow(B))
  varU_new2=h2_new2
  varE_new2=h2_new2
  for(i in 1:length(h2_new2)){
    u2=X2%*%B2[i,]
    varU_new2[i]=var(u2)
    varE_new2[i]=var(y-u2)
    h2_new2[i]=varU_new2[i]/(varU_new2[i]+varE_new2[i])
  }
}
