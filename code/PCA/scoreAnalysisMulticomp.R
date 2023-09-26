
ind <- 25

#############
rawScores2 <-pcs$rotation[,1]


tempName <- sort(rawScores2, decreasing=TRUE)

tempName[1:ind]
###############
rawScores <- abs(pcs$rotation[,1])
tempName <- sort(rawScores, decreasing=TRUE)
ind <- 25
#tempName[1:ind]
#names(tempName[1:ind])
topNames <- names(tempName[1:ind])

pcs$rotation[topNames,1]

