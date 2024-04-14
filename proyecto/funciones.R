distEu <- function(X,Y){
  dEu <- (X-Y) * (X -Y)
  dEu <- sqrt(sum(dEu))
  return(dEu)
}
normalizeDataL <- function(dataF,meanF,stdF){
  dataFN <- dataF
  dataFN <- (dataFN - meanF) / stdF
  return(dataFN)
}

isVExtremo <- function(X,meanX,sdX){
  return( X > meanX + 3*sdX | X < meanX -3*sdX)
}
isVExtremoQuartil <- function(X){
  Q1 <- quantile(X,.25)
  Q3 <- quantile(X,.75)
  iqrX <- IQR(X)
  return(x < (Q1 - 1.5*iqrX) | X > (Q3 + 1.5*iqrX))
}
