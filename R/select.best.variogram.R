select.best.variogram <- function(dat, K=0, criteria='bic') {
  #use bic
  max.Gamma <- max(dat$VariogramData$Gamma)
  bic.tbl <- dat$Selection[dat$Selection$Criteria=='BIC',]
  if(K==0) {
    idx <- which.min(bic.tbl$Score)
    K <- bic.tbl$K[idx]
  }
  
  VariogramData=dat$VariogramData[dat$VariogramData$K==K,]
  ResidualsData=dat$ResidualsData[dat$ResidualsData$K==K,]
  
  VariogramData$Gamma <- VariogramData$Gamma/max.Gamma
  ResidualsData$Gamma <- ResidualsData$Gamma/max.Gamma
  VariogramData$Source <- 'Model'
  ResidualsData$Source <- 'Residuals'
  return(rbind(VariogramData,ResidualsData))
}