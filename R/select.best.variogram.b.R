select.best.variogram <- function(dat, K=0, criteria='bic') {
  if(criteria=='slope') {
    #select non-significant slopes
    
    slope.tbl <- dat$Selection[dat$Selection$Criteria=='Slope',]
    #find the k values that have non-significant slopes
    slope.tbl <- slope.tbl[abs(slope.tbl$Score)>0.05,]
    if(dim(slope.tbl)[1]>1) {
      K.tmp <- max(slope.tbl$K)
      Ks <- unique(dat$Selection$K)
      idx <- which(Ks==K.tmp)
      if(idx < length(Ks)) {
        idx <- idx+1
      }
      K <- Ks[idx]
    } else {
      #if no slopes are non-significant, take the largest P value 
      #(slope closest to 0)
      slope.tbl <- dat$Selection[dat$Selection$Criteria=='Slope',]
      K <- slope.tbl$K[which.max(slope.tbl$Score)]
    }
  } else if(criteria=='nugget') {
    #same logic as SLope
    slope.tbl <- dat$Selection[dat$Selection$Criteria=='Nugget',]
    slope.tbl <- slope.tbl[abs(slope.tbl$Score)>0.05,]
    if(dim(slope.tbl)[1]>1) {
      K.tmp <- max(slope.tbl$K)
      Ks <- unique(dat$Selection$K)
      idx <- which(Ks==K.tmp)
      if(idx < length(Ks)) {
        idx <- idx+1
      }
      K <- Ks[idx]
    } else {
      slope.tbl <- dat$Selection[dat$Selection$Criteria=='Nugget',]
      K <- slope.tbl$K[which.max(slope.tbl$Score)]
    }
  } else {
    #use bic

    bic.tbl <- dat$Selection[dat$Selection$Criteria=='BIC',]
    if(K==0) {
      idx <- which.min(bic.tbl$Score)
      K <- bic.tbl$K[idx]
    }

  }

  VariogramData=dat$VariogramData[dat$VariogramData$K==K,]
  ResidualsData=dat$ResidualsData[dat$ResidualsData$K==K,]
  
  #normalize
  max.Gamma <- max(dat$VariogramData$Gamma)
  VariogramData$Gamma <- VariogramData$Gamma/max.Gamma
  ResidualsData$Gamma <- ResidualsData$Gamma/max.Gamma
  VariogramData$Source <- 'Model'
  ResidualsData$Source <- 'Residuals'
  VariogramData$Index <- idx
  ResidualsData$Index <- idx
  return(rbind(VariogramData,ResidualsData))
}