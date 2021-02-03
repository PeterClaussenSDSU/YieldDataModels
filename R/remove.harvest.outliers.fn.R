remove.harvest.outliers.fn <- function(tbl,rng=3) {
  tbl <- tbl[!is.na(tbl$Yield),]
  yield.mean <- mean(tbl$Yield)
  yield.sd <- sd(tbl$Yield)
  tbl <- tbl[tbl$Yield<=yield.mean+rng*yield.sd,]
  tbl<- tbl[tbl$Yield>=yield.mean+rng-yield.sd,]
  return(tbl)
}