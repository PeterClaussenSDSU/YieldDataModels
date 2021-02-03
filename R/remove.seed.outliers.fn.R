remove.seed.outliers.fn <- function(tbl) {
  tbl <- tbl[tbl$AppliedRate<=32000,]
  tbl<- tbl[tbl$AppliedRate>=20000,]
  return(tbl)
}