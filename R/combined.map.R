combined.map <- function(maps) {
  
  Easting <- maps[[1]]$Easting
  Northing <- maps[[1]]$Northing
  Rank <- rank(maps[[1]]$Yield)
  Quantile=Rank/max(Rank)
  Map=rep(attr(maps[[1]],'Year'),length(Quantile))
  
  for(i in 2:length(maps)) {
    Easting <- c(Easting,maps[[i]]$Easting)
    Northing <- c(Northing,maps[[i]]$Northing)
    Rank <- rank(maps[[i]]$Yield)
    Quantile=c(Quantile,Rank/max(Rank))
    Map=c(Map,rep(attr(maps[[i]],'Year'),length(Rank)))
  }
  
  return(data.frame(Easting=Easting,
                    Northing=Northing,
                    Quantile=Quantile,
                    Map=Map))
}