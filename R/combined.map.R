combined.map <- function(maps) {
  
  Longitude <- maps[[1]]$Longitude
  Latitude <- maps[[1]]$Latitude
  Rank <- rank(maps[[1]]$Yield)
  Quantile=Rank/max(Rank)
  Map=rep(attr(maps[[1]],'Year'),length(Quantile))
  
  for(i in 2:length(maps)) {
    Longitude <- c(Longitude,maps[[i]]$Longitude)
    Latitude <- c(Latitude,maps[[i]]$Latitude)
    Rank <- rank(maps[[i]]$Yield)
    Quantile=c(Quantile,Rank/max(Rank))
    Map=c(Map,rep(attr(maps[[i]],'Year'),length(Rank)))
  }
  
  return(data.frame(Longitude=Longitude,
                    Latitude=Latitude,
                    Quantile=Quantile,
                    Map=Map))
}