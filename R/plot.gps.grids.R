plot.gps.grids <- function(harvest.dat,plots,ratio=1,residuals=TRUE) {
  
  grey <- "#999999"
  orange <- "#E69F00"
  skyblue <- "#56B4E9"
  bluishgreen <- "#009E73"
  yellow <- "#F0E442"
  blue <- "#0072B2"
  vermillion <- "#D55E00"
  
  if(plots>0) {
    for(i in 1:1000) {
      mask <- harvest.dat$Longitude<(i*ratio) & harvest.dat$Latitude<i
      if(sum(mask)>plots) {
        break
      }
    }
    harvest.dat <- harvest.dat[mask,]
  }
  
  harvest.dat$DISTANCE <- harvest.dat$DISTANCE*0.3048
  harvest.dat$SWATHWIDTH <- harvest.dat$SWATHWIDTH*(harvest.dat$Swaths+2)*0.3048
  
  #harvest.dat$front <- harvest.dat$Latitude
  #harvest.dat$rear <- harvest.dat$Latitude - harvest.dat$DISTANCE + 0.1
  #harvest.dat$right <- harvest.dat$Longitude + harvest.dat$SWATHWIDTH/2
  #harvest.dat$left <- harvest.dat$Longitude - harvest.dat$SWATHWIDTH/2
  
  if(residuals) {
    harvest.dat$Yield <- harvest.dat$Yield - mean(harvest.dat$Yield)
  }
  harvest.dat$DistanceAngle <- harvest.dat$Heading*pi/180
  harvest.dat$DistanceRise <- cos(harvest.dat$DistanceAngle)*harvest.dat$DISTANCE
  harvest.dat$DistanceRun <- sin(harvest.dat$DistanceAngle)*harvest.dat$DISTANCE
  
  harvest.dat$SwathAngle <- (harvest.dat$Heading-90)*pi/180
  harvest.dat$SwathRise <- cos(harvest.dat$SwathAngle)*harvest.dat$SWATHWIDTH/2
  harvest.dat$SwathRun <- sin(harvest.dat$SwathAngle)*harvest.dat$SWATHWIDTH/2
  
  if(residuals) {
    base.plot <- ggplot(harvest.dat, aes(Longitude, Latitude)) + geom_point(aes(colour = Yield),size = 2) +
      scale_colour_gradient2(low=vermillion, mid=yellow, high=blue)
  } else {
    base.plot <- ggplot(harvest.dat, aes(Longitude, Latitude)) + geom_point(aes(colour = Yield),size = 2) +
      scale_colour_gradient(low=yellow, high=bluishgreen)
  }

  
  #front line (left to right)
  base.plot <- base.plot + geom_segment(aes(x = Longitude - SwathRun, 
                                            xend = Longitude + SwathRun,
                                            y = Latitude - SwathRise,
                                            yend = Latitude + SwathRise), 
                                        data = harvest.dat,color = grey,size = .5)
  #rear line (left to right)
  base.plot <- base.plot + geom_segment(aes(x = Longitude - SwathRun - DistanceRun, 
                                            xend = Longitude + SwathRun - DistanceRun,
                                            y = Latitude - DistanceRise - SwathRise,
                                            yend = Latitude - DistanceRise + SwathRise), 
                                        data = harvest.dat,color = skyblue,size = .3)
  #right line (front to back)
  base.plot <- base.plot + geom_segment(aes(x = Longitude + SwathRun, 
                                            xend = Longitude + SwathRun - DistanceRun,
                                            y = Latitude + SwathRise,
                                            yend = Latitude + SwathRise - DistanceRise), 
                                        data = harvest.dat,color = skyblue,size = .3)
  #left line (front to back)
  base.plot <- base.plot + geom_segment(aes(x = Longitude - SwathRun, 
                                            xend = Longitude - SwathRun - DistanceRun,
                                            y = Latitude - SwathRise,
                                            yend = Latitude - SwathRise - DistanceRise), 
                                        data = harvest.dat,color = skyblue,size = .3)
  return(base.plot)
}