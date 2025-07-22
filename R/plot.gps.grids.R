plot.gps.grids <- function(gps.dat,plots=0,swath.width=0, ratio=1,residuals=TRUE,valueField = "Yield") {
  
  grey <- "#999999"
  orange <- "#E69F00"
  skyblue <- "#56B4E9"
  bluishgreen <- "#009E73"
  yellow <- "#F0E442"
  blue <- "#0072B2"
  vermillion <- "#D55E00"
  
  if(plots>0) {
    for(i in 1:1000) {
      mask <- gps.dat$Easting<(i*ratio) & gps.dat$Northing<i
      if(sum(mask)>plots) {
        break
      }
    }
    gps.dat <- gps.dat[mask,]
  }
  
  gps.dat$DISTANCE <- gps.dat$DISTANCE*0.3048
  if(swath.width==0) {
    #use the monitor's swath width value
    gps.dat$SWATHWIDTH <- gps.dat$SWATHWIDTH*(gps.dat$Swaths+2)*0.3048
  } else {
    gps.dat$SWATHWIDTH <- swath.width
  }

  
  #gps.dat$front <- gps.dat$Easting
  #gps.dat$rear <- gps.dat$Easting - gps.dat$DISTANCE + 0.1
  #gps.dat$right <- gps.dat$Northing + gps.dat$SWATHWIDTH/2
  #gps.dat$left <- gps.dat$Northing - gps.dat$SWATHWIDTH/2
  
  gps.dat$Value  = gps.dat[,valueField] 
  if(residuals) {
    gps.dat$Value <- gps.dat$Value - mean(gps.dat$Value)
  }
  gps.dat$DistanceAngle <- gps.dat$Heading*pi/180
  gps.dat$DistanceRise <- cos(gps.dat$DistanceAngle)*gps.dat$DISTANCE
  gps.dat$DistanceRun <- sin(gps.dat$DistanceAngle)*gps.dat$DISTANCE
  
  gps.dat$SwathAngle <- (gps.dat$Heading-90)*pi/180
  gps.dat$SwathRise <- cos(gps.dat$SwathAngle)*gps.dat$SWATHWIDTH/2
  gps.dat$SwathRun <- sin(gps.dat$SwathAngle)*gps.dat$SWATHWIDTH/2
  
  if(residuals) {
    base.plot <- ggplot(gps.dat, aes(Easting, Northing)) + geom_point(aes(colour = Value),size = 2) +
      scale_colour_gradient2(low=vermillion, mid=yellow, high=blue)
  } else {
    base.plot <- ggplot(gps.dat, aes(Easting, Northing)) + geom_point(aes(colour = Value),size = 2) +
      scale_colour_gradient(low=yellow, high=bluishgreen)
  }

  
  #front line (left to right)
  base.plot <- base.plot + geom_segment(aes(x = Easting - SwathRun, 
                                            xend = Easting + SwathRun,
                                            y = Northing - SwathRise,
                                            yend = Northing + SwathRise), 
                                        data = gps.dat,color = grey,size = .5)
  #rear line (left to right)
  base.plot <- base.plot + geom_segment(aes(x = Easting - SwathRun - DistanceRun, 
                                            xend = Easting + SwathRun - DistanceRun,
                                            y = Northing - DistanceRise - SwathRise,
                                            yend = Northing - DistanceRise + SwathRise), 
                                        data = gps.dat,color = skyblue,size = .3)
  #right line (front to back)
  base.plot <- base.plot + geom_segment(aes(x = Easting + SwathRun, 
                                            xend = Easting + SwathRun - DistanceRun,
                                            y = Northing + SwathRise,
                                            yend = Northing + SwathRise - DistanceRise), 
                                        data = gps.dat,color = skyblue,size = .3)
  #left line (front to back)
  base.plot <- base.plot + geom_segment(aes(x = Easting - SwathRun, 
                                            xend = Easting - SwathRun - DistanceRun,
                                            y = Northing - SwathRise,
                                            yend = Northing - SwathRise - DistanceRise), 
                                        data = gps.dat,color = skyblue,size = .3)
  return(base.plot)
}