add.metric <- function(data, origin=c(-1,-1)) {
  if(any(origin<0)) {
    origin[1] <- min(data$X)
    origin[2] <- min(data$Y)
  }
  data$Longitude <- data$X - origin[1]
  data$Latitude <- data$Y - origin[2]
  latMid <- (min(data$Y) + max(data$Y))/2
  m_per_deg_lat = 111132.954 - 559.822 * cos( 2.0 * latMid ) + 1.175 * cos( 4.0 * latMid)
  m_per_deg_lon = (3.14159265359/180 ) * 6367449 * cos ( latMid )
  data$Longitude <- data$Longitude*m_per_deg_lon
  data$Latitude <- data$Latitude*m_per_deg_lat
  return(data)
}