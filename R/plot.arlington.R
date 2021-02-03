wd <- getwd()
setwd("./McCullagh")
source('arlington.R')
arlington.dat <- data.frame(Yield = rep(0,n),
Longitude = rep(0,n),
Latitude = rep(0,n))
idx <- 0
for (i in 1:nrows) {
  for(j in 1:ncols) {
    idx <- idx+1
    arlington.dat$Latitude[idx]=i*(rowwidth+rowsep)
    arlington.dat$Longitude[idx]=j*(colwidth+colsep)
    arlington.dat$Yield[idx] <- y0[idx]
  }
}

arlington.dat$e <- arlington.dat$Longitude + rowwidth
arlington.dat$w <- arlington.dat$Longitude - rowwidth
arlington.dat$n <- arlington.dat$Latitude + colwidth
arlington.dat$s <- arlington.dat$Latitude - colwidth

base.plot <- ggplot(arlington.dat, aes(Longitude, Latitude)) + geom_point(aes(colour = Yield),size = 1) +
scale_colour_gradient(low=cbPalette[7], high=cbPalette[4])

base.plot <- base.plot + geom_segment(aes(x = e, y = s, xend = w, yend = s), 
data = arlington.dat,color = cbPalette[3])
base.plot <- base.plot + geom_segment(aes(x = e, y = n, xend = w, yend = n), 
data = arlington.dat,color = cbPalette[3])
base.plot <- base.plot + geom_segment(aes(x = e, y = n, xend = e, yend = s), 
data = arlington.dat,color = cbPalette[3])
base.plot <- base.plot + geom_segment(aes(x = w, y = n, xend = w, yend = s), 
data = arlington.dat,color = cbPalette[3])

base.plot

setwd(wd)