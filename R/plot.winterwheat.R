wd <- getwd()
setwd("./McCullagh")
source('winterwheat.R')
winterwheat.dat <- data.frame(Yield = rep(0,n),
Longitude = rep(0,n),
Latitude = rep(0,n))
idx <- 0
for (i in 1:nrows) {
  for(j in 1:ncols) {
    idx <- idx+1
    winterwheat.dat$Latitude[idx]=i*(rowwidth+rowsep)
    winterwheat.dat$Longitude[idx]=j*(colwidth+colsep)
    winterwheat.dat$Yield[idx] <- y[idx]
  }
}

winterwheat.dat$e <- winterwheat.dat$Longitude + rowwidth
winterwheat.dat$w <- winterwheat.dat$Longitude - rowwidth
winterwheat.dat$n <- winterwheat.dat$Latitude + colwidth
winterwheat.dat$s <- winterwheat.dat$Latitude - colwidth

base.plot <- ggplot(winterwheat.dat, aes(Longitude, Latitude)) + geom_point(aes(colour = Yield),size = 1) +
scale_colour_gradient(low=cbPalette[7], high=cbPalette[4])

base.plot <- base.plot + geom_segment(aes(x = e, y = s, xend = w, yend = s), 
data = winterwheat.dat,color = cbPalette[3])
base.plot <- base.plot + geom_segment(aes(x = e, y = n, xend = w, yend = n), 
data = winterwheat.dat,color = cbPalette[3])
base.plot <- base.plot + geom_segment(aes(x = e, y = n, xend = e, yend = s), 
data = winterwheat.dat,color = cbPalette[3])
base.plot <- base.plot + geom_segment(aes(x = w, y = n, xend = w, yend = s), 
data = winterwheat.dat,color = cbPalette[3])

base.plot

setwd(wd)