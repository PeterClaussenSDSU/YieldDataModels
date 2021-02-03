wd <- getwd()
setwd("./McCullagh")
source('antelope.R')
antelope.dat <- data.frame(Yield = rep(0,n),
Longitude = rep(0,n),
Latitude = rep(0,n))
idx <- 0
for (i in 1:nrows) {
  for(j in 1:ncols) {
    idx <- idx+1
    antelope.dat$Latitude[idx]=i*(rowwidth+rowsep)
    antelope.dat$Longitude[idx]=j*(colwidth+colsep)
    antelope.dat$Yield[idx] <- y[idx]
  }
}

antelope.dat$e <- antelope.dat$Longitude + rowwidth
antelope.dat$w <- antelope.dat$Longitude - rowwidth
antelope.dat$n <- antelope.dat$Latitude + colwidth
antelope.dat$s <- antelope.dat$Latitude - colwidth

base.plot <- ggplot(antelope.dat, aes(Longitude, Latitude)) + geom_point(aes(colour = Yield),size = 1) +
scale_colour_gradient(low=cbPalette[7], high=cbPalette[4])

base.plot <- base.plot + geom_segment(aes(x = e, y = s, xend = w, yend = s), 
data = antelope.dat,color = cbPalette[3])
base.plot <- base.plot + geom_segment(aes(x = e, y = n, xend = w, yend = n), 
data = antelope.dat,color = cbPalette[3])
base.plot <- base.plot + geom_segment(aes(x = e, y = n, xend = e, yend = s), 
data = antelope.dat,color = cbPalette[3])
base.plot <- base.plot + geom_segment(aes(x = w, y = n, xend = w, yend = s), 
data = antelope.dat,color = cbPalette[3])

base.plot

setwd(wd)