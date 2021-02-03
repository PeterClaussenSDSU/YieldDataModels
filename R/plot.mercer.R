wd <- getwd()
setwd("./McCullagh")
source('mercer.R')

mercer.dat <- data.frame(Yield = rep(0,n),
Longitude = rep(0,n),
Latitude = rep(0,n))
idx <- 0
for (i in 1:nrows) {
  for(j in 1:ncols) {
    idx <- idx+1
    #mercer.dat$Latitude[idx]=i*(rowwidth+rowsep[i])
    #mercer.dat$Longitude[idx]=j*(colwidth+colsep[j])
    mercer.dat$Latitude[idx]=i*(rowwidth)
    mercer.dat$Longitude[idx]=j*(colwidth)
    mercer.dat$Yield[idx] <- y1[idx]
  }
}

mercer.dat$e <- mercer.dat$Longitude + rowwidth
mercer.dat$w <- mercer.dat$Longitude - rowwidth
mercer.dat$n <- mercer.dat$Latitude + colwidth
mercer.dat$s <- mercer.dat$Latitude - colwidth

base.plot <- ggplot(mercer.dat, aes(Longitude, Latitude)) + geom_point(aes(colour = Yield),size = 1) +
scale_colour_gradient(low=cbPalette[7], high=cbPalette[4])

base.plot <- base.plot + geom_segment(aes(x = e, y = s, xend = w, yend = s), 
data = mercer.dat,color = cbPalette[3])
base.plot <- base.plot + geom_segment(aes(x = e, y = n, xend = w, yend = n), 
data = mercer.dat,color = cbPalette[3])
base.plot <- base.plot + geom_segment(aes(x = e, y = n, xend = e, yend = s), 
data = mercer.dat,color = cbPalette[3])
base.plot <- base.plot + geom_segment(aes(x = w, y = n, xend = w, yend = s), 
data = mercer.dat,color = cbPalette[3])

base.plot

setwd(wd)