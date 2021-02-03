wd <- getwd()
setwd("./McCullagh")
source('wiebe.R')

wiebe.dat <- data.frame(Yield = rep(0,n),
Longitude = rep(0,n),
Latitude = rep(0,n))
idx <- 0
for (i in 1:nrows) {
  for(j in 1:ncols) {
    idx <- idx+1
    #wiebe.dat$Latitude[idx]=i*(rowwidth+rowsep[i])
    #wiebe.dat$Longitude[idx]=j*(colwidth+colsep[j])
    wiebe.dat$Latitude[idx]=i*(rowwidth)
    wiebe.dat$Longitude[idx]=j*(colwidth)
    wiebe.dat$Yield[idx] <- y1[idx]
  }
}

wiebe.dat$e <- wiebe.dat$Longitude + rowwidth
wiebe.dat$w <- wiebe.dat$Longitude - rowwidth
wiebe.dat$n <- wiebe.dat$Latitude + colwidth
wiebe.dat$s <- wiebe.dat$Latitude - colwidth

base.plot <- ggplot(wiebe.dat, aes(Longitude, Latitude)) + geom_point(aes(colour = Yield),size = 1) +
scale_colour_gradient(low=cbPalette[7], high=cbPalette[4])

base.plot <- base.plot + geom_segment(aes(x = e, y = s, xend = w, yend = s), 
data = wiebe.dat,color = cbPalette[3])
base.plot <- base.plot + geom_segment(aes(x = e, y = n, xend = w, yend = n), 
data = wiebe.dat,color = cbPalette[3])
base.plot <- base.plot + geom_segment(aes(x = e, y = n, xend = e, yend = s), 
data = wiebe.dat,color = cbPalette[3])
base.plot <- base.plot + geom_segment(aes(x = w, y = n, xend = w, yend = s), 
data = wiebe.dat,color = cbPalette[3])

base.plot

setwd(wd)