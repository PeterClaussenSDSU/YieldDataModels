metre <- 1.0
inch <- 2.54/100 * metre
foot <- 12 * inch
kilo <- 1.0
pound <- kilo / 2.204622
gram <- kilo/1000

rowwidth <- 0.5 * foot
colwidth <- 1.0 * foot
#PMC according to Fairfield, there were four rows (6 inches apart) 
# on each side and one foot on each end discarded at harvest
#rowsep <- 0 * foot
#colsep <- 0 * foot

rowsep <- 2 * foot
colsep <- 1 * foot
nrows <- 30
ncols <- 36
n <- nrows*ncols

y <- t(matrix(scan("./McCullagh/fairfield1.dat"), ncols, nrows))	# yield per plot
y <- matrix(y, n, 1) * 0.1  # to convert yield to grams

#There a slicker way to do this in R, but I'm going to go a more manual but less error prone route

fairfield.dat <- data.frame(Yield = rep(0,n),
Longitude = rep(0,n),
Latitude = rep(0,n))
idx <- 0
for (i in 1:nrows) {
  for(j in 1:ncols) {
    idx <- idx+1
    fairfield.dat$Latitude[idx]=i*(rowwidth+rowsep)
    fairfield.dat$Longitude[idx]=j*(colwidth+colsep)
    fairfield.dat$Yield[idx] <- y[idx]
  }
}

fairfield.dat$e <- fairfield.dat$Longitude + rowwidth
fairfield.dat$w <- fairfield.dat$Longitude - rowwidth
fairfield.dat$n <- fairfield.dat$Latitude + colwidth
fairfield.dat$s <- fairfield.dat$Latitude - colwidth

base.plot <- ggplot(fairfield.dat, aes(Longitude, Latitude)) + geom_point(aes(colour = Yield),size = 1) +
scale_colour_gradient(low=cbPalette[7], high=cbPalette[4])

base.plot <- base.plot + geom_segment(aes(x = e, y = s, xend = w, yend = s), 
data = fairfield.dat,color = cbPalette[3])
base.plot <- base.plot + geom_segment(aes(x = e, y = n, xend = w, yend = n), 
data = fairfield.dat,color = cbPalette[3])
base.plot <- base.plot + geom_segment(aes(x = e, y = n, xend = e, yend = s), 
data = fairfield.dat,color = cbPalette[3])
base.plot <- base.plot + geom_segment(aes(x = w, y = n, xend = w, yend = s), 
data = fairfield.dat,color = cbPalette[3])

base.plot