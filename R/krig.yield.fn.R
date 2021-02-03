krig.yield.fn <- function(harvest,seed,rng=40,locations=~Longitude+Latitude,idw=FALSE,plot=FALSE) {
  harvest.var <- NULL
  sph.vgm <- NULL
  if(!idw) {
    harvest.var <- variogram(Yield~1, locations=locations, 
                             data=harvest)
    sph.vgm <- fit.variogram(harvest.var, vgm("Sph"))
    seeding.krig <- krige(id="Yield", 
                          formula=Yield~1, 
                          locations=locations,
                          data = harvest, 
                          newdata = seed, 
                          maxdist = rng,
                          model=sph.vgm)
    if(plot) {
      plot(gamma ~ dist, data=harvest.var,ylim=c(0,max(harvest.var$gamma)),col="blue")
      abline(v=rng)
    }
    return(list(harvest.var=harvest.var,
                sph.vgm=sph.vgm,
                Yield=seeding.krig$Yield.pred))
  } else {
    gs <- gstat(formula=Yield~1, locations=~Longitude+Latitude,data=harvest)
    #predict(gs, seed)
    #  seeding.krig <- idw(formula=Yield~1, 
    #                  locations=~Longitude+Latitude,
    #                  data = harvest, 
    #                  newdata = seed, 
    #                  maxdist = rng,
    #                  idp=2)
    return(list(harvest.var=harvest.var,
                sph.vgm=sph.vgm,
                Yield=predict(gs, seed,nmax = rng)$var1.pred))
  }
}