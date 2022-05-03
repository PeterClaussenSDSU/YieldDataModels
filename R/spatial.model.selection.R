spatial.model.selection <- function(data, fixed="",range=c(5,100), models=8, method='gam',CV=FALSE,plot=FALSE,LISA=FALSE,moments=FALSE) {

  require(gstat)
  if(moments) {
    require(moments)
  }
  if(method %in% c('gam','tp','te','re')) {
    require(mgcv)
  }
  if(method %in% c('ns','bs')) {
    require(splines)
  }
  k.vec = seq(range[1],range[2],length.out=models)
  if(range[1]>=1) {
    k.vec <- round(k.vec)
  }
  Models <- vector(mode='list',length=models)
  Predicted <- NULL
  
  Variograms <- vector(models+1,mode='list')
  Residuals <- vector(models+1,mode='list')
  
  tmp.var <- variogram(Yield~1, 
                       locations=~Longitude+Latitude,
                       data=data)
  
  VariogramData <-  data.frame(Distance=tmp.var$dist,
                               Gamma=tmp.var$gamma,
                               K=0)
  ResidualsData <-  VariogramData
  
  if(LISA) {
    dists <- as.matrix(dist(cbind(data$Lon, data$Lat)))
    
    dists.inv <- 1/dists
    diag(dists.inv) <- 0
    VariogramI <- 1:models
    ResidualI <- 1:models
  }
  
  data.columns <- c('Longitude','Latitude','Yield')
  if(fixed=="") {
    fixed <- "1"
  } else {
    data.columns <- c(data.columns,fixed)
  }
  fmla <- paste('Yield ~',fixed)

  summary.fn <- logLik
  if(method %in% c('gam','tp','te','re')) {
    null.model <- gam(as.formula(fmla),data=data[,data.columns])
  } else {
    #null.model <- lm(Yield ~ 1, data=data[,c('Longitude','Latitude','Yield')])
    null.model <- lm(as.formula(fmla), data=data[,data.columns])
  }
  
  adjR2.fn <- function(x){summary(x)$adj.r.squared}
  
  for(i in 1:length(k.vec)) {
    #print(paste("K= ",k.vec[i]))
    tmp.dat <- data[,data.columns]
    if(method=='idw') {
      krig <- krig.yield.fn(tmp.dat,tmp.dat,rng=k.vec[i],idw=TRUE)
      tmp.dat$K = k.vec[i]
      tmp.dat$residuals <- tmp.dat$Yield - krig$Yield
      tmp.dat$Yield <- krig$Yield
    } else if(method=='loess') {
      fmla <- paste('Yield ~ Longitude*Latitude +',fixed)
      tmp.loess <- loess(as.formula(fmla), span=k.vec[i] ,data=tmp.dat)
      #tmp.loess <- loess(Yield ~  Longitude*Latitude, span=k.vec[i] ,data=tmp.dat)
      Yield <- predict(tmp.loess,newdata=tmp.dat)
      tmp.dat$K = k.vec[i]
      tmp.dat$residuals <- tmp.dat$Yield - Yield
      tmp.dat$Yield <- Yield
    } else {
      if(method=='gam') {
        fmla <- paste('Yield ~',fixed,'+ s(Longitude,Latitude,k=',k.vec[i],')')
        Models[[i]] <- gam(as.formula(fmla),data=tmp.dat)
        #Models[[i]] <- gam(Yield ~ s(Longitude,Latitude,k=k.vec[i]),data=tmp.dat)
        adjR2.fn <- function(x){summary(x)$r.sq}
      } else if(method=='tp'){
        Models[[i]] <- gam(Yield ~ te(Longitude,Latitude,bs=c("tp", "tp"),k=k.vec[i]),data=tmp.dat)
        adjR2.fn <- function(x){summary(x)$r.sq}
      } else if(method=='te'){
        Models[[i]] <- gam(Yield ~ te(Longitude,Latitude,k=k.vec[i]),data=tmp.dat)
        adjR2.fn <- function(x){summary(x)$r.sq}
      } else if(method=='re'){
        Models[[i]] <- gam(Yield ~ s(Longitude,Latitude,bs =c('re', 're'),k=k.vec[i]),data=tmp.dat)
        adjR2.fn <- function(x){summary(x)$r.sq}
      } else if(method=='ns'){
        fmla <- paste('Yield ~ ',fixed,' + ns(Longitude,df=',k.vec[i],')*ns(Latitude,df=',k.vec[i],')')
       # print(fmla)
        Models[[i]] <- lm(as.formula(fmla), data=tmp.dat)
        #Models[[i]] <- lm(Yield ~ ns(Longitude,df=k.vec[i])*ns(Latitude,df=k.vec[i]), data=tmp.dat)
      } else if(method=='bs'){
        fmla <- paste('Yield ~ ',fixed,'+ bs(Longitude,df=',k.vec[i],')*bs(Latitude,df=',k.vec[i],')')
        Models[[i]] <- lm(as.formula(fmla), data=tmp.dat)
      } else if(method=='lm'){
        Models[[i]] <- lm(Yield ~ poly(Longitude,k.vec[i])*poly(Latitude,k.vec[i]), data=tmp.dat)
      }
      
      if(plot) {plot(Models[[i]])}
      
      tmp.dat$Yield <- predict(Models[[i]])
      tmp.dat$K = k.vec[i]
      tmp.dat$residuals <- residuals(Models[[i]])
    }
    
    Predicted <- rbind(Predicted, tmp.dat)
    
    tmp.var <- variogram(Yield~1, 
                           locations=~Longitude+Latitude,
                           #alpha=c(0,45,90),
                           data=tmp.dat)
    Variograms[[i+1]] <- tmp.var
    #extract variogram values
    VariogramData <- rbind(VariogramData,
                             data.frame(Distance=tmp.var$dist,
                                        Gamma=tmp.var$gamma,
                                        K=k.vec[i]))
      

    tmp.var <- variogram(residuals~1, 
                           locations=~Longitude+Latitude,
                           #alpha=c(0,45,90),
                           data=tmp.dat)
    ResidualsData <- rbind(ResidualsData,
                             data.frame(Distance=tmp.var$dist,
                                        Gamma=tmp.var$gamma,
                                        K=k.vec[i]))
    Residuals[[i+1]] <- tmp.var
    if(LISA) {
      VariogramI[i] <- Moran.I(tmp.dat$Yield, dists.inv)$observed
      ResidualI[i] <- Moran.I(tmp.dat$residuals, dists.inv)$observed
    }
  }
  
  Slopes.dat <- data.frame(K=unique(ResidualsData$K))
  
  tmp.lm <- lm(Gamma ~ Distance,data=ResidualsData[ResidualsData$K==0,])
  #print(summary(tmp.lm))
  
  Slopes.dat$Intercept <- coef(tmp.lm)[1]
  Slopes.dat$Slope <- coef(tmp.lm)[2]
  sum <- summary(tmp.lm)
  Slopes.dat$T <- sum$coefficients['Distance','t value']
  Slopes.dat$P <- sum$coefficients['Distance','Pr(>|t|)']
  
  #tmp.lm <- lm(Gamma ~ poly(Distance,3),data=VariogramData[VariogramData$K==0,])\
  tmp.lm <- lm(Gamma ~ bs(Distance),data=VariogramData[VariogramData$K==0,])
  Slopes.dat$Nugget <- coef(tmp.lm)[1]


  #Slopes.dat$Intercept <- 0
  #Slopes.dat$Slope <- 0
  #Slopes.dat$T <- 0
  #Slopes.dat$P <- 0
  
  for (k in 1:length(Slopes.dat$K)) {
    K <- Slopes.dat$K[k]
    tmp.lm <- lm(Gamma ~ Distance,data=ResidualsData[ResidualsData$K==K,])
    Slopes.dat$Intercept[k] <- coef(tmp.lm)[1]
    Slopes.dat$Slope[k] <- coef(tmp.lm)[2]
    sum <- summary(tmp.lm)
    Slopes.dat$T[k] <- sum$coefficients['Distance','t value']
    Slopes.dat$P[k] <- sum$coefficients['Distance','Pr(>|t|)']
    #tmp.lm <- lm(Gamma ~ poly(Distance,3),data=VariogramData[VariogramData$K==K,])
    tmp.lm <- lm(Gamma ~ bs(Distance),data=VariogramData[VariogramData$K==K,])
    Slopes.dat$Nugget[k] <- coef(tmp.lm)[1]
    sum <- summary(tmp.lm)
    Slopes.dat$NuggetT[k] <- sum$coefficients[1,'t value']
    Slopes.dat$NuggetP[k] <- sum$coefficients[1,'Pr(>|t|)']
    
  }
  
  #print(length(Slopes.dat$Intercept))
  #print(models)
  #print(length(unlist(lapply(Models,AIC))))
  
  if(LISA) {
    Selection <- data.frame(K = rep(k.vec,4),
                            Criteria = as.factor(c(rep('VariogramI',models), #'logLik',models),
                                                   rep('ResidualI',models),
                                                   rep('AIC',models),
                                                   rep('BIC',models))),
                            Score = c(VariogramI,
                                      ResidualI,
                                      unlist(lapply(Models,AIC)),
                                      unlist(lapply(Models,BIC))
                            ))
  } else if(method=='idw' || method=='loess') {
    Selection <- data.frame(K = rep(c(0,k.vec),2),
                            Criteria = as.factor(c(rep('Intercept',models+1),
                                        rep('Slope',models+1))),
                            Score = c(
                                      Slopes.dat$Intercept,
                                      Slopes.dat$Slope)
    )
  } else {
    if(!moments) {
      Selection <- data.frame(K = rep(c(0,k.vec),4),
                              Criteria = as.factor(c(rep('AIC',models+1),
                                                     rep('BIC',models+1),
                                                     #rep('AdjR2',models+1),
                                                     rep('Nugget',models+1),
                                                     rep('Slope',models+1))),
                              Score = c(c(AIC(null.model),unlist(lapply(Models,AIC))),
                                        c(BIC(null.model),unlist(lapply(Models,BIC))),
                                        #c(adjR2.fn(null.model),unlist(lapply(Models,adjR2.fn))),
                                        Slopes.dat$NuggetP,
                                        #Slopes.dat$Intercept,
                                        Slopes.dat$P)
      )
    } else {
      

    Selection <- data.frame(K = rep(c(0,k.vec),8),
                            Criteria = as.factor(c(rep('AIC',models+1),
                                                   rep('BIC',models+1),
                                                   rep('SkewnessM',models+1),
                                                   rep('SkewnessR',models+1),
                                                   rep('KurtosisM',models+1),
                                                   rep('KurtosisR',models+1),
                                                   #rep('AdjR2',models+1),
                                                   rep('Nugget',models+1),
                                                   rep('Slope',models+1))),
                            Score = c(c(AIC(null.model),unlist(lapply(Models,AIC))),
                                      c(BIC(null.model),unlist(lapply(Models,BIC))),
                                      c(skewness(predict(null.model)),
                                        unlist(lapply(Models,
                                                      function(x) skewness(predict(x))))),
                                      c(skewness(residuals(null.model)),
                                        unlist(lapply(Models,
                                                      function(x) skewness(residuals(x))))),
                                      c(kurtosis(predict(null.model)),
                                        unlist(lapply(Models,
                                                      function(x) kurtosis(predict(x))))),
                                      c(kurtosis(residuals(null.model)),
                                        unlist(lapply(Models,
                                                      function(x) kurtosis(residuals(x))))),
                                      #c(adjR2.fn(null.model),unlist(lapply(Models,adjR2.fn))),
                                      Slopes.dat$NuggetP,
                                      #Slopes.dat$Intercept,
                                      Slopes.dat$P)
                            )
    }
  }
  
  

 # Selection$Criteria <- c(Selection$Criteria,rep('Intercept',length(Slopes.dat$Intercept)))
#  Selection$Score <- c(Selection$Score,Slopes.dat$Intercept)
#  Selection$Criteria <- c(Selection$Criteria,rep('Slope',length(Slopes.dat$Slope)))
#  Selection$Score <- c(Selection$Score,Slopes.dat$Slope)
  
  ret = list(Models=Models,
             Variograms=Variograms,
              Residuals=Residuals,
              VariogramData=VariogramData,
              ResidualsData=ResidualsData,
              Selection=Selection,
              Predicted=Predicted,
             VariogramFit=Slopes.dat,
             TMP=tmp.lm)
  
  attr(ret,'method') <- method
  attr(ret,'Year') <- attr(data,'Year')
  attr(ret,'Field') <- attr(data,'Field')
  attr(ret,'Crop') <- attr(data,'Crop')
  
  return(ret)
}