spatial.selection.plots <- function(models) {

  main <- paste(attr(models,'Field'), attr(models,'Year'), attr(models,'Crop'), attr(models,'method'))
  
  Selection.plot <-  ggplot(models$Selection, aes(K,Score)) + 
    geom_point(aes(colour = Criteria),size=2) + 
    geom_line(aes(colour = Criteria),size=1) + 
    scale_colour_manual(values=cbPalette) + facet_wrap(~Criteria,nrow=3,scales="free_y") +
    labs(title=main, x ="Smoothing Parameter", y = "Score")

  models$VariogramData$Source <- 'Model'
  models$ResidualsData$Source <- 'Residuals'
  side.dat <- rbind(models$VariogramData,models$ResidualsData)
  side.dat$K <- factor(side.dat$K)
  
  Variogram.plot <- ggplot(side.dat, aes(Distance,Gamma)) + 
    geom_point(aes(colour = K),size=2) + 
    geom_smooth(aes(group = K,color=K), se=FALSE,alpha=0.5) +
    scale_colour_manual(values=c(cbPalette,cbPalette)) + 
    labs(title=paste(main,'Variograms'), x ="Distance (m)", y = "Covariance") +
    facet_wrap(~ Source)
  
  #models$VariogramData$K <- factor(models$VariogramData$K)
  #Variogram.plot <- ggplot(models$VariogramData, aes(Distance,Gamma)) + 
  #  geom_point(aes(colour = K),size=2) + 
  #  geom_smooth(aes(group = K,color=K),
  #              se=FALSE) +
  #  scale_colour_manual(values=c(cbPalette,cbPalette)) + 
  #  labs(title=paste(main,'Field Variogram'), x ="Distance (m)", y = "Covariance")
  
  #models$ResidualsData$K <- factor(models$ResidualsData$K)
  
 # Residuals.plot <-  ggplot(models$ResidualsData, aes(Distance,Gamma)) + 
#    geom_point(aes(colour = K),size=2) + 
#    geom_smooth(aes(group = K,color=K),
#                se=FALSE) +
#    scale_colour_manual(values=c(cbPalette,cbPalette)) + 
#    labs(title=paste(main,'Residual Variogram'), x ="Distance (m)", y = "Covariance")
  
  #Residuals.plot <-  ggplot(models$ResidualsData, aes(Distance,Gamma)) + 
  #  geom_point(aes(colour = K),size=2) + 
  #  geom_smooth(aes(group = K,color=K), se=FALSE,alpha=0.3) +
  #  geom_smooth(aes(group = K,color=K),method='lm',
  #              se=FALSE,size=0.5) +
  #  scale_colour_manual(values=c(cbPalette,cbPalette))
  
  return(list(Selection=Selection.plot,
              Variogram=Variogram.plot))
  #           Residuals=Residuals.plot))
  #Slopes=Slopes.dat))
}