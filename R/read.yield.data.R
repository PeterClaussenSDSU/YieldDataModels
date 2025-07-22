read.yield.data <- function(root,field,year,crop,trim=TRUE,outlier=3) {
  name <- paste(field,year,crop,sep=' ')
  path <- paste(root,name,' Harvest.csv',sep='')
  ret <- read.csv(path)
  if(trim) {
    ret <- remove.harvest.outliers.fn(ret,outlier)
  }
  
  attr(ret,'Year') <- year
  attr(ret,'Crop') <- crop
  attr(ret, 'Field') <- field
  return(ret)
}