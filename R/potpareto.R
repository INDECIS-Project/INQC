potpareto<-function(y,thres=0.99){

  #' Peaks over threshold modelling
  #' @description This function fits the Generalized Pareto distribution for exeedances over a threshold
  #' @param y vector of values (a series) to be analyzed
  #' @param thres threshold value of probability to define a corresponding threshold percentile
  #' @return list containing results of modelling/fitting the generalized Pareto distribution
  #' @export

  target<-which(!is.na(y) & y!=0)
  xx<-y[target]
  if(length(target) == 0){return(NULL)} #### this is activated if all the values are 0!!!
  threshold<-stats::quantile(xx,thres,na.rm=TRUE)
  if( threshold >= max(xx)){return(NULL)}
  #### Now, it is necessary to make this resistant to desertic places with small number of rainy days
  ## myfit<-fpot(xx,threshold). Changed to the line below, as with some matrices the std.err=TRUE gives problems
  myfit<-evd::fpot(xx,threshold,std.err=FALSE)
  #  name<-'mypareto.jpg'
  #  name<-paste('potpareto',substring(id,1,10),'%03d.jpg',sep='')
  #  jpeg(name)
  #  plot(myfit)
  #  dev.off()
  return(myfit)
}