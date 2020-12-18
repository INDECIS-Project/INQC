potpareto<-function(y,thres=0.99){

  #' It should be described later
  #' @description This function should be described later
  #' @param y it should be described later
  #' @param thres it should be described later
  #' @return list of values which did not pass the test
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