potpareto<-function(y,thres=0.99){

  #' Peaks over threshold modelling
  #' @description This function fits the Generalized Pareto distribution for exeedances over a threshold
  #' @param y vector of values (a series) to be analyzed
  #' @param thres threshold value of probability to define a corresponding threshold percentile
  #' @return list containing results of modelling/fitting the generalized Pareto distribution
  #' @examples
  #' #Extract the ECA&D precipitation data file from the example data folder
  #' path2inptfl<-system.file("extdata", "RR_SOUID132730.txt", package = "INQC")
  #' #Read the data file
  #' y<-readecad(input=path2inptfl,missing= -9999)[,4]
  #' #Fit the Generalized Pareto distribution
  #' pato<-potpareto(y)
  #' #The parameters of the fitted distribution:
  #' location<-pato$threshold
  #' shape<-pato$estimate[2]
  #' scale<-pato$estimate[1]
  #' print(c(location,shape,scale))
  #' @export

  target<-which(!is.na(y) & y!=0)
  xx<-y[target]
  if(length(target) == 0){return(NULL)} #### this is activated if all the values are 0!!!
  threshold<-stats::quantile(xx,thres,na.rm=TRUE)
  if( threshold >= max(xx)){return(NULL)}
  #### Now, it is necessary to make this resistant to desertic places with small number of rainy days
  ## myfit<-fpot(xx,threshold). Changed to the line below, as with some matrices the std.err=TRUE gives problems
  myfit<-evd::fpot(xx,threshold,std.err=FALSE)
  return(myfit)
}