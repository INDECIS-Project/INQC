returnpotpareto<-function(pato,ret,w=1.65){

  #' Threshold percentile for the Pareto outliers
  #' @description This function returns a value of a threshold percentile for the Pareto outliers
  #' @param pato list with results of modelling/fitting the generalized Pareto distribution
  #' @param ret pseudo-return period (in yr)
  #' @param w average sampling frequency (in 1/yr), a parameter to equate to return period to a temporal interval
  #' (recall the approach is not block maxima but peak over threshold. Typical value of w to equate the return 
  #' period to years is 1.65 (See Wilks, 2011. Statistical Analysis for the Atmospheric Sciences)
  #' @return for a given Pareto distribution, returns the value (the quantile) representing a requested return period
  #' @examples
  #' #Extract the ECA&D precipitation data file from the example data folder
  #' path2inptfl<-system.file("extdata", "RR_SOUID132730.txt", package = "INQC")
  #' #Read the data file
  #' y<-readecad(input=path2inptfl,missing= -9999)[,4]
  #' #Fit the Generalized Pareto distribution
  #' pato<-potpareto(y)
  #' #Define the quantile corresponding to the requested return period 25 years (ret=25)
  #' returnpotpareto(pato,25)
  #' #Define the quantile assuming the existence of 2 precipitation peaks/extreme values 
  #' #every year (on average)
  #' returnpotpareto(pato,25,w=2)
  #' @export

  loc<-pato$threshold
  shape<-pato$estimate[2]
  scale<-pato$estimate[1]
  quanty<-1-1/(ret*w) #  (Wilks, 2008. Statistical Analysis for the Atmospheric Sciences; p.108)
  mle<-evd::qgpd(quanty,loc=loc,shape=shape,scale=scale)
  return(as.numeric(mle))
}