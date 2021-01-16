returnpotpareto<-function(pato,ret,w=1.65){

  #' Threshold percentile for the Pareto outliers
  #' @description This function returns a value of a threshold percentile for the Pareto outliers
  #' @param pato list with results of modelling/fitting the generalized Pareto distribution
  #' @param ret pseudo-return period
  #' @param w parameter to equate to return period to a temporal interval (recall the approach is not 
  #' block maxima but peak over threshold. Typical value of w to equate the return period to years is 1.65 
  #' (See Wilks, 2011. Statistical Analysis for the Atmospheric Sciences)
  #' @return for a given pareto distribution, returns the value representing a requested return period
  #' @export

  loc<-pato$threshold
  shape<-pato$estimate[2]
  scale<-pato$estimate[1]
  quanty<-1-(1/ret*w) #OS: Probably, correctness of this expression should be checked. 'quanty<-1-(1/ret*w)' and 'quanty<-1-(1/(ret*w))' are different
  mle<-evd::qgpd(quanty,loc=loc,shape=shape,scale=scale)
  return(as.numeric(mle))
}