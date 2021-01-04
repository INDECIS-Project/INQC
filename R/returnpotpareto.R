returnpotpareto<-function(pato,ret,w=1.65){

  #' Threshold percentile for the Pareto outliers
  #' @description This function returns a value of a threshold percentile for the Pareto outliyers
  #' @param pato list with results of modelling/fitting the generalized Pareto distribution
  #' @param ret pseudo return period
  #' @param w multiplicator used to calculate a threshold probability
  #' @return percentile of the generalized Pareto distribution corresponding to a threshold probability calculated based on the peudo return period
  #' @export

  loc<-pato$threshold
  shape<-pato$estimate[2]
  scale<-pato$estimate[1]
  quanty<-1-(1/ret*w) #OS: Probably, correctness of this expresion should be checked. 'quanty<-1-(1/ret*w)' and 'quanty<-1-(1/(ret*w))' are different
  mle<-evd::qgpd(quanty,loc=loc,shape=shape,scale=scale)
  return(as.numeric(mle))
}