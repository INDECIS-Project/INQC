returnpotpareto<-function(pato,ret,w=1.65){

  #' It should be described later
  #' @description This function should be described later
  #' @param pato it should be described later
  #' @param ret it should be described later
  #' @param w it should be described later
  #' @return list of values which did not pass the test
  #' @export

  loc<-pato$threshold
  shape<-pato$estimate[2]
  scale<-pato$estimate[1]
  quanty<-1-(1/ret*w)
  mle<-evd::qgpd(quanty,loc=loc,shape=shape,scale=scale)
  return(as.numeric(mle))
}