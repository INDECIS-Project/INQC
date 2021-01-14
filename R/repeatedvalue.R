repeatedvalue<-function(x,margin=20,friki=150){

  ### WARNING: eval this function, as probably does the same than newfriki, but in a less efficient and more time consuming way
  ### also, parameterization is less objective

  #' Finds repeated values
  #' @description This function looks for a value which repeats too many times and, given the decaying shape of precipitation of empirical distribution,
  #' is considered too large to happen that many times
  #' @param x precipitation time series
  #' @param margin difference in frequency of the nearest value
  #' @param friki minimum value to be considered
  #' @return list of positions which do not pass this QC test. If all positions pass the test, returns NULL
  #' @export

  target<-NULL
  nyu<-table(x) # table of values
  prospect<-which(diff(nyu) > margin) ### we look at which values present high differences in frequency. We do not expect it to happen in small values
  ### as we expect decaying freqs. It may happen because of rounding or randomly a few times, so we will look when this happens in values larger than 
  ### friki
  valores<-as.numeric(names(prospect)) ### extract the values
  ene<-length(valores)
  #### Achtung! Acomodar para length 0 de ene
  cutposition<-gdata::first(which(valores > friki))
  #cutposition<-gdata::first(which(diff(valores) > friki))+1
  if(length(cutposition)==0){return(NULL)} ### if there are no values longer than cutposition, returns NULL. Revisit
  if(!is.na(cutposition)){
    ojete<-valores[cutposition:ene]
    target<-which(x %in% ojete)
  }
  return(target)
}