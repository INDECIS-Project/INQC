paretogadget<-function(x,ret){

  #' Finds outliers
  #' @description This function finds outliers for variables which can be described/evaluated by means of the Pareto distribution (e.g. atmospheric precipitation or wind speed)
  #' @param x vector of values (a series) to be analyzed
  #' @param ret pseudo return period
  #' @return list of positions which do not pass this QC test (which can be considered as outliers)
  #' @export

  # Auxiliated by potpareto, returnpotpareto, computecal
  target<-NULL
  nyu<-potpareto(x)
  if(is.null(nyu)){
    return(NULL)
  }
  mus<-returnpotpareto(nyu,ret)
  target<-which(x > mus)
  return(target)
}