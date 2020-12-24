paretogadget<-function(x,ret){

  #' Finds outliyers
  #' @description This function finds outliyers for variables which can be described/evaluated by means of the Pareto distribution (e.g. atmospheric precipitation or wind speed)
  #' @param x a vector of values (a series) to be analyzed
  #' @param ret a pseudo return period
  #' @return a list of positions where corresponding values do not pass this QC test (which can be considered as outliyers)
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