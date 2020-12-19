paretogadget<-function(x,ret){

  #' It should be described later
  #' @description This function should be described later
  #' @param x the series you are going to use (will take precip)
  #' @param ret the pseudo return period (remember is pareto and I destroyed the years)
  #' @return list of values which did not pass the test
  #' @export

  # Auxiliated by potpareto, returnpotpareto, computecal
  target<-NULL
  nyu<-potpareto(x);if(is.null(nyu)){return(NULL)}
  mus<-returnpotpareto(nyu,ret)
  target<-which(x > mus)
  return(target)
}