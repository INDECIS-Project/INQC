flat<-function(y,maxseq,exclude=NULL){

  #' Flat sequences
  #' @description This function detects consecutive equal values (e.g., 15.1, 15.1, 15.1, 15.1...). Also can be used to detect consecutive equal decimal part of the values (e.g., 15.1, 12.1, 13.1, 10.1 ...)
  #' @param y data vector
  #' @param maxseq the maximum number of contiguous repetitions of a value (e.g., if 3, sequences of 4 will be flagged)
  #' @param exclude values to be excluded. This is useful for variables where a single value is expected to repeat many times, e.g. 0.0 in precipitation.
  #' @return  list of positions which do not pass this QC test. If all positions pass the test, returns NULL
  #' @examples
  #' y<-rnorm(100)
  #' y[10:20]<-10
  #' flat(y,5)
  #'
  #' #Extract the ECA&D data file from the example data folder
  #' path2inptfl<-system.file("extdata", "TX_SOUID132734.txt", package = "INQC")
  #' #Read the data file
  #' y<-readecad(input=path2inptfl,missing= -9999)[,4]
  #' #Find all consecutive (with a length > 5 elements) equal values in the time series
  #' flat(y,5)
  #'
  #' #Introduce the duplicated dates
  #' y[6:12]<-10
  #' #Find all consecutive (with a length > 5 elements) equal values in the time series
  #' flat(y,5)
  #' @export

  target<-NULL
  mu<-rle(y)$lengths # sequences
  cumu<-cumsum(mu) # cumulative sum to find real position of a sequence
  longbad<-mu[which(mu>maxseq)] # length of each sequence
  # make resistant to nullbad
  if(length(longbad)!=0){
    bad<-cumu[which(mu>maxseq)] # last point of a sequence
    firstbad=(bad-longbad)+1 # first point of a sequence
    ene<-length(firstbad)
    for(i in 1:ene){
      if(i==1){target<-firstbad[i]:bad[i]}else{target<-c(target,firstbad[i]:bad[i])}
    }
    if(!is.null(exclude)){
      quitar<-which(y[target] == exclude)
      target<-target[-quitar]
    }
    return(target)
  }
}
