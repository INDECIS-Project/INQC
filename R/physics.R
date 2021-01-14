physics<-function(x,nyu=0,compare=1){

  #' Isolates anomalous values
  #' @description Given a data vector, the function will compare the values to the specified threshold
  #' @param x data vector
  #' @param nyu threshold, numeric
  #' @param compare logical operation to apply over the threshold. 1: larger; 2: larger or equal; 3: smaller; 4: smaller or equal; 5 equal
  #' @return list of positions which do not pass this QC test. If all positions pass the test, returns NULL
  #' @examples
  #' x<-rnorm(100)
  #' x[10]<-100
  #' physics(x,5,1)
  #' @export

  bad<-NULL
  if(compare==1){bad<-which(x > nyu)}
  if(compare==2){bad<-which(x >= nyu)}
  if(compare==3){bad<-which(x < nyu)}
  if(compare==4){bad<-which(x <= nyu)}
  if(compare==5){bad<-which(x == nyu)}
  return(bad)
}
