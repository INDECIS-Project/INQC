duplas<-function(x){

  #' Detect duplicated dates
  #' @description This function detects duplicated dates
  #' @param x a vector of dates in ECA&D format (YYYYMMDD)
  #' @return a vector with the list of positions which do not pass this test. If all positions pass the test, returns NULL
  #' @export

  total<-NULL
  antes<- which(duplicated(x,fromLast=TRUE)==TRUE)
  despues<- which(duplicated(x,fromLast=FALSE)==TRUE)
  if(length(antes)!=0){ #OS. It seems an error was here (compare with the original code)
    total<-c(antes,despues)
    return(total)
  }
}
