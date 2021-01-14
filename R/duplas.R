duplas<-function(x){

  #' Detects duplicated dates
  #' @description This function detects duplicated dates in the input time series
  #' @param x vector of dates in the ECA&D format (YYYYMMDD)
  #' @return vector with the list of positions which do not pass this QC test. If all positions pass the test, returns NULL
  #' @examples
  #' #Extract the ECA&D data file from the example data folder
  #' path2inptfl<-system.file("extdata", "TX_SOUID132734.txt", package = "INQC")
  #' #Read the data file
  #' x<-readecad(input=path2inptfl,missing= -9999)[,3]
  #' #Find all duplicated dates in the time series
  #' duplas(x)
  #'
  #' #Introduce the duplicated dates
  #' x[31]<-'19610130'
  #' #Find all duplicated dates in the time series
  #' duplas(x) 
  #' @export

  total<-NULL
  antes<- which(duplicated(x,fromLast=TRUE)==TRUE)
  despues<- which(duplicated(x,fromLast=FALSE)==TRUE)
  if(length(antes)!=0){ #OS. It seems an error was here (please, compare with the original code)
    total<-c(antes,despues)
    return(total)
  }
}
