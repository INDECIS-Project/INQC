weirddate<-function(x){

  #' Locate impossible dates
  #' @description This function is intended to flag impossible dates (e.g., 19990230 or 29990112, etc)
  #' @param x two-columns dataframe. First column is date in the ECA&D format (yyyymmdd), second columns is value
  # @param minyear a numeric value identifiying the first year which can contain data. Defaulted to 1800, understanding that most series
  # will not have 18th century data. Customize according to the dataset.
  #' @return list of positions which do not pass this QC test. If all positions pass the test, returns NULL
  #' @examples
  #' #Extract the ECA&D data file from the example data folder
  #' path2inptfl<-system.file("extdata", "TX_SOUID132734.txt", package = "INQC")
  #' #Read the data file
  #' x<-readecad(input=path2inptfl,missing= -9999)[,3:4]
  #' #Find all suspicious positions in the time series
  #' weirddate(x)
  #'
  #' #Introduce the weird dates
  #' x[31,1]<-'19610132'
  #' #Find all suspicious positions in the time series
  #' weirddate(x) 
  #' @export

  ##This is old version (v1.0)
  ##**************************
  #max<-Sys.Date();max<-as.numeric(paste0(substring(max,1,4),substring(max,6,7),substring(max,9,10)))
  #bad<-NULL
  #fy<-as.numeric(substring(x[1,1],1,4))
  #ly<-as.numeric(substring(x[nrow(x),1],1,4))
  #x$year<-as.numeric(substring(x[,1],1,4));x$month<-as.numeric(substring(x[,1],5,6));x$day<-as.numeric(substring(x[,1],7,8))
  #k1<-which(x[,1] > max)
  #k8<-which(x$year < minyear)
  #k2<-which(x$day > 31)
  #k3<-which(x$day > 30 & x$month %in% c(4,6,9,11))
  #k4<-which(x$day > 29 & x$month ==2)
  #k5<-which(x$day > 28 & x$month == 2 & x$year%%4 !=0 )
  #k6<-which(x$day > 28 & x$month == 2 & x$year == 1900 )
  #k7<-which(x$month < 1 | x$month > 12)
  #bad<-unique(c(k1,k2,k3,k4,k5,k6,k7,k8))
  #return(bad)
  ##**************************

  bad<-NULL
  fy<-as.numeric(substring(x[1,1],1,4))
  ly<-as.numeric(substring(x[nrow(x),1],1,4))
  x$year<-as.numeric(substring(x[,1],1,4));x$month<-as.numeric(substring(x[,1],5,6));x$day<-as.numeric(substring(x[,1],7,8))
  k1<-which(x$year < fy | x$year > ly)
  k2<-which(x$day > 31)
  k3<-which(x$day > 30 & x$month %in% c(4,6,9,11))
  k4<-which(x$day > 29 & x$month ==2)
  k5<-which(x$day > 28 & x$month == 2 & x$year%%4 !=0 )
  k6<-which(x$day > 28 & x$month == 2 & x$year == 1900 )
  k7<-which(x$month < 1 | x$month > 12)
  bad<-c(k1,k2,k3,k4,k5,k6,k7)
  return(bad)
}
