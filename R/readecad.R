readecad<-function(input="SS_STAID000143.txt",missing= -9999){

  #' Reads an ECA&D data/sources/stations file
  #' @description  This function reads one ECA&D file and puts it in yyyy/mm/dd/value. Data is NOT divided by 10, to transform it into true units
  #' @param input ECA&D filename
  #' @param missing missing value code, set to the default ECA&D mvc
  # @return a series with yyyy/mm/dd/value format: n rows; 4 columns (it seems to be incorrect!)
  #' @return data frame containing data (time series) from the ECA&D file. An introductory part of the ECA&D file with meta data information is skipped
  #' @examples
  #' #Extract the ECA&D data file from the example data folder
  #' path2inptfl<-system.file("extdata", "CC_SOUID132727.txt", package = "INQC")
  #' #Read the data file
  #' df<-readecad(input=path2inptfl,missing= -9999) 
  #' @export

  ##This is old version (v1.0)
  ##**************************
  #x<-utils::read.fwf(input,widths=15)
  #nyu<-grep('STAID,',x[,1])
  #x<-utils::read.table(input,na.strings=missing,skip=nyu,sep=',',header=FALSE,stringsAsFactors=FALSE)
  #return(x)
  ##**************************

  x<-utils::read.fwf(input,widths=15)
  x<-substring(gsub(" ","",x[,1]),1,5)
  nyu<-min(which(x=='STAID' | x=='SOUID'))
  nyu<-nyu-1
  #OS. Commands above are used only to define how many lines in the header of the ECA&D file have to be skipped
  #x<-utils::read.table(input,na.strings=missing,skip=nyu+1,sep=',',header=FALSE,stringsAsFactors=FALSE)
  x<-utils::read.table(input,na.strings=missing,skip=nyu,sep=',',header=TRUE,stringsAsFactors=FALSE)
  return(x)
}