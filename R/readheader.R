readheader<-function(input="SS_STAID000143.txt"){

  #' Reads the header of an ECA&D file
  #' @description This function reads one ECA&D file and returns the header (an introductory part of the ECA&D file), so it can be written in the same way
  #' @param input ECA&D filename
  #' @return header of an ECA&D file
  #' @examples
  #' #Extract the ECA&D data file from the example data folder
  #' path2inptfl<-system.file("extdata", "CC_SOUID132727.txt", package = "INQC")
  #' #Read the data file
  #' head<-readheader(input=path2inptfl)
  #' @export

  ##This is old version (v1.0)
  ##**************************
  #x<-utils::read.fwf(input,widths=300,stringsAsFactors=FALSE)
  #nyu<-grep('STAID,',x[,1])
  #return(x[1:nyu,1])
  ##**************************

  x<-utils::read.fwf(input,widths=300,stringsAsFactors=FALSE)
  y<-substring(gsub(" ","",x[,1]),1,5)
  nyu<-min(which(y=='STAID' | y =='SOUID'))
  return(x[1:nyu,1])
}