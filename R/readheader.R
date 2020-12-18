readheader<-function(input="SS_STAID000143.txt"){

  #' Read the header of an ECA&D file
  #' @description Reads one ECA&D file and returns the header, so it can be written in the same way
  #' @param input an ECA&D filename
  #' @return The header of an ECA&D file
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