decimaldegrees <- function(dms,sep = ":"){

  #' Converter for geographical coordinates from the ECA&D format into decimal degrees
  #' @description This function takes sexagesimal degrees in the ECA&D format and converts them into decimal degrees.
  #' Initial idea was taken from: https://modtools.wordpress.com/2013/09/25/dms2dec/
  #' @param dms ONE ELEMENT from the LAT or LON field in ECA&D listings
  #' @param sep the separator between elements, in ECA&D ":"
  #' @return geographical coordinates (latitude or longitude) in decimal degrees
  #' @examples
  #' dms<-'+48:03:00'
  #' dec<-decimaldegrees(dms)
  #'
  #' dms<-'-015:03:00'
  #' dec<-decimaldegrees(dms)
  #' @export

  deg <- as.numeric(unlist(strsplit(dms, split = sep))[1])
  min <- as.numeric(unlist(strsplit(dms, split = sep))[2])
  sec <- as.numeric(unlist(strsplit(dms, split = sep))[3])

  if(deg<0){
    negative=TRUE
    deg <- -1*deg
  }else{
    negative=FALSE
  }
  dec <- as.numeric(deg) + (as.numeric(min)/60) + (as.numeric(sec)/3600)
  if(negative){dec <- -1*dec}
  return(dec)
}
