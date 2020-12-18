decimaldegrees <- function(dms,sep = ":"){

  #' Converter of geograpical coordinates from the ECA&D format into decimal degrees
  #' @description This function takes sexagesimal degrees in the ECA&D format and converts them into decimal degrees.
  #' Initial idea taken from: https://modtools.wordpress.com/2013/09/25/dms2dec/
  #' @param dms ONE ELEMENT from the LAT or LON field in ECA&D listings
  #' @param sep the separator between elements, in ECA&D ":"
  #' @return geographical coordinates (latitude or longitude) in decimal degrees
  #' @export

  deg <- as.numeric(unlist(strsplit(dms, split = sep))[1])
  min <- as.numeric(unlist(strsplit(dms, split = sep))[2])
  sec <- as.numeric(unlist(strsplit(dms, split = sep))[3])

  #OS. I slightly corrected the original code below
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
