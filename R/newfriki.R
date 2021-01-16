newfriki<-function(date,value,margina=0.999,times=2){

  #' Isolates values which are not continuous in the distribution
  #' @description The function isolates extreme values which are not continuous in the distribution. If 
  #' the gap is larger than a pre-set big margin, the values above (or below) are flagged
  #' @param date vector of dates with the ECA&D format yyyymmdd
  #' @param value vector of data values
  #' @param margina tolerance margin, expressed as quantile of the differences
  #' @param times multiplier for the tolerance margin. Intended usage is to run this twice. Once with times = 1 and flag values as suspect; once with times = 2 and flag as error
  #' @return positions which do not pass this QC test
  #' @export

  bad<-NULL
  y<-data.frame(date,value)
  for(ij in 1:12){
    position<-which(as.numeric(substring(y$date,5,6))==ij) # find the positions for the month ij
    mes<-y[position,]  # subset the positions
    sorted<-sort(mes$value)  # sort the values
    diffy<-(diff(sorted)) # create the first difference of the sorted values
    margen<-stats::quantile(diffy,margina)
    malo<-which(diffy > times*margen) # isolate the values 
    if(length(malo!=0)){
      chungo<-sorted[malo] # isolate the values 
      qchungo<-stats::ecdf(sorted)(chungo) 
      for(k in 1:length(qchungo)){
        if(qchungo[k] > 0.5){fechachungo<-y$date[which(y$value > chungo[k] & as.numeric(substring(y$date,5,6))==ij)]} #### If it is in the second half of the distro
        if(qchungo[k] <=0.5){fechachungo<-y$date[which(y$value <= chungo[k] & as.numeric(substring(y$date,5,6))==ij)]} #### It if is in the first half of the distro 
        if(exists('fechas')){fechas<-c(fechas,fechachungo)}else{fechas<-fechachungo}
      }
    }
  }
  if(exists('fechas')){bad<-which(date%in% fechas)}
  return(bad)
}
