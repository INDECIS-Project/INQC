computecal=function(fy,ly){

  #' Prepares a calendar frame
  #' @description This function prepares a calendar frame and returns it as year,month,day (i.e., the 3 first columns of the RClimdex format)
  #' @param fy first year to work with (past)
  #' @param ly last year to work with (present)
  #' @return 3 columns containing year,month,day
  #' @examples
  #' fy<-1981
  #' ly<-2020
  #' clndr<-computecal(fy,ly)
  #' @export

  fy<-paste(fy,1,1,sep='/')
  ly<-paste(ly,12,31,sep='/')
  pepe<-seq(as.Date(fy),as.Date(ly),"days")
  jose<-as.numeric(substring(pepe,1,4))
  lito<-as.numeric(substring(pepe,6,7))
  lote<-as.numeric(substring(pepe,9,10))
  calframe<-cbind(jose,lito,lote)
  return(calframe)
}