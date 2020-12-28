putjulian<-function(x){

  #' Merges julian days
  #' @description This function merges julian days to a yyyy,mm,dd and data
  #' @param x data frame with year, month, day and data columns
  #' @return the same data frame with added 1 column: year, month, day, julian and data
  #' @examples
  #' date<-c('20201230','20201231','20210101')
  #' value<-c(-10,-12,-9)
  #' df<-data.frame(date,value)
  #' year<-as.numeric(substring(date,1,4))
  #' month<-as.numeric(substring(date,5,6))
  #' day<-as.numeric(substring(date,7,8))
  #' x<-data.frame(year,month,day,date,value)
  #' y<-putjulian(x)
  #' @export

  mes<-c(31,29,31,30,31,30,31,31,30,31,30,31)
  cumes<-cumsum(mes)
  cumes<-c(0,cumes)
  p<-cumes[x[,2]]+x[,3] #OS: This is the great solution: simple and effective!
  colnum<-ncol(x)
  x<-cbind(x[,1:3],p,x[,4:colnum])
  return(x)
}