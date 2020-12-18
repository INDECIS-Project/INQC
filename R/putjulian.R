putjulian<-function(x){

  #' Merge julian days
  #' @description This function merges julian days to a yyyy,mm,dd and data
  #' @param x nxn with year,month,day and data columns
  #' @return nxn+1 columns with year,month,day, julian and data
  #' @export

  mes<-c(31,29,31,30,31,30,31,31,30,31,30,31)
  cumes<-cumsum(mes)
  cumes<-c(0,cumes)
  p<-cumes[x[,2]]+x[,3]
  colnum<-ncol(x)
  x<-cbind(x[,1:3],p,x[,4:colnum])
  return(x)
}