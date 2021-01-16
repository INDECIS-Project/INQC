IQRoutliers<-function(date,value,level=3,window=11,exclude=NULL){

  #' Computes outliers
  #' @description This function computes outliers centralized around a day, using a number of days around it
  #' @param date vector with dates
  #' @param value vector with data values
  #' @param level number of IQRs to be added to percentile 75 and subtracted to percentile 25 to 
  #' determinate the tolerance interval. Values outside this interval, will be declared as outliers.
  #' @param window number of days to be considered (including the target)
  #' @param exclude if it is not null, the code will exclude this value from the analysis (i.e., good to exclude 0 for precipitation)
  #' @return positions which do not pass this QC test
  #' @examples
  #' #Extract the ECA&D data file from the example data folder
  #' path2inptfl<-system.file("extdata", "TX_SOUID132734.txt", package = "INQC")
  #' #Read the data file
  #' date<-readecad(input=path2inptfl,missing= -9999)[,3]
  #' value<-readecad(input=path2inptfl,missing= -9999)[,4]
  #' #Find all suspicious positions in the time series
  #' IQRoutliers(date,value,level=3,window=11,exclude=NULL)
  #' @export

  bad<-NULL
  year<-as.numeric(substring(date,1,4))
  month<-as.numeric(substring(date,5,6))
  day<-as.numeric(substring(date,7,8))
  modulo<-(window-1)/2
  y<-data.frame(year,month,day,date,value)
  y<-putjulian(y)
  y$qc<-0
  julios<-1:366
  luego<-1:modulo
  antes<-(366-modulo+1):366
  julios<-c(antes,julios,luego)
  yy<-y ### this works the magic for exclude.
  if(!is.null(exclude)){target<-which(y$value != exclude);y<-y[target,]}
  for(j in (modulo+1):(modulo+366)){
    target<-julios[(j-5):(j+5)]
    valores<-y$value[y$p %in% target] # hasta consigue seleccionar valores centrados en el dia juliano definido por j
    valores<-y[y$p %in% target,] # hasta consigue seleccionar valores centrados en el dia juliano definido por j
    p75<-stats::quantile(valores$value,0.75,na.rm=TRUE)
    p25<-stats::quantile(valores$value,0.25,na.rm=TRUE)
    iqr<-stats::IQR(valores$value,na.rm=TRUE)
    above<-p75+level*iqr
    below<-p25-level*iqr
    bud<-which(valores$value > above | valores$value < below)
    if(length(bud != 0 )){
      if(exists('fecha')){fecha<-c(fecha,valores$date[bud])}else{fecha<-valores$date[bud]}
    }
  }
  #if(exists('fecha')){bad<-which(y$date %in% fecha)
  if(exists('fecha')){
    bad<-which(yy$date %in% fecha) ### this makes shure that, even using exclude, the returned bad values are correct
    return(bad)
  }
}