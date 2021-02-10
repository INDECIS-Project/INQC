jumps2<-function(date,value,quanty=0.999,times=1,force=NULL){

  #' Labels interdiurnal large differences
  #' @description This function looks for interdiurnal differences considered too large (larger than 
  #' a threshold value). The threshold can be defined by two different ways: (1) as an absolute value,
  #' the same for all differences. It is specified directly through the parameter 'force' (see below);
  #' (2) as a quantile in a probability distribution of the interdiurnal differences (built
  #' for each month separately). In this case, the threshold is specified indirectly through the 
  #' parameter 'quanty' (see below). The calculated threshold quantiles can be also modified 
  #' (increased/decreased) by means of the parameter 'times' (see below).
  #' 
  #' Consequently, jumps2() can be used in two different modes: 'absolute' and 'quantile'
  #' @param date vector of dates
  #' @param value vector of values
  #' @param quanty threshold quantile rank (cumulative probability) to define corresponding 
  #' quantiles in the distributions of the interdiurnal differences (for each month separately)
  #' @param times factor to modify (increase/decrease) the threshold quantile values 
  #' @param force value of threshold for daily value differences to be forced
  #' @return list of positions which do not pass this QC test
  #' @examples
  #' #Extract the ECA&D data file from the example data folder
  #' path2inptfl<-system.file("extdata", "TX_SOUID132734.txt", package = "INQC")
  #' #Read the data file
  #' date<-readecad(input=path2inptfl,missing= -9999)[,3]
  #' value<-readecad(input=path2inptfl,missing= -9999)[,4]
  #' #Find all suspicious positions in the time series (in 'quantile' mode)
  #' jumps2(date,value,quanty=0.999,times=1)
  #' #Find all suspicious positions in the time series (in 'absolute' mode)
  #' jumps2(date,value,force=100)
  #' @export

  chungo<-NULL
  x<-data.frame(month=as.numeric(substring(date,5,6)),value,date) ### create a data.frame with month, value, date
  x$difs<-0;x$difs[1]<-NA;x$difs[2:nrow(x)]<-diff(x$value) # creating the difs field
  targets<-stats::aggregate(x$difs,by=list(x$month),stats::quantile,probs=quanty,na.rm=TRUE)
  names(targets)<-c("month","threshold") # targets, by month
  ## merging, threshold is multiplied by times
  x<-merge(x,targets,all.x=TRUE)
  x<-x[order(x$date),]
  x$threshold=x$threshold*times
  if(!is.null(force)){x$threshold=force}
  ## this isolates differences over the threshold
  chungo<-which(abs(x$difs)>x$threshold)
  ene<-length(chungo)
  if(ene==0){return(NULL)}
  # here, we decided which member of the pair involved in a large difference is the culprit
  for(i in 1:ene){
    # rationale is , one by one, we look at which of both is most extreme in the ecdf, computed without them
    left=chungo[i]-1
    center=chungo[i]
    ### the next section makes the code robust to series where almost all data is NA. They should not be even QC'd, but not easy to monitori
    keke<-length(which(!is.na(x$value[-(left:center)])))
    if(keke>100){
      koko<-stats::ecdf(x$value[-(left:center)])      
      ecenter<-min(1-koko(x$value[center]),koko(x$value[center]))
      eleft<-min(1-koko(x$value[left]),koko(x$value[left]))
      if(eleft < ecenter){chungo[i]<-left}
    }
  }
  # need to use unique, as some values might be there twice (large difference with the previous and the following)
  chungo<-unique(chungo)
  return(chungo)
}