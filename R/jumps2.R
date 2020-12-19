jumps2<-function(date,value,quanty=0.999,times=1,force=NULL){

  #' Labels interdiurnal differences considered too large
  #' @description The function labels interdiurnal differences considered too large
  #' @param date it should be described later
  #' @param value vector of values
  #' @param quanty it should be described later
  #' @param times it should be described later
  #' @param force it should be described later
  #' @return chungo both values involved in the jump are returned. Need an additional function to decide which is the culprit
  #' @export

  chungo<-NULL
  x<-data.frame(month=as.numeric(substring(date,5,6)),value,date) ### create a data.frame with month, value, date
  x$difs<-0;x$difs[1]<-NA;x$difs[2:nrow(x)]<-diff(x$value) # creating the difs field
  targets<-stats::aggregate(x$difs,by=list(x$month),stats::quantile,probs=quanty,na.rm=TRUE);names(targets)<-c("month","threshold") # targets, by month
  ## merging, threshold is multiplied by times
  x<-merge(x,targets,all.x=TRUE)
  x<-x[order(x$date),]
  x$threshold=x$threshold*times
  if(!is.null(force)){x$threshold=force}
  ## this isolates  differences over the threshold
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