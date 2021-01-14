flatsun<-function(x,maxseq,id,modonube=FALSE){

  #' Flat sequences for sunshine duration
  #' @description This function uses flat() and modifies it with "smart" comparison with clouds.
  #' If close to 8 and close to 0 clouds, allowed; if close to maxsundur and clouds near 0, allowed
  #  CAUTION: While E. Coyote Genius Masterpiece updated to test cloud flats; Rationale: add "modononube"; if TRUE, changes are made to end
  #  up with "sun" in x and "clouds" in y thereafter, everything remains the same.
  #' @param x data.frame date/value (need dates in this implementation of flat)
  #' @param maxseq maximum number of contiguous repetitions of a value (e.g., if 3, sequences of 4 will be flagged)
  #' @param id SOUID_SSxxxxxx.txt
  # @param casa does not include raw, e.g. '../mickeymouse/'
  #' @param modonube logical flag
  # @param liston a list file with all time series
  #' @return list of positions which do not pass this QC test
  #' @export

  bad<-NULL
  ##if(!exists("liston")){listonator(casa)}
  #Get values of 'Global variables' 'liston' and 'homefolder'
  liston <- getOption("liston")
  homefolder <- getOption("homefolder")
  milista<-unique(liston)
  if(!modonube){me<-gdata::first(which(milista$SOUID == substring(id,9,14) & substring(milista$ELEI,1,2)=='SS'))}
  if(modonube){me<-gdata::first(which(milista$SOUID == substring(id,9,14) & substring(milista$ELEI,1,2)=='CC'))}
  if(!modonube){cloud<-gdata::first(which(milista$STAID == milista$STAID[me] & milista$PARID == milista$PARID[me] & substring(milista$ELEI,1,2)=='CC'))}
  if(modonube){cloud<-gdata::first(which(milista$STAID == milista$STAID[me] & milista$PARID == milista$PARID[me] & substring(milista$ELEI,1,2)=='SS'))}
 
  if(length(cloud)==0){bad<-flat(x[,2],maxseq,exclude=100);return(bad)}
  if(!is.null(cloud) &!(is.na(cloud))){
    if(!modonube){y<-readecad(paste0(homefolder,'raw/CC_SOUID',milista$SOUID[cloud],'.txt'))[,3:4]}
    if(modonube){y<-x;x<-readecad(paste0(homefolder,'raw/SS_SOUID',milista$SOUID[cloud],'.txt'))[,3:4]}
    names(x)<-c('date','sun');names(y)<-c('date','cloud')
    if(!modonube){z<-merge(x,y,all.x=TRUE,all.y=FALSE)}
    if(modonube){z<-merge(x,y,all.x=FALSE,all.y=TRUE)}
    maximo<-length(which(!is.na(z[,2])));real<-length(which(!is.na(z[,2]) & !is.na(z[,3])))
    evaluation=real/maximo
    if(!modonube & evaluation < 0.80){bad<-flat(x[,2],maxseq,exclude=100);return(bad)}
    if(modonube & evaluation < 0.80){bad<-flat(x[,2],maxseq,exclude=c(0,8));return(bad)}
    if(!modonube){bid<-flat(z[,2],maxseq)}
    if(modonube){bid<-flat(z[,3],maxseq)}
    if(length(bid)==0){return(bad)}
    z$flat<-0;z$flat[bid]<-1
    z$difs<-0;z$difs[1]<-NA;z$difs[2:nrow(z)]<-diff(z$flat)
    if(z$flat[1]==1){z$difs[1]<-1}
    start=which(z$difs == 1)
    end=which(z$difs == -1)-1
    if(z$flat[nrow(z)]==1){end<-c(end,nrow(z))}
    juntos<-data.frame(start,end)
    for(i in 1:nrow(juntos)){juntos$sol[i]<-mean(z$sun[juntos$start[i]:juntos$end[i]],na.rm=TRUE)}
    for(i in 1:nrow(juntos)){juntos$nube[i]<-mean(z$cloud[juntos$start[i]:juntos$end[i]],na.rm=TRUE)}
    # for sunshine: if no sun a clouds more than 7, unflagged
    if(!modonube){for(i in 1:nrow(juntos)){if(!is.na(juntos$sol[i]) & !is.na(juntos$nube[i])){if(juntos$sol[i]< 1 & juntos$nube[i] > 7){z$flat[juntos$start[i]:juntos$end[i]]<-0}}}}
    juntos$date<-z$date[start]
    juntos$date<-as.Date(paste(substring(juntos$date,1,4),substring(juntos$date,5,6),substring(juntos$date,7,8),sep='-'))
    juntos$lat<-milista$LAT[me];juntos$lon<-milista$LON[me]
     totest<-data.frame(juntos$date,juntos$lat,juntos$lon);names(totest)<-c('date','lat','lon')     
    juntos$maxsun<-juntos$sol/(as.numeric(suncalc::getSunlightTimes(data=totest)$night-suncalc::getSunlightTimes(data=totest)$dawn)*10)*100       ## proportion to the maximum sun          
    juntos$maxsun[which(is.na(juntos$maxsun))]<-0 ### need this beacuse getSunlight times gives an unexplained NA sometimes
    #### If proportion to maximum sun is larger than 90 and clouds smaller than 1, allowed (sun mode)
    if(!modonube){for(i in 1:nrow(juntos)){if(!is.na(juntos$sol[i]) & !is.na(juntos$nube[i])){if(juntos$maxsun[i]> 90 & juntos$nube[i] < 2){z$flat[juntos$start[i]:juntos$end[i]]<-0}}}}
    #### If proportion to maximum sun is larger than 70 and clouds smaller than 1, allowed (cloud mode)
    if(modonube){for(i in 1:nrow(juntos)){if(!is.na(juntos$sol[i]) & !is.na(juntos$nube[i])){if(juntos$maxsun[i]> 70 & juntos$nube[i] < 2){z$flat[juntos$start[i]:juntos$end[i]]<-0}}}}
    #### If proportion to maximum sun is smaller than 10 and clouds larger than 7, allowed (cloud mode)
    if(modonube){for(i in 1:nrow(juntos)){if(!is.na(juntos$sol[i]) & !is.na(juntos$nube[i])){if(juntos$maxsun[i]< 10 & juntos$nube[i] > 7){z$flat[juntos$start[i]:juntos$end[i]]<-0}}}}
    bad<-which(z$flat == 1)
    return(bad)
  }
}