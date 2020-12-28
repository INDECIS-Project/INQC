sunafterdark<-function(y,code='991274'){

  #' Maximum sunshine hours
  #' @description This function compares sunshine data to the maximum theoretical sunshine an ECA&D station, according the day, lat and lon.
  #' Maximum sunshine hours are computed from the 'suncalc' package, using "night" and "dawn" parameters.
  #' This contrasts quite a lot with other functions computing "daylength". This formulation is more conservative
  #' @param y ECA&D style two columns with date (yyyymmdd) and  values (expressed in 0.1 hours)
  #' @param code "numeric" part of the ECA&D SOUID, expressed as character, to avoid trouble with leading zeroes
  # @param casa the homefolder
  #' @return vector with the list of positions which do not pass this test. If all positions pass the test, returns NULL
  #' @details depends on either a previous execution of listas() or on a proper execution of listas() to run properly
  #' @seealso listas()
  #' @export

  bad<-NULL
  ##OS. The commands in 'if' operator below are exactly the 'listonator' function defined in the separate R-file
  #if(!exists("liston")){#### this takes quite a lot of time, and better if ran only once. Then, the list is declared as a global variable
  #  liston<-listas(rooty=casa)
  #  lat<-apply(as.data.frame(liston$LAT),1,FUN=decimaldegrees)
  #  lon<-apply(as.data.frame(liston$LON),1,FUN=decimaldegrees)
  #  coordinates<-data.frame(lat,lon)
  #  liston$LAT<-lat
  #  liston$LON<-lon
  #  #OS: I commented the line below because it cause a problem during check()
  #  #assign("liston",liston,envir = .GlobalEnv)
  #}
  ##OS. In the 'tntx' function the commands above were replaced by:
  ##if(!exists("liston")){listonator(home)} ### warning! not working here :-()
  ##OS. I had to comment this line and to introduce a new input parameter 'liston' to fix the note message. But I am not sure if this is correct.

  #Get value of 'Global variable' 'liston'
  liston <- getOption("liston")
  lat<-gdata::first(liston$LAT[which(liston$SOUID==code & substring(liston$ELEI,1,2)=='SS')]) ## prefixing with first because some stations are repeated. Assume lat/lon
  lon<-gdata::first(liston$LON[which(liston$SOUID==code & substring(liston$ELEI,1,2)=='SS')]) ## will not be too different in "susnhine" terms
  names(y)<-c('fecha','value')
  y$year<-substring(y$fecha,1,4)
  y$month<-substring(y$fecha,5,6)
  y$day<-substring(y$fecha,7,8)
  y<-y[,c(3:5,1:2)]
  targetyear<-round(mean(as.numeric(y$year)))
  while(targetyear%%4 != 0 & targetyear!=1900){targetyear=targetyear+1}
  targetstart<-as.Date(paste(targetyear,'01','01',sep='-'))
  targetdates<-seq(targetstart,targetstart+365,1)
  totest<-data.frame(targetdates,lat,lon);names(totest)<-c('date','lat','lon')
  totest$maxdur<-as.numeric(suncalc::getSunlightTimes(data = totest)$night-suncalc::getSunlightTimes(data = totest)$dawn)*10
  y$monthday<-paste(y$month,y$day,sep='-')
  totest$monthday<-substring(totest$date,6,10)
  zzpaff<-merge(y,totest);zzpaff<-zzpaff[order(zzpaff$fecha),]
  bad<-which(zzpaff$maxdur<zzpaff$value)
  return(bad)
}
