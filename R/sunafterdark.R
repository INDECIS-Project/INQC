sunafterdark<-function(y,code="991274"){

  #' Maximum sunshine hours (only for "non-blended" ECA&D data)
  #' @description This function compares sunshine data to the maximum theoretical sunshine at an ECA&D station, 
  #' according the day, lat and lon. Maximum sunshine hours are computed from the "suncalc" package, using 
  #' "night" and "dawn" parameters. This contrasts quite a lot with other functions computing "daylength". 
  #' This formulation is more conservative
  #' @param y ECA&D style two columns with date (yyyymmdd) and  values (expressed in 0.1 hours)
  #' @param code "numeric" part of the ECA&D SOUID, expressed as character, to avoid trouble with leading zeroes
  #' @return vector with the list of positions which do not pass this test. If all positions pass the test, returns NULL
  #' @details depends on either a previous execution of listas() or on a proper execution of listas() to run properly
  #' @seealso listas()
  #' @examples
  #' #Set a temporal working directory:
  #' wd <- tempdir(); wd0 <- setwd(wd)
  #' #Extract the non-blended ECA&D data and a station file from the example data folder
  #' path2sslist<-system.file("extdata", "ECA_blend_source_ss.txt", package = "INQC")
  #' sslist<-readr::read_lines_raw(path2sslist)
  #' readr::write_lines(sslist,'ECA_blend_source_ss.txt')
  #' path2ssdata<-system.file("extdata", "SS_SOUID132728.txt", package = "INQC")
  #' #Read the sunshine data
  #' y<-readecad(input=path2ssdata,missing= -9999)[,3:4]
  #' options("homefolder"='./'); options("blend"=FALSE)
  #' listonator(check=TRUE)
  #' #Call sunafterdark()
  #' sunafterdark(y,code='132728')
  #' #Introduce error values in the sunshine data
  #' y[1:10,2]<-200
  #' #Call sunafterdark()
  #' sunafterdark(y,code='132728')
  #' #Return to user's working directory:
  #' setwd(wd0)
  #' @export

  bad<-NULL
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