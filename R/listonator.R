listonator<-function(check=TRUE){

  #' Creates a list (as 'Global' variable) of stations to be QCed.
  #' @description This function creates a list (and makes it 'Global' variable) of stations to be QCed.
  #' It can be 'blended' or 'non-blended' stations. Geographical coordinates are transformed into decimal degrees
  # @param casa home directory where the "ECA_blend_source*" files are located(?).
  #' @param check logical parameter TRUE/FALSE. If check=TRUE a list of stations is created.
  #' @return list of stations to be QCed
  #' @export

  ##This is old version (v1.0)
  ##**************************
  #if(!exists("liston") & isTRUE(check)){ ## this takes quite a lot of time, and better if ran only once. Then, the list is declared as a global variable
  #  liston<-listas(rooty=casa)
  #  #for(nyu in 1:nrow(liston)){decimaldegrees(liston$LAT[nyu]);print(nyu)}
  #  lat<-apply(as.data.frame(liston$LAT),1,FUN=decimaldegrees)
  #  lon<-apply(as.data.frame(liston$LON),1,FUN=decimaldegrees)
  #  coordinates<-data.frame(lat,lon)
  #  liston$LAT<-lat
  #  liston$LON<-lon
  #  #OS: I commented the line below
  #  #assign("liston",liston,envir = .GlobalEnv)
  #}
  ##**************************
  
  #Get values of 'Global variables' 'blend' and 'homefolder'
  blend <- getOption("blend")
  homefolder <- getOption("homefolder")
  if(!exists("liston") & blend & check){
    liston<-readecad(paste0(homefolder,'raw/stations.txt'))
    names(liston)<-c('STAID','STANAME','CN','LAT','LON','HGHT')
  }else{
    if(!exists("liston") & isTRUE(check)){#### this takes quite a lot of time, and better if ran only once. Then, the list is declared as a global variable
      liston<-listas()
    }
  }
  #for(nyu in 1:nrow(liston)){decimaldegrees(liston$LAT[nyu]);print(nyu)}
  lat<-apply(as.data.frame(liston$LAT),1,FUN=decimaldegrees)
  lon<-apply(as.data.frame(liston$LON),1,FUN=decimaldegrees)
  coordinates<-data.frame(lat,lon) 
  liston$LAT<-lat
  liston$LON<-lon
  #assign("liston",liston,envir = .GlobalEnv)
  options("liston"=liston)
}
