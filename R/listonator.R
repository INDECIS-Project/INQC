listonator<-function(check=TRUE){

  #' Creates a list (as 'Global' variable) of stations to be QCed.
  #' @description This function creates a list (and makes it 'Global' variable) of stations to be QCed.
  #' It can be 'blended' or 'non-blended' stations. Geographical coordinates are converted into decimal degrees
  #' @param check logical parameter TRUE/FALSE. If check=TRUE a list of stations is created.
  #' @return list of stations to be QCed
  #' @examples
  #' #Set a temporal working directory:
  #' wd <- tempdir(); wd0 <- setwd(wd)
  #' #NON-BLENDED ECA&D SERIES
  #' #Extract the non-blended ECA&D station files from the example data folder
  #' #Only TX (maximum air temperature) and CC (cloud cover) variables are used in the example
  #' path2txlist<-system.file("extdata", "ECA_blend_source_tx.txt", package = "INQC")
  #' txlist<-readr::read_lines_raw(path2txlist)
  #' readr::write_lines(txlist,'ECA_blend_source_tx.txt')
  #' path2cclist<-system.file("extdata", "ECA_blend_source_cc.txt", package = "INQC")
  #' cclist<-readr::read_lines_raw(path2cclist)
  #' readr::write_lines(cclist,'ECA_blend_source_cc.txt')
  #' options("homefolder"='./'); options("blend"=FALSE)
  #' listonator(check=TRUE)
  #' liston.nb<-getOption("liston")
  #' #BLENDED ECA&D SERIES
  #' #Create subdirectory where a station file has to be located
  #' dir.create(file.path(wd, 'raw'))
  #' #Extract the blended ECA&D station file from the example data folder
  #' path2list<-system.file("extdata", "stations.txt", package = "INQC")
  #' list<-readr::read_lines_raw(path2list)
  #' readr::write_lines(list,file=paste(wd,'/raw/stations.txt',sep=''))
  #' options("blend"=TRUE)
  #' listonator(check=TRUE)
  #' liston.b<-getOption("liston")
  #' #Return to user's working directory:
  #' setwd(wd0)
  #' @export

  #Get values of 'Global variables' 'blend' and 'homefolder'
  blend <- getOption("blend")
  homefolder <- getOption("homefolder")
  if(!exists("liston") & blend & check){ #.............................List of 'blended' stations will be created
    liston<-readecad(paste0(homefolder,'raw/stations.txt'))
    names(liston)<-c('STAID','STANAME','CN','LAT','LON','HGHT')
  }else{ #.........................................................List of 'non-blended' stations will be created
    if(!exists("liston") & isTRUE(check)){# this takes quite a lot of time, and better if ran only once.
                                          # Then, the list is declared as a global variable
      liston<-listas()
    }
  }
  #for(nyu in 1:nrow(liston)){decimaldegrees(liston$LAT[nyu]);print(nyu)}
  lat<-apply(as.data.frame(liston$LAT),1,FUN=decimaldegrees)
  lon<-apply(as.data.frame(liston$LON),1,FUN=decimaldegrees)
  coordinates<-data.frame(lat,lon) 
  liston$LAT<-lat
  liston$LON<-lon
  options("liston"=liston)
}
