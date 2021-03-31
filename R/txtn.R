txtn<-function(y,id){

  #' Comparison of tx an tn data (for "non-blended" ECA&D data)
  #' @description This function compares tx an tn data. First it looks for the closest station and then merges both
  #' data frames. If one value is flagged, looks at the ecdfs of tx and tn. If the target variable (e.g tx) is central
  #' (between quantiles 0.2 and 0.8) and the other variable (e.g. tn) is outside this range, the value is not flagged,
  #' assuming the other variable is the culprit
  #' @param y two columns with date and data
  #' @param id name of a file ("xx_SOUIDxxxxxx.txt", non-blended) we are working with
  #' @return list of positions which do not pass this QC test. If all positions pass the test, returns NULL
  #' @examples
  #' #Set a temporal working directory:
  #' wd <- tempdir(); wd0 <- setwd(wd)
  #' #Create subdirectory where raw data files have to be located
  #' dir.create(file.path(wd, "raw"))
  #' #Extract the non-blended ECA&D data and station files from the example data folder
  #' path2txlist<-system.file("extdata", "ECA_blend_source_tx.txt", package = "INQC")
  #' txlist<-readr::read_lines_raw(path2txlist)
  #' readr::write_lines(txlist,"ECA_blend_source_tx.txt")
  #' path2txdata<-system.file("extdata", "TX_SOUID132734.txt", package = "INQC")
  #' txdata<-readr::read_lines_raw(path2txdata)
  #' readr::write_lines(txdata, file=paste(wd,"/raw/TX_SOUID132734.txt",sep=""))
  #' path2tnlist<-system.file("extdata", "ECA_blend_source_tn.txt", package = "INQC")
  #' tnlist<-readr::read_lines_raw(path2tnlist)
  #' readr::write_lines(tnlist,"ECA_blend_source_tn.txt")
  #' path2tndata<-system.file("extdata", "TN_SOUID132733.txt", package = "INQC")
  #' tndata<-readr::read_lines_raw(path2tndata)
  #' readr::write_lines(tndata, file=paste(wd,"/raw/TN_SOUID132733.txt",sep=""))
  #' #Read the tn data
  #' y<-readecad(input=path2tndata,missing= -9999)[,3:4]
  #' options("homefolder"="./"); options("blend"=FALSE)
  #' listonator(check=TRUE)
  #' #Call txtn()
  #' txtn(y,"TN_SOUID132733.txt")
  #' #Introduce error values in the tn data
  #' y[c(1,3),2]<-100
  #' #Call txtn()
  #' txtn(y,"TN_SOUID132733.txt")
  #' #Return to user's working directory:
  #' setwd(wd0)
  #' @export

  #Get values of 'Global variables' 'liston' and 'homefolder'
  liston <- getOption("liston")
  homefolder <- getOption("homefolder")
  bad<-NULL
  whoami<-substring(id,1,2);if(whoami=='TX'){targetvariable='TN'}else{targetvariable='TX'}
  me<-gdata::first(which(liston$SOUID == substring(id,9,14)))
  em<-gdata::first(which(liston$STAID == liston$STAID[me] & liston$PARID==liston$PARID[me] & liston$ELEI==paste0(targetvariable,substring(liston$ELEI[me],3))))
  ### comprobar si tenemos estaciones. Pensar si hay mas de una que se hace. 
  if(length(em)==0){return(bad)}
  cerca<-sprintf('%s_SOUID%06d.txt', targetvariable, liston$SOUID[em]) 
  name<-paste(homefolder,'raw/',cerca,sep='')
  if(!file.exists(name)){return(bad)}
  x<-readecad(name);x<-x[,3:4]
  if(whoami == 'TX'){names(x)<-c('date','tn');names(y)<-c('date','tx')}
  if(whoami == 'TN'){names(y)<-c('date','tn');names(x)<-c('date','tx')}
  z<-merge(x,y,all.y=TRUE,all.x=FALSE)
  bad<-which(z$tx <= z$tn)
  if(length(bad)==0){return(bad)}

  z$month<-as.factor(as.numeric(substring(z$date,5,6)))
  z$bad<-0;z$bad[bad]<-1
  for(i in 1:length(bad)){
    mes<-which(z$month==z$month[bad[i]])
    mes<-mes[-bad[i]]
    etx<-abs(stats::ecdf(z$tx[mes])(z$tx[bad[i]])-1);etn<-abs(stats::ecdf(z$tn[mes])(z$tn[bad[i]])-1)
    if(targetvariable=='TX' & etn > 0.8 & etx < 0.8){z$bad[bad[i]]==0}
    if(targetvariable=='TN'& etx > 0.8 & etn < 0.8){z$bad[bad[i]]==0}
  }
return(bad)
}
