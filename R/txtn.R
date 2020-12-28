txtn<-function(y,id){

  #' Comparison of tx an tn data
  #' @description This function compares tx an tn data. First it looks for the closest station and then merges both
  #' data frames. If one value is flagged, looks at the ecdfs of tx and tn. If the target variable (e.g tx) is central
  #' (between quantiles 0.2 and 0.8) and the other variable (e.g. tn) is outside this range, the value is not flagged,
  #' assuming the other variable is the culprit.
  #' @param y two columns with date and data
  #' @param id id we are working with
  # @param home home folder, need to add "raw" inside the fucntion
  # @param liston a list file with all time series
  #' @return list of positions which do not pass this QC test. If all positions pass the test, returns NULL
  #' @export

  #Get values of 'Global variables' 'liston' and 'homefolder'
  liston <- getOption("liston")
  homefolder <- getOption("homefolder")
  bad<-NULL
  #if(!exists("liston")){listonator(home)} ### warning! not working here :-()
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
