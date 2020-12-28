txtnblend<-function(y,id){

  #' Comparison of tx an tn
  #' @description This function first looks for the closest station and then merges both data frames.
  #' If one value is flagged, looks at the ecdfs of tx and tn. If the target variable (e.g tx) is central
  #' (between quantiles 0.2 and 0.8) and the other variable (e.g. tn) is outside this range, the value is
  #' not flagged, assuming the other variable is the culprit
  #' @param y two columns with date and data
  #' @param id id we are working with
  #' @return list of positions which do not pass this QC test. 
  #' @export

  #Get value of 'Global variable' 'homefolder'
  homefolder <- getOption("homefolder")
  bad<-NULL
  whoami<-substring(id,1,2)
  if(whoami=='TX'){targetvariable='TN'}else{targetvariable='TX'}
  id2<-id;substring(id2,1,2)<-targetvariable; targetfile<-paste0(homefolder,'raw/',id2)
  if(file.exists(targetfile)){z<-readecad(targetfile)}else{return(bad)}
  zcols<-c(ncol(z)-2,ncol(z)-1);z<-z[,zcols] ### keep dates and values
  zz<-merge(y,z,all.x=TRUE,all.y=FALSE)### do not be fooled! all.y referres to the second variable!
  if(targetvariable=='TN'){bad<-which(zz[,2]<=zz[,3])}
  if(targetvariable=='TX'){bad<-which(zz[,2]>=zz[,3])}
  return(bad)
}