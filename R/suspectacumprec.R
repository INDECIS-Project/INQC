suspectacumprec<-function(datos,limit=2000,tolerance=10){

  #' Detects precipitation values above limit
  #' @description This function detects values above limit preceded by a number of "non precipitation days", given by tolerance
  #' @param datos two columns vector, date and data, in the ECA&D format
  #' @param limit threshold/limit value for atmospheric precipitation
  #' @param tolerance how many consecutive days with 0 or NA you need to jump
  #' @return list of positions which do not pass this QC test
  #' @examples
  #' #Extract the ECA&D data file from the example data folder
  #' path2inptfl<-system.file("extdata", "RR_SOUID132730.txt", package = "INQC")
  #' #Read the data file
  #' datos<-readecad(input=path2inptfl,missing= -9999)[,3:4]
  #' #Find all suspicious positions in the precipitation time series
  #' suspectacumprec(datos,limit=2000,tolerance=10)
  #' @export

  bisco<-NULL
  datos$year<-as.numeric(substring(datos[,1],1,4))
  datos$month<-as.numeric(substring(datos[,1],5,6))
  datos$day<-as.numeric(substring(datos[,1],7,8))
  datos<-datos[,c(3,4,5,2)]
  ni<-tolerance+1
  y<-datos
  x<-datos
  fy<-min(x[,1],na.rm=TRUE)
  ly<-max(x[,1],na.rm=TRUE)
  p<-computecal(fy,ly)
  x<-x[,1:4]
  px<-merge(p,x,by.x=c(1,2,3),by.y=c(1,2,3),all.x=TRUE,all.y=TRUE)
  x<-px[,4]
  target<-which(x>=limit)
  ne<-length(target)
  chungo<-0
  if(ne!=0){
    for(i in 1:ne){
      if(target[i]>tolerance){
        nyu<-target[i]-tolerance
        nyi<-target[i]-1
        k<-sum(x[nyu:nyi],na.rm=TRUE)
        if(k==0){chungo<-c(chungo,nyu:target[i])}
        #if(k==0){chungo<-c(chungo,target[i])}
      }
    }
  }
  if(length(chungo)>1){
    chungo<-chungo[-1]
  }
  for(jj in 1:length(chungo)){
    busco<-which(y[,1] == px[chungo[jj],1] & y[,2] == px[chungo[jj],2] & y[,3] == px[chungo[jj],3])
    if(jj==1){bisco<-busco}else{bisco<-c(bisco,busco)}
  } # there must be a more elegant way to do this than with a loop, but works
  #Qachtung
  return(bisco) ### need to revert this to real indices, it seems that they're altered by computecal! [CHECK!!!!!!!!! ACHTUUUUNGGGGG!!!!!]
}
