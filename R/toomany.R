toomany<-function(y,blockmany=15,scope=1,exclude=NULL){

  #' Looks if a value is repeated too many times
  #' @description This function splits data by month and looks if a value is repeated too many times
  #' @param y two columns with date and data
  #' @param blockmany maximum number of repeated values in a month, year, or season
  #' @param scope monthly (1), annual (2)
  #' @param exclude values to exclude, e.g. if precipitation, 0 must be excluded
  #' @return list of positions which do not pass this QC test. If all positions pass the test, returns NULL
  #' @examples
  #' #Extract the ECA&D data file (maximum air temperature) from the example data folder
  #' path2inptfl<-system.file("extdata", "TX_SOUID132734.txt", package = "INQC")
  #' #Read the data file
  #' y<-readecad(input=path2inptfl,missing= -9999)[,3:4]
  #' #Introduce the errors in first 20 data values
  #' y[1:20,2]<-30
  #' #Find all suspicious positions in the time series
  #' toomany(y,blockmany=15,scope=1,exclude=NULL)
  #'
  #' #Extract the ECA&D data file (atmospheric precipitation) from the example data folder
  #' path2inptfl<-system.file("extdata", "RR_SOUID132730.txt", package = "INQC")
  #' #Read the data file
  #' y<-readecad(input=path2inptfl,missing= -9999)[,3:4]
  #' #Introduce the errors in first 20 data values
  #' y[1:20,2]<-10
  #' #Find all suspicious positions in the time series
  #' toomany(y,blockmany=15,scope=1,exclude=0)
  #' @export

  bad<-NULL
  if(scope == 1){y[,1]<-as.numeric(substring(y[,1],1,6))}
  if(scope == 2){y[,1]<-as.numeric(substring(y[,1],1,4))}
  nyu<-as.data.frame(table(y[,1],y[,2])) ### this is good, it can identify those which are over the blocksize, but need to now how to extract/label the values
  if(nrow(nyu)==0){return(NULL)}
  if(!is.null(exclude)){
    #target<-which(nyu[,3] >= blockmany & nyu[,2] %nin% exclude)
    target<-which(nyu[,3] >= blockmany & !(nyu[,2]%in%exclude)) #OS. Is not this the same?
  }else{
    target<-which(nyu[,3] >= blockmany)
  }
  ### notice I am using the Hmisc operator %nin%, as !%in% does not exist as negation of the base %in%

  ene<-length(target)
  if(ene > 0){
    for(i in 1:ene){
      tirget<-nyu[target[i],]   
      fecha<-tirget[,1]
      valor<-tirget[,2]
      wanted<-which(y[,1] == fecha & y[,2] == valor)
      if(i == 1){bad<-wanted}else{bad<-c(bad,wanted)}
    }
  }
  bad<-unique(bad)
  return(bad)
}