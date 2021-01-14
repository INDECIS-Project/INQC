roundprecip<-function(y,blocksize=20,exclude=0){

  #' Rounding in precipitation data
  #' @description This function splits data by month and looks if a decimal value is repeated too many times
  #' @param y two columns with date and data
  #' @param blocksize maximum number of repeated values with the same decimal
  #' @param exclude value to be excluded (zero for precipitation)
  #' @return list of positions which do not pass this QC test. If all positions pass the test, returns NULL
  #' @examples
  #' #Extract the ECA&D data file from the example data folder
  #' path2inptfl<-system.file("extdata", "RR_SOUID132730.txt", package = "INQC")
  #' #Read the data file
  #' y<-readecad(input=path2inptfl,missing= -9999)[,3:4]
  #' #Find all suspicious positions in the precipitation time series
  #' roundprecip(y,blocksize=20,exclude=0) 
  #' @export

  bad<-NULL 
  y[,1]<-as.numeric(substring(y[,1],1,6))
  ### Achtuuung!!!
  y[,3]<-as.integer(substring(y[,2],nchar(y[,2]),nchar(y[,2]))) ### This is a better way to find the decimal part in ECA&D: it is always the last character. 
  zerapio<-which(y[,2] %in% exclude | is.na(y[,2]))
  z<-y[-zerapio,]
  if(nrow(z) == 0){return(NULL)} ### This is to make it resistant to series with no values different than zero!
  #### Warning! This line was erroneous and was: 
  #nyu<-as.data.frame(table(y[,1],y[,2])) ### this is good, it can identify those which are over the blocksize, but need to now how to extract/label the values
  # This was causing the code to be really slow, as it was looking at "y" instead of "z". This was labeling most of the values as erroneous and the next loop was
  # taking forever.
  nyu<-as.data.frame(table(z[,1],z[,3])) ### this is good, it can identify those which are over the blocksize, but need to now how to extract/label the values
  target<-which(nyu[,3] >= blocksize)
  ene<-length(target)
  if(ene > 0){
    for(i in 1:ene){
      tirget<-nyu[target[i],]   
      fecha<-tirget[,1]
      valor<-tirget[,2]
      wanted<-which(y[,1] == fecha & y[,3] == valor) ## Modified: y[,3] now contains the "decimal" part. 
      if(i == 1){bad<-wanted}else{bad<-c(bad,wanted)}
    }
  }
  bad<-unique(bad)
  return(bad)
}