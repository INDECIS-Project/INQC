rounding<-function(y,blocksize=20){

  #' Detects rounded sections
  #' @description This function splits data by month and looks if a decimal value is repeated too many times
  #' @param y two columns with date in the ECA&D format (yyyymmdd) and data
  #' @param blocksize maximum number of repeated values with the same decimal allowed on each block (blocks = months)
  #' @return list of positions which do not pass this QC test. If all positions pass the test, returns NULL
  # @examples
  # y<-readecad('./inst/extdata/raw/TX_SOUID99094.txt')[,3:4]
  # y[1:50,2]<-round((y[1:50,2])/10)*10
  # rounding(y,blocksize=20)
  #' @export

  bad<-NULL
  y[,1]<-as.numeric(substring(y[,1],1,6))
  y[,2]<-y[,2]/10;y[,2]<-y[,2]-floor(y[,2])
  nyu<-as.data.frame(table(y[,1],y[,2])) ### this is good, it can identify those which are over the blocksize, but need to now how to extract/label the values
  if(nrow(nyu)==0){return(NULL)}
  target<-which(nyu[,3] >= blocksize)
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
