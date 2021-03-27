lister<-function(element){

  #' Creates a list of blended/non-bladed files for some climate variable
  #' @description This function creates a list of blended or non-bladed files containing data of a specified element
  #' to be QCed.
  #' @param element climatological element (defined by means of two letters, i.e. 'TX')
  #' @return list of blended or non-bladed files to be QCed
  #' @examples
  #' #Set a temporal working directory:
  #' wd <- tempdir(); wd0 <- setwd(wd)
  #' #Create subdirectory where a station file has to be located
  #' dir.create(file.path(wd, 'raw'))
  #' #NON-BLENDED ECA&D SERIES
  #' #Extract the non-blended ECA&D data and station files from the example data folder
  #' #Only TX (maximum air temperature) and CC (cloud cover) data are used in the example
  #' path2txdata<-system.file("extdata", "TX_SOUID132734.txt", package = "INQC")
  #' txdata<-readr::read_lines_raw(path2txdata)
  #' readr::write_lines(txdata, file=paste(wd,'/raw/TX_SOUID132734.txt',sep=''))
  #' path2ccdata<-system.file("extdata", "CC_SOUID132727.txt", package = "INQC")
  #' ccdata<-readr::read_lines_raw(path2ccdata)
  #' readr::write_lines(ccdata, file=paste(wd,'/raw/CC_SOUID132727.txt',sep=''))
  #' options("homefolder"='./'); options("blend"=FALSE)
  #' list.nb<-lister('TX')
  #' #BLENDED ECA&D SERIES
  #' #Extract the blended ECA&D data and station files from the example data folder
  #' #Only TX (maximum air temperature) and CC (cloud cover) data are used in the example
  #' path2txdata<-system.file("extdata", "TX_STAID000002.txt", package = "INQC")
  #' txdata<-readr::read_lines_raw(path2txdata)
  #' readr::write_lines(txdata, file=paste(wd,'/raw/TX_STAID000002.txt',sep=''))
  #' path2ccdata<-system.file("extdata", "CC_STAID000001.txt", package = "INQC")
  #' ccdata<-readr::read_lines_raw(path2ccdata)
  #' readr::write_lines(ccdata, file=paste(wd,'/raw/CC_STAID000001.txt',sep=''))
  #' options("blend"=TRUE)
  #' list.b<-lister('TX')
  #' #Return to user's working directory:
  #' setwd(wd0)
  #' @export

  #Get values of 'Global variables' 'blend' and 'homefolder'
  blend <- getOption("blend")
  homefolder <- getOption("homefolder")
  if(blend){
    lista<-list.files(path=paste(homefolder,'raw',sep=''),pattern='STAID')
  }else{
    lista<-list.files(path=paste(homefolder,'raw',sep=''),pattern='SOUID')
  }
  tx<-lista[which(substring(lista,1,2)==element)]
  return(tx)
}