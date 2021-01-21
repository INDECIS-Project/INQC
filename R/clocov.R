clocov<-function(element='CC',maxseq=8,blocksizeround=20,blockmanymonth=20,blockmanyyear=200,inisia=FALSE){

  #' QC for Cloud Cover (CC)
  #' @description This function will centralize temperature-like QC routines. It will create a file in the folder QC
  #' with an additional 0/1 column, where "1" means test failed.
  # @param home path to the home directory
  #' @param element two-letters ECA&D code for the element (CC for cloud cover)
  #' @param maxseq maximum number of consecutive repeated values, FUNCTION: flat  (11.1,11.1,11.1 would be 3 consecutive values).
  #' @param blocksizeround maximum number of values in a month with the same decimal, FUNCTION: rounding
  #' @param blockmanymonth maximum number of equal values in a month, FUNCTION: toomany
  #' @param blockmanyyear maximum number of equal values in a year, FUNCTION: toomany
  #' @param inisia a logical flag. If it is TRUE inithome() will be called
  #' @return QC results for CC
  #' @examples
  #' #Set a temporal working directory:
  #' wd <- tempdir()
  #' wd0 <- setwd(wd)
  #' #Create subdirectory where raw data files have to be located
  #' dir.create(file.path(wd, 'raw'))
  #' options("homefolder"='./'); options("blend"=FALSE)
  #' #Extract the ECA&D data and station files from the example data folder
  #' path2cclist<-system.file("extdata", "ECA_blend_source_cc.txt", package = "INQC")
  #' cclist<-readr::read_lines_raw(path2cclist)
  #' readr::write_lines(cclist,'ECA_blend_source_cc.txt')
  #' path2ccdata<-system.file("extdata", "CC_SOUID132727.txt", package = "INQC")
  #' ccdata<-readr::read_lines_raw(path2ccdata)
  #' readr::write_lines(ccdata, file=paste(wd,'/raw/CC_SOUID132727.txt',sep=''))
  #' #Perform QC of Cloud Cover data
  #' clocov(inisia=TRUE)
  #' #Remove some temporary files
  #' list = list.files(pattern = "Rfwf")
  #' file.remove(list)
  #' #Return to user's working directory:
  #' setwd(wd0)
  #' #The QC results can be found in the directory:
  #' print(wd)
  #' @export

  #Suppress warning messages
  suppressWarnings(warning("clocov"))
  
  #Get values of 'Global variables' 'blend' and 'homefolder'
  #blend <- getOption("blend")
  homefolder <- getOption("homefolder")
  if(inisia){inithome()}
  tx<-lister(element)
  ene<-length(tx)
  if(ene==0){return()}
  for(i in 1:ene){
    name<-paste(homefolder,'raw/',tx[i],sep='')
    print(paste(name,i,'of',ene),quote=FALSE)
    x<-readecad(input=name) ; print(paste(Sys.time(),'Ended readecad'),quote=FALSE)
    if(!'STAID' %in% colnames(x) & ncol(x)==4){x<-cbind(STAID=as.numeric(substring(tx[i],9,14)),x)}
    x<-x[,1:4];colnames(x)<-c('STAID','SOUID','date','value')
    bad<-duplas(x$date);x$dupli<-0;if(length(bad)!=0){x$dupli[bad]<-1} ; print(paste(Sys.time(),'Ended duplas'),quote=FALSE)
    bad<-weirddate(x[,3:4]);x$weirddate<-0;if(length(bad)!=0){x$weirddate[bad]<-1}; print(paste(Sys.time(),'Ended weirddate'),quote=FALSE)
    bad<-toomany(x[,3:4],blockmanymonth,1);x$toomanymonth<-0;if(length(bad)!=0){x$toomanymonth[bad]<-1}; print(paste(Sys.time(),'Ended toomany, month'),quote=FALSE)
    bad<-toomany(x[,3:4],blockmanyyear,2);x$toomanyyear<-0;if(length(bad)!=0){x$toomanyyear[bad]<-1}; print(paste(Sys.time(),'Ended toomany, year'),quote=FALSE)
    bad<-flat(x$value,maxseq);x$flat<-0;if(length(bad)!=0){x$flat[bad]<-1}; print(paste(Sys.time(),'Ended flat for values'),quote=FALSE)
    bad<-flatsun(x[,3:4],maxseq,tx[i],TRUE);x$flatsun<-0;if(length(bad)!=0){x$flatsun[bad]<-1}; print(paste(Sys.time(),'Ended flatsun'),quote=FALSE)
    bad<-rounding(x[,3:4],blocksizeround);x$rounding<-0;if(length(bad)!=0){x$rounding[bad]<-1}; print(paste(Sys.time(),'Ended rounding'),quote=FALSE)
    bad<-physics(x$value,8,1);x$large<-0;if(length(bad)!=0){x$large[bad]<-1}; print(paste(Sys.time(),'Ended physics, large'),quote=FALSE)
    bad<-physics(x$value,0,3);x$small<-0;if(length(bad)!=0){x$small[bad]<-1}; print(paste(Sys.time(),'Ended physics, small'),quote=FALSE)
    consolidator(tx[i],x)
  }
}
