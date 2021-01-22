snowdepth<-function(element='SD',maxseq=20,blocksizeround=20,blockmanymonth=20,blockmanyyear=200,large=5000,exclude=0,inisia=FALSE){

  #' QC for Snow Depth (SD)
  #' @description This function will centralize temperature-like QC routines. It will create a file in the folder QC
  #' with an additional 0/1 column, where "1" means test failed.
  # @param home path to the home directory
  #' @param element two-letters ECA&D code for the element (SD for snow depth)
  #' @param maxseq maximum number of consecutive repeated values, FUNCTION: flat() (11.1,11.1,11.1 would be 3 consecutive values)
  #' @param blocksizeround maximum number of values in a month with the same decimal, FUNCTION: rounding()
  #' @param blockmanymonth maximum number of equal values in a month, FUNCTION: toomany()
  #' @param blockmanyyear maximum number of equal values in a year, FUNCTION: toomany()
  #' @param large value above which the observation is considered physically impossible for the region, FUNCTION: physics()
  #' @param exclude value to be excluded from a function (in this case, 0 for flats)
  #' @param inisia logical flag. If it is TRUE inithome() will be called
  #' @return results of QC for SD
  #' @examples
  #' #Set a temporal working directory:
  #' wd <- tempdir()
  #' wd0 <- setwd(wd)
  #' #Create subdirectory where raw data files have to be located
  #' dir.create(file.path(wd, 'raw'))
  #' options("homefolder"='./'); options("blend"=FALSE)
  #' #Extract the ECA&D data and station files from the example data folder
  #' path2sdlist<-system.file("extdata", "ECA_blend_source_sd.txt", package = "INQC")
  #' sdlist<-readr::read_lines_raw(path2sdlist)
  #' readr::write_lines(sdlist,'ECA_blend_source_sd.txt')
  #' path2sddata<-system.file("extdata", "SD_SOUID132731.txt", package = "INQC")
  #' sddata<-readr::read_lines_raw(path2sddata)
  #' readr::write_lines(sddata, file=paste(wd,'/raw/SD_SOUID132731.txt',sep=''))
  #' #Perform QC of Snow Depth data
  #' snowdepth(inisia=TRUE)
  #' #Remove some temporary files
  #' list = list.files(pattern = "Rfwf")
  #' file.remove(list)
  #' #Return to user's working directory:
  #' setwd(wd0)
  #' #The QC results can be found in the directory:
  #' print(wd)
  #' @export

  #Get values of 'Global variables' 'blend' and 'homefolder'
  blend <- getOption("blend")
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
    bad<-toomany(x[,3:4],blockmanymonth,1,exclude=0);x$toomanymonth<-0;if(length(bad)!=0){x$toomany[bad]<-1}; print(paste(Sys.time(),'Ended toomany, month'),quote=FALSE)
    bad<-toomany(x[,3:4],blockmanyyear,2,exclude=0);x$toomanyyear<-0;if(length(bad)!=0){x$toomany[bad]<-1}; print(paste(Sys.time(),'Ended toomany, year'),quote=FALSE)
    bad<-roundprecip(x[,3:4],blocksizeround,exclude =0);x$rounding<-0;if(length(bad)!=0){x$rounding[bad]<-1};  print(paste(Sys.time(),'Ended roundsnow'),quote=FALSE)
    bad<-physics(x$value,large,1);x$large<-0;if(length(bad)!=0){x$large[bad]<-1}; print(paste(Sys.time(),'Ended physics, large'),quote=FALSE)
    bad<-physics(x$value,0,3);x$small<-0;if(length(bad)!=0){x$small[bad]<-1}; print(paste(Sys.time(),'Ended physics, small'),quote=FALSE)
    bad<-flat(x$value,maxseq,exclude=0);x$flat<-0;if(length(bad)!=0){x$flat[bad]<-1}; print(paste(Sys.time(),'Ended flat for values'),quote=FALSE)
    consolidator(tx[i],x)
  }
}
