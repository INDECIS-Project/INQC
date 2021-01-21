selepe<-function(element='PP',large=15000,small=8000,maxjump=2000,maxseq=3,margina=0.999,
                 level=5,window=30,roundmax=10,blockmanymonth=15,blockmanyyear=180,
                 blocksizeround=20,qjump=0.999,tjump=1.5,
                 inisia=FALSE){

  #' QC for Atmospheric Pressure (PP)
  #' @description This function will centralize temperature-like QC routines. It will create a file in the folder QC
  #' with an additional 0/1 column where "1" means test failed.
  # @param home path to the home directory
  #' @param element two-letters ECA&D code for the element (PP for sea level pressure)
  #' @param large value above which the observation is considered physically impossible for the region
  #' @param small value below which the observation is considered physically impossible for the region
  #' @param maxjump forcing for jump2() in absolute mode (in the same units of the variable). Passed on to jump2(). See ?jump2 for further details.
  #' @param maxseq maximum number of consecutive repeated values, for flat function (11.1,11.1,11.1 would be 3 consecutive values)
  #' @param margina tolerance margin, expressed as quantile of the differences, FUNCTION: newfriki(). Passed on to newfriki(). See ?newfriki for details
  #' @param level number of IQRs for IQR outliers
  #' @param window window, in days, for IQR outliers
  #' @param roundmax maximum number of consecutive decimal part value, for flat function (10.0, 11.0, 12.0 would be 3 consecutive values)
  # @param blocksize such parameter (blocksize=10) was included into the arguments of the function but not used in the calculations
  # @param step such parameter (step=30) was included into the arguments of the function but not used in the calculations
  #' @param blockmanymonth maximum number of equal values in a month, FUNCTION: toomany()
  #' @param blockmanyyear maximum number of equal values in a year, FUNCTION: toomany()
  #' @param blocksizeround maximum number of values in a month with the same decimal, for rounding function
  #' @param qjump quantile for the calculated maximum jump allowed
  #' @param tjump factor to multiply the qjump computed differences
  #' @param inisia a logical flag. If it is TRUE inithome() will be called
  #' @return results of QC for PP
  #' @examples
  #' #Set a temporal working directory:
  #' wd <- tempdir()
  #' wd0 <- setwd(wd)
  #' #Create subdirectory where raw data files have to be located
  #' dir.create(file.path(wd, 'raw'))
  #' options("homefolder"='./'); options("blend"=FALSE)
  #' #Extract the ECA&D data and station files from the example data folder
  #' path2pplist<-system.file("extdata", "ECA_blend_source_pp.txt", package = "INQC")
  #' pplist<-readr::read_lines_raw(path2pplist)
  #' readr::write_lines(pplist,'ECA_blend_source_pp.txt')
  #' path2ppdata<-system.file("extdata", "PP_SOUID132729.txt", package = "INQC")
  #' ppdata<-readr::read_lines_raw(path2ppdata)
  #' readr::write_lines(ppdata, file=paste(wd,'/raw/PP_SOUID132729.txt',sep=''))
  #' #Perform QC of Atmospheric Pressure data
  #' selepe(inisia=TRUE)
  #' #Remove some temporary files
  #' list = list.files(pattern = "Rfwf")
  #' file.remove(list)
  #' #Return to user's working directory:
  #' setwd(wd0)
  #' #The QC results can be found in the directory:
  #' print(wd)
  #' @export

  #Suppress warning messages
  suppressWarnings(warning("selepe"))
  
  #Get values of 'Global variables' 'blend' and 'homefolder'
  blend <- getOption("blend")
  homefolder <- getOption("homefolder")
  if(inisia){inithome()}
  tx<-lister(element)
  ene<-length(tx)
  if(ene==0){return()}
  ### provisional
  for(i in 1:ene){
    name<-paste(homefolder,'raw/',tx[i],sep='')
    print(paste(name,i,'of',ene),quote=FALSE)
    x<-readecad(input=name) ; print(paste(Sys.time(),'Ended readecad'),quote=FALSE)
    if(!'STAID' %in% colnames(x) & ncol(x)==4){x<-cbind(STAID=as.numeric(substring(tx[i],9,14)),x)}
    x<-x[,1:4];colnames(x)<-c('STAID','SOUID','date','value') 
    bad<-weirddate(x[,3:4]);x$weirddate<-0;if(length(bad)!=0){x$weirddate[bad]<-1}; print(paste(Sys.time(),'Ended weirddate'),quote=FALSE)
    bad<-duplas(x$date);x$dupli<-0;if(length(bad)!=0){x$dupli[bad]<-1}; print(paste(Sys.time(),'Ended duplas'),quote=FALSE)
    bad<-physics(x$value,large,1);x$large<-0;if(length(bad)!=0){x$large[bad]<-1}; print(paste(Sys.time(),'Ended physics, large'),quote=FALSE)
    bad<-physics(x$value,small,3);x$small<-0;if(length(bad)!=0){x$small[bad]<-1}; print(paste(Sys.time(),'Ended physics, small'),quote=FALSE)
    bad<-jumps2(x$date,x$value,force=maxjump);x$jumpABS<-0;if(length(bad)!=0){x$jumpABS[bad]<-1}; print(paste(Sys.time(),'Ended jumps ABSOLUTE'),quote=FALSE)
    bad<-jumps2(x$date,x$value,qjump,tjump);x$jumpQUANT<-0;if(length(bad)!=0){x$jumpQUANT[bad]<-1}; print(paste(Sys.time(),'Ended jumps QUANTILE'),quote=FALSE)
    bad<-flat(x$value,maxseq);x$flat<-0;if(length(bad)!=0){x$flat[bad]<-1}; print(paste(Sys.time(),'Ended flat for values'),quote=FALSE) ## this looks at consecutive equal values
    bad<-flat(x$value%%10,roundmax);x$roundmax<-0;if(length(bad)!=0){x$roundmax[bad]<-1}; print(paste(Sys.time(),'Ended flat for decimal part'),quote=FALSE) #this looks at consecutive decimal parts
    bad<-toomany(x[,3:4],blockmanymonth,1);x$toomanymonth<-0;if(length(bad)!=0){x$toomanymonth[bad]<-1}; print(paste(Sys.time(),'Ended toomany, month'),quote=FALSE)
    bad<-toomany(x[,3:4],blockmanyyear,2);x$toomanyyear<-0;if(length(bad)!=0){x$toomanyyear[bad]<-1}; print(paste(Sys.time(),'Ended toomany, year'),quote=FALSE)
    bad<-newfriki(x$date,x$value,margina,times=3);x$friki<-0;if(length(bad)!=0){x$friki[bad]<-1} ; print(paste(Sys.time(),'Ended newfriki for errors'),quote=FALSE)
    bad<-newfriki(x$date,x$value,margina,times=1.5);x$frikilight<-0;if(length(bad)!=0){x$frikilight[bad]<-1} ; print(paste(Sys.time(),'Ended newfriki for suspect'),quote=FALSE)
    bad<-IQRoutliers(x$date,x$value,level,window);x$IQRoutliers<-0;if(length(bad)!=0){x$IQRoutliers[bad]<-1}; print(paste(Sys.time(),'Ended IQRoutliers'),quote=FALSE) # check
    bad<-rounding(x[,3:4],blocksizeround);x$rounding<-0;if(length(bad)!=0){x$rounding[bad]<-1}; print(paste(Sys.time(),'Ended rounding'),quote=FALSE)
    consolidator(tx[i],x)
  }
}