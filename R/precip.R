precip<-function(element='RR',large=5000,small=0,ret=500,retornoracha=500,margin=20,friki=150,blocksizeround=20,
                 blockmanymonth=15,blockmanyyear=180,limit=1500,tolerance=8,maxseq=3,roundmax=10,level=15,window=30,
                 margina=0.999,inisia=FALSE){

  #' QC for Atmospheric Precipitation (RR)
  #' @description This function will centralize precipitation-like QC routines. It will create a file in the folder QC
  #' with an additional 0/1 column, where "1" means test failed.
  # @param home path to the home directory
  #' @param element two-letters ECA&D code for the element (RR for precipitation)
  #' @param large value above which the observation is considered physically impossible for the region
  #' @param small value below which the observation is considered physically impossible for the region
  #' @param ret pseudo-return period for the pareto outliers
  #' @param retornoracha return period for the calculation of the maximum dry and wet spell
  #' @param margin frequency difference between consecutive values for repeatedvalue()
  #' @param friki minimum value to be considered by repeatedvalue()
  #' @param blocksizeround maximum number of repeated values with the same decimal, FUNCTION: roundprecip()
  #' @param blockmanymonth maximum number of equal values in a month, FUNCTION: toomany()
  #' @param blockmanyyear maximum number of equal values in a year, FUNCTION: toomany()
  #' @param limit cut threshold for FUNCTION suspectacumprec()
  #' @param tolerance number of NA or 0s before allowed before the limit, FUNCTION: suspectacumprec()
  #' @param maxseq maximum number of consecutive repeated values, FUNCTION: flat() (11.1,11.1,11.1 would be 3 consecutive values)
  #' @param roundmax maximum number of consecutive decimal part values, FUNCTION: flat() (10.0, 11.0, 12.0 would be 3 consecutive values)
  #' @param level number of IQRs, FUNCTION: IQRoutliers()
  #' @param window number of days to be considered (including the target), FUNCTION: IQRoutliers()
  #' @param margina a tolerance margin, expressed as quantile of the differences, FUNCTION: newfriki()
  #' @param inisia a logical flag. If it is TRUE inithome() will be called
  # NOTE: parameters exclude, excluido and alike are not included anymore. No need to parameterize the obvious and unchanging: always
  # need to exclude 0 for precipitation
  #' @return results of QC for RR
  #' @examples
  #' #Set a temporal working directory:
  #' wd <- tempdir()
  #' wd0 <- setwd(wd)
  #' #Create subdirectory where raw data files have to be located
  #' dir.create(file.path(wd, 'raw'))
  #' options("homefolder"='./'); options("blend"=FALSE)
  #' #Extract the ECA&D data and station files from the example data folder
  #' path2rrlist<-system.file("extdata", "ECA_blend_source_rr.txt", package = "INQC")
  #' rrlist<-readr::read_lines_raw(path2rrlist)
  #' readr::write_lines(rrlist,'ECA_blend_source_rr.txt')
  #' path2rrdata<-system.file("extdata", "RR_SOUID132730.txt", package = "INQC")
  #' rrdata<-readr::read_lines_raw(path2rrdata)
  #' readr::write_lines(rrdata, file=paste(wd,'/raw/RR_SOUID132730.txt',sep=''))
  #' #Perform QC of Atmospheric Precipitation data
  #' precip(inisia=TRUE)
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
  ### provisional
  for(i in 1:ene){
    name<-paste(homefolder,'raw/',tx[i],sep='')
    print(paste(name,i,'of',ene),quote=FALSE)
    x<-readecad(input=name) ; print(paste(Sys.time(),'Ended readecad'),quote=FALSE)
    if(!'STAID' %in% colnames(x) & ncol(x)==4){x<-cbind(STAID=as.numeric(substring(tx[i],9,14)),x)}
    x<-x[,1:4];colnames(x)<-c('STAID','SOUID','date','value') 
    bad<-duplas(x$date);x$dupli<-0;if(length(bad)!=0){x$dupli[bad]<-1} ; print(paste(Sys.time(),'Ended duplas '),quote=FALSE)
    bad<-weirddate(x[,3:4]);x$weirddate<-0;if(length(bad)!=0){x$weirddate[bad]<-1}; print(paste(Sys.time(),'Ended weirddate'),quote=FALSE)
    bad<-roundprecip(x[,3:4],blocksizeround,exclude =0);x$rounding<-0;if(length(bad)!=0){x$rounding[bad]<-1};  print(paste(Sys.time(),'Ended roundprecip'),quote=FALSE)
    bad<-repeatedvalue(x[,4],margin,friki);x$repeatedvalue<-0;if(length(bad)!=0){x$repeatedvalue[bad]<-1} ; print(paste(Sys.time(),'Ended repeatedvalue'),quote=FALSE)
    bad<-drywetlong(x[,4],retornoracha);x$drywetlong<-0;if(length(bad)!=0){x$drywetlong[bad]<-1} ; print(paste(Sys.time(),'Ended drywetlong'),quote=FALSE)
    bad<-physics(x$value,large,1);x$large<-0;if(length(bad)!=0){x$large[bad]<-1} ; print(paste(Sys.time(),'Ended physics, large'),quote=FALSE)
    bad<-physics(x$value,small,3);x$small<-0;if(length(bad)!=0){x$small[bad]<-1} ; print(paste(Sys.time(),'Ended physics, small'),quote=FALSE)
    bad<-suspectacumprec(x[,3:4],limit,tolerance);x$suspectacumprec<-0;if(length(bad)!=0){x$suspectacumprec[bad]<-1} ; print(paste(Sys.time(),'Ended suspectacumprec'),quote=FALSE)
    bad<-paretogadget(x[,4],ret);x$paretogadget<-0;if(length(bad)!=0){x$paretogadget[bad]<-1} ; print(paste(Sys.time(),'Ended paretogadget'),quote=FALSE)
    bad<-toomany(x[,3:4],blockmanymonth,1,0);x$toomanymonth<-0;if(length(bad)!=0){x$toomany[bad]<-1}; print(paste(Sys.time(),'Ended toomany, month'),quote=FALSE)
    bad<-toomany(x[,3:4],blockmanyyear,2,0);x$toomanyyear<-0;if(length(bad)!=0){x$toomany[bad]<-1}; print(paste(Sys.time(),'Ended toomany, year'),quote=FALSE)
    bad<-newfriki(x$date,x$value,margina,times=3);x$friki<-0;if(length(bad)!=0){x$friki[bad]<-1} ; print(paste(Sys.time(),'Ended newfriki for errors'),quote=FALSE)
    bad<-newfriki(x$date,x$value,margina,times=1.5);x$frikilight<-0;if(length(bad)!=0){x$frikilight[bad]<-1} ; print(paste(Sys.time(),'Ended newfriki for suspect'),quote=FALSE)
    bad<-flat(x$value,maxseq,exclude=0);x$flat<-0;if(length(bad)!=0){x$flat[bad]<-1}; print(paste(Sys.time(),'Ended flat for values. Sequences with 0 are excluded'),quote=FALSE)
    bad<-flat(x$value%%10,roundmax,exclude=0);x$roundmax<-0;if(length(bad)!=0){x$roundmax[bad]<-1}; print(paste(Sys.time(),'Ended flat for decimal part'),quote=FALSE) #this looks at consecutive decimal parts
    bad<-IQRoutliers(x[,3],x[,4],exclude=0,window=window,level = level);x$IQRoutliers<-0;if(length(bad)!=0){x$IQRoutliers[bad]<-1}; print(paste(Sys.time(),'Ended IQROutliers'),quote=FALSE)
    consolidator(tx[i],x)
  }
}
