selepe<-function(large=15000,small=8000,maxjump=2000,maxseq=3,margina=0.999,
                 level=5,window=30,roundmax=10,blocksize=10,step=30,blockmanymonth=15,blockmanyyear=180,
                 blocksizeround=20,qjump=0.999,tjump=1.5,
                 element='PP',inisia=FALSE){

  #' QC for Atmospheric Pressure (PP)
  #' @description This function will centralize temperature-like qc routines. It will create a file in the folder QC
  #' with an additional 0/1 column where "1" means test failed.
  # @param home path to the home directory
  #' @param large value above which the observation is considered physically impossible for the region
  #' @param small value below which the observation is considered physically impossible for the region
  #' @param maxjump it should be described later
  #' @param maxseq maximum number of consecutive repeated values, for flat function (11.1,11.1,11.1 would be 3 consecutives)
  #' @param margina quantile for newfriki function
  #' @param level number of IQRs for IQR outliers
  #' @param window window, in days, for IQR outliers
  #' @param roundmax maximum number of consecutive decimal part value, for flat function (10.0, 11.0, 12.0 would be 3 consecutive)
  #' @param blocksize should be described later
  #' @param step should be described later
  #' @param blockmanymonth maximum number of equal values in a month, FUNCTION: toomany
  #' @param blockmanyyear maximum number of equal values in a yaer, FUCNTION: toomany
  #' @param blocksizeround maximum number of values in a month with the same decimal, for rounding function
  #' @param qjump quantile for the calculated maximum jump allowed
  #' @param tjump factor to multiply the qjump computed differences
  #' @param element two-letters ECA&D code for the element (PP for sea level pressure)
  #' @param inisia a logical flag. If it is TRUE inithome() will be called
  #' @return results of QC for PP
  #' @export

  ##This is old version (v1.0)
  ##**************************
  #lista<-list.files(path=paste(home,'raw',sep=''),pattern='SOUID')
  #tx<-lista[which(substring(lista,1,2)==element)];ene<-length(tx)
  #if(length(tx)==0){return()}
  #for(i in 1:ene){
  #  name<-paste(home,'raw/',tx[i],sep='')
  #  print(paste(name,i,'of',ene),quote=FALSE)
  #  x<-readecad(input=name) ; print(Sys.time())
  #  x<-x[,1:4];colnames(x)<-c('STAID','SOUID','date','value')
  #  #pattern(x[,4])
  #  bad<-weirddate(x[,3:4]);x$weirddate<-0;if(length(bad)!=0){x$weirddate[bad]<-1}; print(paste(Sys.time(),'Ended weirddate'),quote=FALSE)
  #  bad<-duplas(x$date);x$dupli<-0;if(length(bad)!=0){x$dupli[bad]<-1}; print(paste(Sys.time(),'Ended duplas'),quote=FALSE)
  #  bad<-physics(x$value,large,1);x$large<-0;if(length(bad)!=0){x$large[bad]<-1}; print(paste(Sys.time(),'Ended physics, large'),quote=FALSE)
  #  bad<-physics(x$value,small,3);x$small<-0;if(length(bad)!=0){x$small[bad]<-1}; print(paste(Sys.time(),'Ended physics, small'),quote=FALSE)
  #  bad<-jumps2(x$date,x$value,force=maxjump);x$jumpABS<-0;if(length(bad)!=0){x$jumpABS[bad]<-1}; print(paste(Sys.time(),'Ended jumps ABSOLUTE'),quote=FALSE)
  #  bad<-jumps2(x$date,x$value,qjump,tjump);x$jumpQUANT<-0;if(length(bad)!=0){x$jumpQUANT[bad]<-1}; print(paste(Sys.time(),'Ended jumps QUANTILE'),quote=FALSE)
  #  bad<-flat(x$value,maxseq);x$flat<-0;if(length(bad)!=0){x$flat[bad]<-1}; print(paste(Sys.time(),'Ended flat for values'),quote=FALSE) ## this looks at consecutive equal values
  #  bad<-flat(x$value%%10,roundmax);x$roundmax<-0;if(length(bad)!=0){x$roundmax[bad]<-1}; print(paste(Sys.time(),'Ended flat for decimal part'),quote=FALSE) #this looks at consecutive decimal parts
  #  bad<-toomany(x[,3:4],blockmanymonth,1);x$toomanymonth<-0;if(length(bad)!=0){x$toomanymonth[bad]<-1}; print(paste(Sys.time(),'Ended toomany, month'),quote=FALSE)
  #  bad<-toomany(x[,3:4],blockmanyyear,2);x$toomanyyear<-0;if(length(bad)!=0){x$toomanyyear[bad]<-1}; print(paste(Sys.time(),'Ended toomany, year'),quote=FALSE)
  #  bad<-newfriki(x$date,x$value,margina,times=3);x$friki<-0;if(length(bad)!=0){x$friki[bad]<-1} ; print(paste(Sys.time(),'Ended newfriki for errors'),quote=FALSE)
  #  bad<-newfriki(x$date,x$value,margina,times=1.5);x$frikilight<-0;if(length(bad)!=0){x$frikilight[bad]<-1} ; print(paste(Sys.time(),'Ended newfriki for suspect'),quote=FALSE)
  #  bad<-IQRoutliers(x$date,x$value,level,window);x$IQRoutliers<-0;if(length(bad)!=0){x$IQRoutliers[bad]<-1}; print(paste(Sys.time(),'Ended IQRoutliers'),quote=FALSE) # check
  #  bad<-rounding(x[,3:4],blocksizeround);x$rounding<-0;if(length(bad)!=0){x$rounding[bad]<-1}; print(paste(Sys.time(),'Ended rounding'),quote=FALSE)
  #  consolidator(home,tx[i],x)
  #  utils::write.table(x,paste(home,'QC/qc_',tx[i],sep=''),col.names=TRUE,row.names=FALSE,sep='\t',quote=FALSE); print(paste(Sys.time(),'Wrote QC results'),quote=FALSE)
  #}
  ##**************************

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