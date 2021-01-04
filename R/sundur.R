sundur<-function(element='SS',maxseq=3,blocksizeround=20,blockmanymonth=15,blockmanyyear=180,roundmax=10,inisia=FALSE){

  #' QC for Sunshine Duration (SS)
  #' @description This function will centralize temperature-like qc routines. Will create a file in the folder QC
  #' with an additional 0/1 column, where "1" means test failed
  # @param home path to the home directory
  #' @param element two-letters ECA&D code for the element (SS for sunshine duration)
  #' @param maxseq maximum number of consecutive repeated values, for flat function (11.1,11.1,11.1 would be 3 consecutives). Passed on to flat(). See ?flat for details
  #' @param blocksizeround maximum number of values in a month with the same decimal, FUNCTION: rounding()
  #' @param blockmanymonth maximum number of equal values in a month, FUNCTION: toomany()
  #' @param blockmanyyear maximum number of equal values in a year, FUCNTION: toomany()
  #' @param roundmax maximum number of consecutive decimal part value, for flat() function (10.0, 11.0, 12.0 would be 3 consecutive)
  #' @param inisia logical flag. If it is TRUE inithome() will be called
  #' @return results of QC for SS
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
  #  bad<-duplas(x$date);x$dupli<-0;if(length(bad)!=0){x$dupli[bad]<-1} ; print(paste(Sys.time(),'Ended duplas'),quote=FALSE)
  #  bad<-weirddate(x[,3:4]);x$weirddate<-0;if(length(bad)!=0){x$weirddate[bad]<-1}; print(paste(Sys.time(),'Ended weirddate'),quote=FALSE)
  #  bad<-toomany(x[,3:4],blockmanymonth,1,exclude=0);x$toomanymonth<-0;if(length(bad)!=0){x$toomanymonth[bad]<-1}; print(paste(Sys.time(),'Ended toomany, month'),quote=FALSE)
  #  bad<-toomany(x[,3:4],blockmanyyear,2,exclude=0);x$toomanyyear<-0;if(length(bad)!=0){x$toomanyyear[bad]<-1}; print(paste(Sys.time(),'Ended toomany, year'),quote=FALSE)
  #  bad<-flat(x$value,maxseq);x$flat<-0;if(length(bad)!=0){x$flat[bad]<-1}; print(paste(Sys.time(),'Ended flat for values'),quote=FALSE)
  #  bad<-flatsun(x[,3:4],maxseq,tx[i],home);x$flatsun<-0;if(length(bad)!=0){x$flatsun[bad]<-1}; print(paste(Sys.time(),'Ended flatsun'),quote=FALSE)
  #  bad<-flat(x$value%%10,roundmax);x$roundmax<-0;if(length(bad)!=0){x$roundmax[bad]<-1}; print(paste(Sys.time(),'Ended flat for decimal part'),quote=FALSE) #this looks at consecutive decimal parts
  #  bad<-roundprecip(x[,3:4],blocksizeround,exclude=0);x$rounding<-0;if(length(bad)!=0){x$rounding[bad]<-1}; print(paste(Sys.time(),'Ended rounding'),quote=FALSE)
  #  bad<-physics(x$value,240,1);x$large<-0;if(length(bad)!=0){x$large[bad]<-1}; print(paste(Sys.time(),'Ended physics, large'),quote=FALSE)
  #  bad<-physics(x$value,0,3);x$small<-0;if(length(bad)!=0){x$small[bad]<-1}; print(paste(Sys.time(),'Ended physics, small'),quote=FALSE)
  #  bad<-sunafterdark(x[3:4],substring(tx[i],9,14),home);x$maxsun<-0;if(length(bad)!=0){x$maxsun[bad]<-1}; print(paste(Sys.time(),'Ended sunafterdark'),quote=FALSE)
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
  for(i in 1:ene){
    name<-paste(homefolder,'raw/',tx[i],sep='')
    print(paste(name,i,'of',ene),quote=FALSE)
    x<-readecad(input=name) ; print(paste(Sys.time(),'Ended readecad'),quote=FALSE)
    if(!'STAID' %in% colnames(x) & ncol(x)==4){x<-cbind(STAID=as.numeric(substring(tx[i],9,14)),x)}
    x<-x[,1:4];colnames(x)<-c('STAID','SOUID','date','value')
    bad<-duplas(x$date);x$dupli<-0;if(length(bad)!=0){x$dupli[bad]<-1} ; print(paste(Sys.time(),'Ended duplas'),quote=FALSE)
    bad<-weirddate(x[,3:4]);x$weirddate<-0;if(length(bad)!=0){x$weirddate[bad]<-1}; print(paste(Sys.time(),'Ended weirddate'),quote=FALSE)
    bad<-toomany(x[,3:4],blockmanymonth,1,exclude=0);x$toomanymonth<-0;if(length(bad)!=0){x$toomanymonth[bad]<-1}; print(paste(Sys.time(),'Ended toomany, month'),quote=FALSE)
    bad<-toomany(x[,3:4],blockmanyyear,2,exclude=0);x$toomanyyear<-0;if(length(bad)!=0){x$toomanyyear[bad]<-1}; print(paste(Sys.time(),'Ended toomany, year'),quote=FALSE)
    bad<-flat(x$value,maxseq);x$flat<-0;if(length(bad)!=0){x$flat[bad]<-1}; print(paste(Sys.time(),'Ended flat for values'),quote=FALSE)
    bad<-flatsun(x[,3:4],maxseq,tx[i]);x$flatsun<-0;if(length(bad)!=0){x$flatsun[bad]<-1}; print(paste(Sys.time(),'Ended flatsun'),quote=FALSE)
    bad<-flat(x$value%%10,roundmax);x$roundmax<-0;if(length(bad)!=0){x$roundmax[bad]<-1}; print(paste(Sys.time(),'Ended flat for decimal part'),quote=FALSE) #this looks at consecutive decimal parts
    bad<-roundprecip(x[,3:4],blocksizeround,exclude=0);x$rounding<-0;if(length(bad)!=0){x$rounding[bad]<-1}; print(paste(Sys.time(),'Ended rounding'),quote=FALSE)
    bad<-physics(x$value,240,1);x$large<-0;if(length(bad)!=0){x$large[bad]<-1}; print(paste(Sys.time(),'Ended physics, large'),quote=FALSE)
    bad<-physics(x$value,0,3);x$small<-0;if(length(bad)!=0){x$small[bad]<-1}; print(paste(Sys.time(),'Ended physics, small'),quote=FALSE)
    bad<-sunafterdark(x[3:4],substring(tx[i],9,14));x$maxsun<-0;if(length(bad)!=0){x$maxsun[bad]<-1}; print(paste(Sys.time(),'Ended sunafterdark'),quote=FALSE)
    consolidator(tx[i],x)
  }
}