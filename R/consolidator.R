consolidator<-function(filename,x){

  #' Consolidates QC files
  #' @description This function is not intended to be called as a stand-alone function. It is automatically called each time a file ends its QC.
  #' It will write the quality control files. One file will be placed in a subfolder of the homefolder named QCConslidated. It will use the exact ECA&D
  #' format (date, value, QC flag). The QC flags include:
  #'
  #' 0: Passed QC;
  #' 1: ERROR;
  #' 2: ALMOST CERTAIN, ERROR;
  #' 3: OUTLIER, SUSPECT;
  #' 4: COLLECTIVELY SUSPECT;
  #' 9: Missing value.
  #'
  #' A second file is placed in the subfolder QC and includes all date, value and a column for each QC test ran over this file. Values passing/not passing QC
  #' are labelled with 0/1. A third file summarizes the number of values falling on each category (0,1,2,3,4,9) and the number of values failing each test
  #'
  # @param home homefolder of the ECA&D file name. Defaulted to ./data
  #' @param filename ECA&D file name, expressed as VV_SOUIDXXXXXX.txt, where "VV" is the two-letters variable code, "SOUID" is literal,
  #' XXXXXX is the ECA&D SOUID code and ".txt" is literal
  #' @param x QCed series, formatted as date, value, QC flag
  #' @return It does not return any value. Each time when called, it will create three files:
  #' Summary file,  placed at ./QCSumamry/SummaryVV_SOUIDXXXXXX.txt;
  #' QC consolidated file, placed at ./QCConsolidated/VV_SOUIDXXXXXX.txt;
  #' Verbose QC file, placed at ./QC/qc_VV_SOUIDXXXXXX.txt.
  #' @export

  #Get value of 'Global variables' 'homefolder' and 'blend'
  homefolder <- getOption("homefolder")
  blend <- getOption("blend")
  utils::write.table(x,paste0(homefolder,'QC/qc_',filename),col.names=TRUE,row.names=FALSE,sep='\t',quote=FALSE)
  element<-substring(filename,1,2)
  stationsummary<-apply(x[,5:ncol(x)],2,sum)
  failed<-as.data.frame(stationsummary[which(stationsummary!=0)])
  utils::write.table(failed,paste0(homefolder,'/QCSummary/Summary',filename),quote=FALSE,sep='\t',col.names=FALSE)
  x$consolidated = 0
  prov<-which(is.na(x$value))
  ## not available (9)
  x$consolidated[prov]<-9
  ## collectively suspect: 
  x$consolidated[which(x$toomanymonth==1)]<-4
  x$consolidated[which(x$toomanyyear==1)]<-4
  x$consolidated[which(x$rounding==1)]<-4
  x$consolidated[which(x$roundmax==1)]<-4
  x$consolidated[which(x$repeatedvalue==1)]<-4
  x$consolidated[which(x$drywetlong==1)]<-4
  x$consolidated[which(x$suspectacumprec==1)]<-4
  x$consolidated[which(x$flat==1)]<-4
  x$consolidated[which(x$flatsun==1)]<-4
  ## outliers (3)
  x$consolidated[which(x$paretogadget==1)]<-3
  x$consolidated[which(x$IQRoutliers==1)]<-3
  x$consolidated[which(x$frikilight==1)]<-3
  ## almost certain, error (2)
  x$consolidated[which(x$jumpQUANT==1)]<-2
  x$consolidated[which(x$friki==1)]<-2
  ## error, with no doubt (1)
  x$consolidated[which(x$jumpABS==1)]<-1
  x$consolidated[which(x$weirddate==1)]<-1
  x$consolidated[which(x$dupli==1)]<-1
  x$consolidated[which(x$large==1)]<-1
  x$consolidated[which(x$small==1)]<-1
  x$consolidated[which(x$txtn==1)]<-1
  x$consolidated[which(x$maxsun==1)]<-1
  resumen<-as.data.frame(table(x$consolidated));names(resumen)<-c('QC_Code','Freq')
  utils::write.table(resumen,paste0(homefolder,'/QCSummary/Summary',filename),quote=FALSE,sep='\t',append=TRUE,col.names=TRUE,row.names=FALSE)
  ## Consolidated file
  header<-readheader(paste0(homefolder,'raw/',filename))
  #OS. I commented 2 lines below. The ECA&D blended data now have the same format as non-blended
  #if(blend){anchos<-c(6,8,5,5)}else{anchos<-c(6,6,8,5,5)}
  #if(blend){x<-x[,-(which(names(x)=='STAID'))]} ### This avoids writing the STAID column in blended files
  anchos<-c(6,6,8,5,5)
  grannyu<-ncol(x)
  x<-x[,c(1:4,grannyu)]
  utils::write.table('Quality control with INQC V2.0.4, INDECIS Project, by Dr. Enric Aguilar, C3/URV',paste0(homefolder,'QCConsolidated/',filename),quote=FALSE,col.names=FALSE,row.names=FALSE,sep=',',na='')
  utils::write.table(header,paste0(homefolder,'QCConsolidated/',filename),quote=FALSE,col.names=FALSE,row.names=FALSE,sep=',',na='',append=TRUE)
  gdata::write.fwf(x,paste0(homefolder,'QCConsolidated/',filename),colnames=FALSE,rownames=FALSE,sep=',',quote=FALSE,append=TRUE,na='-9999',width=anchos)
  print(paste(Sys.time(),'Wrote QC files'),quote=FALSE)
}
