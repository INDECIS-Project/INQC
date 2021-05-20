climdex2ecad<-function(homefolder='./',stationlist='stations.csv',countrycode='DE'){

  #' Converter from the ClimDex format into the ECA&D format (blended version)
  #' @description This function will convert station and data files in ClimDex format into 
  #' corresponding station and data files in the ECA&D format (blended version) 
  #' @param homefolder path to the home directory which should contain the subdirectory 'raw_ClimDex' with 
  #' files in the ClimDex format
  #' @param stationlist list (as 'csv'-file) of climatological stations to be considered. Each line should be in the format:
  #' lat (as dec. degree), lon (as dec. degree), height, staname
  #' @param countrycode two character country code
  #' @return station and data files in the ECA&D format stored in the subdirectory 'raw'
  #' @examples
  #' #Set a temporal working directory:
  #' wd <- tempdir(); wd0 <- setwd(wd)
  #' #Create subdirectory where raw data files in the ClimDex format have to be located
  #' dir.create(file.path(wd, "raw_ClimDex"))
  #' #Extract the ClimDex data and station files from the example data folder
  #' path2stalist<-system.file("extdata", "stations.csv", package = "INQC")
  #' stalist<-readr::read_lines_raw(path2stalist)
  #' readr::write_lines(stalist,file=paste0(wd,"/raw_ClimDex/stations.csv"))
  #' path2data1<-system.file("extdata", "Deuselbach.txt", package = "INQC")
  #' data1<-readr::read_lines_raw(path2data1)
  #' readr::write_lines(data1, file=paste0(wd,"/raw_ClimDex/Deuselbach.txt"))
  #' path2data2<-system.file("extdata", "Staname.txt", package = "INQC")
  #' data2<-readr::read_lines_raw(path2data2)
  #' readr::write_lines(data2, file=paste0(wd,"/raw_ClimDex/Staname.txt"))
  #' #Call the converter
  #' climdex2ecad(homefolder = "./",stationlist = "stations.csv",countrycode = "DE")
  #' #The results can be found in the directory:
  #' print(wd)
  #' #Return to user's working directory:
  #' setwd(wd0)
  #' @export
  
  #Define several lines with specific information for climate variables in in the header of ECA&D data files
  ecv.ECAD <- c("rr","tx","tn")
  ecv.hl15 <- c("24-28 RR   : Precipitation amount in 0.1 mm", "24-28 TX   : Maximum temperature in 0.1 C",
                "24-28 TN   : Minimum temperature in 0.1 C")
  ecv.hl16 <- c("30-34 Q_RR : quality code for RR (0='valid'; 1='suspect'; 9='missing')",
                "30-34 Q_TX : quality code for TX (0='valid'; 1='suspect'; 9='missing')",
                "30-34 Q_TN : quality code for TN (0='valid'; 1='suspect'; 9='missing')")
  ecv.hl22 <- c(" STAID, SOUID,    DATE,   RR, Q_RR"," STAID, SOUID,    DATE,   TX, Q_TX",
                " STAID, SOUID,    DATE,   TN, Q_TN")
  ecv <- data.frame(ECAD=ecv.ECAD,hl15=ecv.hl15,hl16=ecv.hl16,hl22=ecv.hl22)

  #Read a list of stations (the station list and data files must be located in the same folder 'raw_ClimDex')
  lst.stn <- utils::read.table(paste0(homefolder,'raw_ClimDex/',stationlist),header = FALSE, sep = ",",na.strings='-99.9')
  #Define a number of stations
  n.stn <- nrow(lst.stn)
  #Create (if it does not exist) the 'raw' directory (where ECA&D files should be stored)
  if(!dir.exists(paste0(homefolder,'raw'))){dir.create(paste0(homefolder,'raw'))}
  #Convert the station and data files from the ClimDex to ECA&D format
  for (i in 1:n.stn) { #Cycle over all stations
    #Define a name of a data file
    filename<-paste0(lst.stn[i,4],'.txt')
    print(filename)
    #Define LAT and LON of a station (in the ECA&D format)
    lat.angel <- lst.stn[i,1]
    if(lat.angel < 0){nyu<-TRUE;lat.angel = lat.angel*(-1)}else{nyu=FALSE}
    lat.angel.chr <- as.character(lat.angel)
    lat.deg <- as.numeric(strsplit(lat.angel.chr, "\\.")[[1]][1])
    lat.min <- as.numeric(strsplit(as.character((lat.angel-lat.deg)*60), "\\.")[[1]][1])
    lat.sec <- round((((lat.angel-lat.deg)*60) - lat.min) * 60)
    if (nyu) {lat.deg <- paste0("-",sprintf("%02d",lat.deg))} else {lat.deg <- paste0("+",sprintf("%02d",lat.deg))}
    lat.min <- sprintf("%02d",lat.min)
    lat.sec <- sprintf("%02d",lat.sec)
    LAT <- paste0(lat.deg,':',lat.min,':',lat.sec)
    lon.angel <- lst.stn[i,2]
    if(lon.angel < 0){nyu<-TRUE;lon.angel = lon.angel*(-1)}else{nyu=FALSE}
    lon.angel.chr <- as.character(lon.angel)
    lon.deg <- as.numeric(strsplit(lon.angel.chr, "\\.")[[1]][1])
    lon.min <- as.numeric(strsplit(as.character((lon.angel-lon.deg)*60), "\\.")[[1]][1])
    lon.sec <- round((((lon.angel-lon.deg)*60) - lon.min) * 60)
    if (nyu) {lon.deg <- paste0("-",sprintf("%03d",lon.deg))} else {lon.deg <- paste0("+",sprintf("%03d",lon.deg))}
    lon.min <- sprintf("%02d",lon.min)
    lon.sec <- sprintf("%02d",lon.sec)
    LON <- paste0(lon.deg,':',lon.min,':',lon.sec)
    #Read a data file (if it exists)
    if(file.exists(paste0(homefolder,'raw_ClimDex/',filename))){
      y<-utils::read.table(paste0(homefolder,'raw_ClimDex/',filename),na.strings='-99.9') #OS. Read a RClimDex file
      y[,4]<-round(y[,4],1) # RR data
      y[,5]<-round(y[,5],1) # TX data
      y[,6]<-round(y[,6],1) # TN data
      date<-as.numeric(paste0(y[,1],sprintf("%02d",y[,2]),sprintf("%02d",y[,3])))
      #---------------------------------------------------------
      # Write a data files for RR, TX and TN in the ECA&D format
      #---------------------------------------------------------
      STAID <- i
      for (j in 1:3) {
        SOUID <- paste0(j,sprintf("%05d",i))
        #Write the header to an ECA&D data file
        file.nm  <- paste0(toupper(ecv[j,1]),'_STAID',sprintf("%06d",STAID),'.txt')
        file.out <- file(paste0(homefolder,'raw/',file.nm))
        writeLines(c("This is INQC's formal conversion of a COST Home data file to the ECA&D format (STAID and SOUID are not real!)",
                     paste("EUROPEAN CLIMATE ASSESSMENT & DATASET (ECA&D), file created on:",Sys.Date(),sep="  "),
                     "THESE DATA CAN BE USED FOR NON-COMMERCIAL RESEARCH AND EDUCATION PROVIDED THAT THE FOLLOWING SOURCE IS ACKNOWLEDGED: ",
                     "",
                     "Klein Tank, A.M.G. and Coauthors, 2002. Daily dataset of 20th-century surface",
                     "air temperature and precipitation series for the European Climate Assessment.",
                     "Int. J. of Climatol., 22, 1441-1453.",
                     "Data and metadata available at http://www.ecad.eu",
                     "",
                     "FILE FORMAT (MISSING VALUE CODE = -9999):",
                     "",
                     "01-06 STAID: Station identifier",
                     "08-13 SOUID: Source identifier",
                     "15-22 DATE : Date YYYYMMDD",
                     ecv[j,2],
                     ecv[j,3],
                     "",
                     paste0("This is the blended series of station ",lst.stn[i,4],", ",countrycode," (STAID: ",STAID,")"),
                     paste0("Blended and updated with sources: ",SOUID),
                     "See files sources.txt and stations.txt for more info.",
                     "",
                     ecv[j,4]),
                   file.out)
        close(file.out)
        #Define vectors of data values and corresponding quality flags
        VALUE <- as.numeric(y[,j+3])*10
        QC <- vector(mode='integer',length=length(VALUE))
        QC[1:length(VALUE)] <- 0
        QC[which(is.na(VALUE))] <- 9 
        VALUE[which(is.na(VALUE))] <- -9999
        #Define a data frame to be written to a file
        df.j <- data.frame('STAID'=format(STAID,width=6), 'SOUID'=SOUID, 'DATE'=date, 
                           'VALUE'=format(VALUE,width=5), 'QC'=format(QC,width=5))
        utils::write.table(df.j, file=paste0(homefolder,'raw/',file.nm),row.names = FALSE,col.names = FALSE,
                           sep = ",",append=TRUE,quote = FALSE)
      }
      #Define a data frame with meta data in the ECA&D format
      stn.fl.ECAD.i <- data.frame(format(STAID,width=5),format(lst.stn[i,4],width=40),countrycode,
                                  LAT,LON,format(lst.stn[i,3],width=4))
      if (i==1) {
        stn.fl.ECAD <- stn.fl.ECAD.i
      } else {
        stn.fl.ECAD <- rbind(stn.fl.ECAD, stn.fl.ECAD.i)
      }
    }
  }
  names(stn.fl.ECAD) <- c('STAID','STANAME                                 ','CN','      LAT','       LON','HGHT')
  #Write a station file in the ECA&D format (blended version)
  utils::write.table(stn.fl.ECAD,file=paste0(homefolder,'raw/','stations.txt'),
                     col.names=TRUE,row.names=FALSE,sep=',',quote = FALSE)
}
