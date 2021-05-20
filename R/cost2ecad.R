cost2ecad<-function(homefolder='./'){

  #' Converter from the COST Home format into the ECA&D format (blended version)
  #' @description This function will convert station and data files in COST Home format into 
  #' corresponding station and data files in the ECA&D format (blended version) 
  #' @param homefolder path to the home directory which should contain the subdirectory 'raw_COST' with 
  #' files in the COST Home format. Files of all variables must be stored in 'raw_COST'
  #' @return station and data files in the ECA&D format stored in the subdirectory 'raw'
  #' @examples
  #' #Set a temporal working directory:
  #' wd <- tempdir(); wd0 <- setwd(wd)
  #' #Create subdirectory where raw data files in the COST format have to be located
  #' dir.create(file.path(wd, "raw_COST"))
  #' #TG: Extract the COST data and station files from the example data folder
  #' path2tglist<-system.file("extdata", "000001stations.txt", package = "INQC")
  #' tglist<-readr::read_lines_raw(path2tglist)
  #' readr::write_lines(tglist,file=paste0(wd,"/raw_COST/000001stations.txt"))
  #' path2tgdata1<-system.file("extdata", "ratmd00000001d.txt", package = "INQC")
  #' tgdata1<-readr::read_lines_raw(path2tgdata1)
  #' readr::write_lines(tgdata1, file=paste0(wd,"/raw_COST/ratmd00000001d.txt"))
  #' path2tgdata2<-system.file("extdata", "ratmd00000005d.txt", package = "INQC")
  #' tgdata2<-readr::read_lines_raw(path2tgdata2)
  #' readr::write_lines(tgdata2, file=paste0(wd,"/raw_COST/ratmd00000005d.txt"))
  #' #PP: Extract the COST data and station files from the example data folder
  #' path2pplist<-system.file("extdata", "000002stations.txt", package = "INQC")
  #' pplist<-readr::read_lines_raw(path2pplist)
  #' readr::write_lines(pplist,file=paste0(wd,"/raw_COST/000002stations.txt"))
  #' path2ppdata1<-system.file("extdata", "rappd00000001d.txt", package = "INQC")
  #' ppdata1<-readr::read_lines_raw(path2ppdata1)
  #' readr::write_lines(ppdata1, file=paste0(wd,"/raw_COST/rappd00000001d.txt"))
  #' path2ppdata2<-system.file("extdata", "rappd00000012d.txt", package = "INQC")
  #' ppdata2<-readr::read_lines_raw(path2ppdata2)
  #' readr::write_lines(ppdata2, file=paste0(wd,"/raw_COST/rappd00000012d.txt"))
  #' #Call the converter
  #' cost2ecad(homefolder = "./")
  #' #The results can be found in the directory:
  #' print(wd)
  #' #Return to user's working directory:
  #' setwd(wd0)
  #' @export
  
  #Define abbreviations for essential climate variables (ecv) in the COST and ECA&D formats
  ecv.name <- c("wind direction","wind speed","cloud cover","mean temperature","minimum temperature",
                "maximum temperature","atmospheric pressure","precipitation","sunshine duration",
                "snow depth","relative humidity","global radiation")
  ecv.COST <- c("dd","ff","nn","tm","tn","tx","pp","rr","sd","sn","rh","rg")
  ecv.ECAD <- c("dd","fg","cc","tg","tn","tx","pp","rr","ss","sd","hu","qq")
  #Define several lines in in the header of ECA&D data files
  ecv.hl15 <- c("24-28 DD   : Wind direction in degrees",    "24-28 FG   : Wind speed in 0.1 m/s",
                "24-28 CC   : Cloud cover in oktas",         "24-28 TG   : Mean temperature in 0.1 C",
                "24-28 TN   : Minimum temperature in 0.1 C", "24-28 TX   : Maximum temperature in 0.1 C",
                "24-28 PP   : Sea level pressure in 0.1 hPa","24-28 RR   : Precipitation amount in 0.1 mm",
                "24-28 SS   : Sunshine in 0.1 Hours",        "24-28 SD   : Snow depth in 1 cm",
                "24-28 HU   : Humidity in 1 %",              "24-28 QQ   : Global radiation in W/m2")
  ecv.hl16 <- c("30-34 Q_DD : quality code for DD (0='valid'; 1='suspect'; 9='missing')",
                "30-34 Q_FG : quality code for FG (0='valid'; 1='suspect'; 9='missing')",
                "30-34 Q_CC : quality code for CC (0='valid'; 1='suspect'; 9='missing')",
                "30-34 Q_TG : quality code for TG (0='valid'; 1='suspect'; 9='missing')",
                "30-34 Q_TN : quality code for TN (0='valid'; 1='suspect'; 9='missing')",
                "30-34 Q_TX : quality code for TX (0='valid'; 1='suspect'; 9='missing')",
                "30-34 Q_PP : quality code for PP (0='valid'; 1='suspect'; 9='missing')",
                "30-34 Q_RR : quality code for RR (0='valid'; 1='suspect'; 9='missing')",
                "30-34 Q_SS : quality code for SS (0='valid'; 1='suspect'; 9='missing')",
                "30-34 Q_SD : quality code for SD (0='valid'; 1='suspect'; 9='missing')",
                "30-34 Q_HU : quality code for HU (0='valid'; 1='suspect'; 9='missing')",
                "30-34 Q_QQ : quality code for QQ (0='valid'; 1='suspect'; 9='missing')")
  ecv.hl22 <- c(" STAID, SOUID,    DATE,   DD, Q_DD"," STAID, SOUID,    DATE,   FG, Q_FG",
                " STAID, SOUID,    DATE,   CC, Q_CC"," STAID, SOUID,    DATE,   TG, Q_TG",
                " STAID, SOUID,    DATE,   TN, Q_TN"," STAID, SOUID,    DATE,   TX, Q_TX",
                " STAID, SOUID,    DATE,   PP, Q_PP"," STAID, SOUID,    DATE,   RR, Q_RR",
                " STAID, SOUID,    DATE,   SS, Q_SS"," STAID, SOUID,    DATE,   SD, Q_SD",
                " STAID, SOUID,    DATE,   HU, Q_HU"," STAID, SOUID,    DATE,   QQ, Q_QQ")
  ecv <- data.frame(name=ecv.name,COST=ecv.COST,ECAD=ecv.ECAD,hl15=ecv.hl15,hl16=ecv.hl16,hl22=ecv.hl22)

  #Define a list of station files (all variables must be located in the same folder 'raw_COST')
  lst.stn.fls <- list.files(paste0(homefolder,'raw_COST'), pattern = '*stations*')
  #Define a number of station files (climate variables)
  n.stn.fls <- length(lst.stn.fls)
  #Create (if it does not exist) the 'raw' directory (where ECA&D files should be stored)
  if(!dir.exists(paste0(homefolder,'raw'))){dir.create(paste0(homefolder,'raw'))}
  #Convert the station files from the COST HOME to ECA&D format
  for (i in 1:n.stn.fls) { #Cycle over all station files (or over all climate variables)
    #Read a station file for a current climate variable
    stn.fl.i <- utils::read.table(paste0(homefolder,'raw_COST/',lst.stn.fls[i]),header = FALSE, sep = "\t", dec = ".")
    #Define a number of stations (station files) for a current climate variable
    n.stns <- length(stn.fl.i[,1])
    #Define variables/vectors for a station list in the ECA&D format (blended)
    
    #Assume that STAID is last 4 digits in a station code (these digits should be unique for each station)
    STAID <- as.numeric(substring(stn.fl.i[,1],10,13)) 
    SOUID <- paste0(substring(lst.stn.fls[i],5,6),substring(stn.fl.i[,1],10,13))
    SOUNAME <- stn.fl.i[,10]
    CN <- stn.fl.i[,9]
    #Comment. In the LAT and LON calculations (presented below) there is one specific case when results are not correct
    #This happens when e.g. LAT=-00:35:19
    LAT1 <- sprintf("%03d",stn.fl.i[,2]) #Initial values of latitudes (assuming they all are negative)
    LAT1[which(stn.fl.i[,2]>=0)] <- paste0('+',sprintf("%02d",stn.fl.i[which(stn.fl.i[,2]>=0),2])) #If latitudes are positive
    LAT2 <- sprintf("%02d",stn.fl.i[,3])
    LAT3 <- sprintf("%02d",stn.fl.i[,4])
    LON1 <- sprintf("%04d",stn.fl.i[,5]) #Initial values of longitudes (assuming they all are negative)
    LON1[which(stn.fl.i[,5]>=0)] <- paste0('+',sprintf("%03d",stn.fl.i[which(stn.fl.i[,5]>=0),5])) #If longitudes are positive
    LON2 <- sprintf("%02d",stn.fl.i[,6])
    LON3 <- sprintf("%02d",stn.fl.i[,7])
    LAT <- paste0(LAT1,':',LAT2,':',LAT3)
    LON <- paste0(LON1,':',LON2,':',LON3)
    HGHT <- as.integer(stn.fl.i[,8])
    #Define a position of the climate variable in the list
    i.ecv <- which(ecv[,2]==substring(stn.fl.i[1,1],3,4))
    for (j in 1:n.stns) { #Cycle over all data files
      #Read a data file
      dat.j <- utils::read.table(paste0(homefolder,'raw_COST/',stn.fl.i[j,1]),header = FALSE, sep = "\t", dec = ".")
      #Define a number of days in the data file
      n.days <- length(dat.j[,1])
      #Define DATE vector (in the ECA&D format)
      month <- sprintf("%02d",dat.j[,2])
      day <- sprintf("%02d",dat.j[,3])
      DATE <- paste0(as.character(dat.j[,1]),month,day)
      #Define VALUE vector and transform it to ECA&D units
      VALUE <- dat.j[,4]
      if(i.ecv==2|i.ecv==4|i.ecv==5|i.ecv==6|i.ecv==7|i.ecv==8|i.ecv==9) {
        VALUE <- VALUE*10
      }
      #Define QC flags for corresponding data values
      QC <- vector(mode='integer',length=n.days)
      QC[1:n.days] <- 0
      QC[which(VALUE<=-999)] <- 9 
      
      #Write the header to an ECA&D data file
      file.nm  <- paste0(toupper(ecv[i.ecv,3]),'_STAID',sprintf("%06d",STAID[j]),'.txt')
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
                   ecv[i.ecv,4],
                   ecv[i.ecv,5],
                   "",
                   paste0("This is the blended series of station ",SOUNAME[j],", ",CN[j]," (STAID: ",STAID[j],")"),
                   paste0("Blended and updated with sources: ",SOUID[j]),
                   "See files sources.txt and stations.txt for more info.",
                   "",
                   ecv[i.ecv,6]),
                 file.out)
      close(file.out)
      #Define a data frame to be written to a file
      df.j <- data.frame('STAID'=format(STAID[j],width=6), 'SOUID'=SOUID[j], 'DATE'=DATE, 
                         'VALUE'=format(VALUE,width=5), 'QC'=format(QC,width=5))
      utils::write.table(df.j, file=paste0(homefolder,'raw/',file.nm),row.names = FALSE,col.names = FALSE,sep = ",",
                         append=TRUE,quote = FALSE)
    }
    
    #Define a data frame with meta data in the ECA&D format
    stn.fl.ECAD.i <- data.frame(format(STAID,width=5),format(SOUNAME,width=40),CN,
                                LAT,LON,format(HGHT,width=4))
    if (i==1) {
      stn.fl.ECAD <- stn.fl.ECAD.i
    } else {
      stn.fl.ECAD <- rbind(stn.fl.ECAD, stn.fl.ECAD.i)
    }
  }
  names(stn.fl.ECAD) <- c('STAID','STANAME                                 ','CN','      LAT','       LON','HGHT')
  #Write a station file in the ECA&D format (blended version)
  utils::write.table(stn.fl.ECAD,file=paste0(homefolder,'raw/','stations.txt'),
                     col.names=TRUE,row.names=FALSE,sep=',',quote = FALSE)
}
