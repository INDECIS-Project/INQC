downloadator<-function(homefolder='../ecad_updated',
                       #********************
                       #tx="https://www.ecad.eu//utils/downloadfile.php?file=download/ECA_nonblend_tx.zip",
                       #tx2="https://www.ecad.eu//download/ECA_blend_source_tx.txt",
                       #tn="https://www.ecad.eu//utils/downloadfile.php?file=download/ECA_nonblend_tn.zip",
                       #tn2="https://www.ecad.eu//download/ECA_blend_source_tn.txt",
                       #tg="https://www.ecad.eu//utils/downloadfile.php?file=download/ECA_nonblend_tg.zip",
                       #tg2="https://www.ecad.eu//download/ECA_blend_source_tg.txt",
                       #sd="https://www.ecad.eu//utils/downloadfile.php?file=download/ECA_nonblend_sd.zip",
                       #sd2="https://www.ecad.eu//download/ECA_blend_source_sd.txt",
                       #ss="https://www.ecad.eu//utils/downloadfile.php?file=download/ECA_nonblend_ss.zip",
                       #ss2="https://www.ecad.eu//download/ECA_blend_source_ss.txt",
                       #rr="https://www.ecad.eu//utils/downloadfile.php?file=download/ECA_nonblend_rr.zip",
                       #rr2="https://www.ecad.eu//download/ECA_blend_source_rr.txt",
                       #pp="https://www.ecad.eu//utils/downloadfile.php?file=download/ECA_nonblend_pp.zip",
                       #pp2="https://www.ecad.eu//download/ECA_blend_source_pp.txt",
                       #cc="https://www.ecad.eu//utils/downloadfile.php?file=download/ECA_nonblend_cc.zip",
                       #cc2="https://www.ecad.eu//download/ECA_blend_source_cc.txt",
                       #hu="https://www.ecad.eu//utils/downloadfile.php?file=download/ECA_nonblend_hu.zip",
                       #hu2="https://www.ecad.eu//download/ECA_blend_source_hu.txt",
                       #fg="https://www.ecad.eu//utils/downloadfile.php?file=download/ECA_nonblend_fg.zip",
                       #fg2="https://www.ecad.eu//download/ECA_blend_source_fg.txt"){
                       #********************
                       #In order to avoid an error/note message, below 'http' is used instead of 'https'
                       tx="http://knmi-ecad-assets-prd.s3.amazonaws.com/download/ECA_nonblend_tx.zip",
                       tx2="http://knmi-ecad-assets-prd.s3.amazonaws.com/download/ECA_blend_source_tx.txt",
                       tn="http://knmi-ecad-assets-prd.s3.amazonaws.com/download/ECA_nonblend_tn.zip",
                       tn2="http://knmi-ecad-assets-prd.s3.amazonaws.com/download/ECA_blend_source_tn.txt",
                       tg="http://knmi-ecad-assets-prd.s3.amazonaws.com/download/ECA_nonblend_tg.zip",
                       tg2="http://knmi-ecad-assets-prd.s3.amazonaws.com/download/ECA_blend_source_tg.txt",
                       sd="http://knmi-ecad-assets-prd.s3.amazonaws.com/download/ECA_nonblend_sd.zip",
                       sd2="http://knmi-ecad-assets-prd.s3.amazonaws.com/download/ECA_blend_source_sd.txt",
                       ss="http://knmi-ecad-assets-prd.s3.amazonaws.com/download/ECA_nonblend_ss.zip",
                       ss2="http://knmi-ecad-assets-prd.s3.amazonaws.com/download/ECA_blend_source_ss.txt",
                       rr="http://knmi-ecad-assets-prd.s3.amazonaws.com/download/ECA_nonblend_rr.zip",
                       rr2="http://knmi-ecad-assets-prd.s3.amazonaws.com/download/ECA_blend_source_rr.txt",
                       pp="http://knmi-ecad-assets-prd.s3.amazonaws.com/download/ECA_nonblend_pp.zip",
                       pp2="http://knmi-ecad-assets-prd.s3.amazonaws.com/download/ECA_blend_source_rr.txt",
                       cc="http://knmi-ecad-assets-prd.s3.amazonaws.com/download/ECA_nonblend_cc.zip",
                       cc2="http://knmi-ecad-assets-prd.s3.amazonaws.com/download/ECA_blend_source_cc.txt",
                       hu="http://knmi-ecad-assets-prd.s3.amazonaws.com/download/ECA_nonblend_hu.zip",
                       hu2="http://knmi-ecad-assets-prd.s3.amazonaws.com/download/ECA_blend_source_hu.txt",
                       fg="http://knmi-ecad-assets-prd.s3.amazonaws.com/download/ECA_nonblend_fg.zip",
                       fg2="http://knmi-ecad-assets-prd.s3.amazonaws.com/download/ECA_blend_source_fg.txt"){
                       #********************
                       #tx="http://knmi-ecad-assets-prd.s3.amazonaws.com/download/ECA_nonblend_tx.zip",
                       #tx2="http://knmi-ecad-assets-prd.s3.amazonaws.com/download/ECA_blend_source_tx.txt",
                       #tn=NULL,tn2=NULL,tg=NULL,tg2=NULL,sd=NULL,sd2=NULL,ss=NULL,ss2=NULL,rr=NULL,rr2=NULL,
                       #pp=NULL,pp2=NULL,cc=NULL,cc2=NULL,hu=NULL,hu2=NULL,fg=NULL,fg2=NULL){
                       #********************
                       #tx,tx2,tn,tn2,tg,tg2,sd,sd2,ss,ss2,rr,rr2,pp,pp2,cc,cc2,hu,hu2,fg,fg2){

  #' Downloads the latest version of blended data from the ECA&D website
  #' @description This function will use the default or specified links to download one or several files from ECA&D and place them for their use
  #' with INQC. For each variable a data file and a station file will/should be specified.
  ## When a parameter is not specified or the link does not exist, the function will skip this variable.
  #' @param homefolder full path to local folder in the form './homefolder'. The function will store there the station files and create ./homefolder/raw
  #' and will store there the data
  #' @param tx  link to download daily maximum temperature or NULL. Default set to working ECA&D link, as of 22/12/2020. Provided link MUST exist.
  #' @param tx2 link to download daily maximum temperatures station list  or NULL. Default set to working ECA&D link, as of 22/12/2020. Provided link MUST exist.
  #' @param tn  link to download daily minimum temperature or NULL. Default set to working ECA&D link, as of 22/12/2020. Provided link MUST exist.
  #' @param tn2 link to download daily minimum temperature station list or NULL. Default set to working ECA&D link, as of 22/12/2020. Provided link MUST exist.
  #' @param tg  link to download daily average temperature or NULL. Default set to working ECA&D link, as of 22/12/2020. Provided link MUST exist.
  #' @param tg2 link to download daily average temperature station list or NULL. Default set to working ECA&D link, as of 22/12/2020. Provided link MUST exist.
  #' @param sd  link to download daily snow depth or NULL. Default set to working ECA&D link, as of 22/12/2020. Provided link MUST exist.
  #' @param sd2 link to download daily snow depth station list or NULL. Default set to working ECA&D link, as of 22/12/2020. Provided link MUST exist.
  #' @param ss  link to download daily sunshine duration or NULL. Default set to working ECA&D link, as of 22/12/2020. Provided link MUST exist.
  #' @param ss2 link to download daily sunshine duration station list or NULL. Default set to working ECA&D link, as of 22/12/2020. Provided link MUST exist.
  #' @param rr  link to download daily rainfall or NULL. Default set to working ECA&D link, as of 22/12/2020. Provided link MUST exist.
  #' @param rr2 link to download daily rainfall station list or NULL. Default set to working ECA&D link, as of 22/12/2020. Provided link MUST exist.
  #' @param pp  link to download daily sea level pressure or NULL. Default set to working ECA&D link, as of 22/12/2020. Provided link MUST exist.
  #' @param pp2 link to download daily sea level pressure station list or NULL. Default set to working ECA&D link, as of 22/12/2020. Provided link MUST exist.
  #' @param cc  link to download daily cloud coverage or NULL. Default set to working ECA&D link, as of 22/12/2020. Provided link MUST exist.
  #' @param cc2 link to download daily cloud coverage station list or NULL. Default set to working ECA&D link, as of 22/12/2020. Provided link MUST exist.
  #' @param hu  link to download daily relative humidity or NULL. Default set to working ECA&D link, as of 22/12/2020. Provided link MUST exist.
  #' @param hu2 link to download daily relative humidity station list or NULL. Default set to working ECA&D link, as of 22/12/2020. Provided link MUST exist.
  #' @param fg  link to download daily wind speed or NULL. Default set to working ECA&D link, as of 22/12/2020. Provided link MUST exist.
  #' @param fg2 link to download daily wind speed station list or NULL. Default set to working ECA&D link, as of 22/12/2020. Provided link MUST exist.
  #' @return For each valid link, the corresponding file will be downloaded. Data files will be unzipped to the ./raw folder (as requested by INQC)
  #' and station files will be stored at the specified homefolder
  #' @examples
  #' \dontrun{
  #' #Set a temporal working directory:
  #' wd <- tempdir()
  #' wd0 <- setwd(wd)
  #' #Please note, the command below might take a while and will download the ECA&D data
  #' #with a size more than 0.5GB
  #' downloadator('./data',
  #'              tx=NULL,
  #'              tx2=NULL,
  #'              tn=NULL,
  #'              tn2=NULL,
  #'              tg=NULL,
  #'              tg2=NULL,
  #'              sd=NULL,
  #'              sd2=NULL,
  #'              ss='http://knmi-ecad-assets-prd.s3.amazonaws.com/download/ECA_nonblend_ss.zip',
  #'              ss2="http://knmi-ecad-assets-prd.s3.amazonaws.com/download/ECA_blend_source_ss.txt",
  #'              rr=NULL,
  #'              rr2=NULL,
  #'              pp=NULL,
  #'              pp2=NULL,
  #'              cc=NULL,
  #'              cc2=NULL,
  #'              hu=NULL,
  #'              hu2=NULL,
  #'              fg=NULL,
  #'              fg2=NULL)
  #' #Delete the downloaded archive (the zip-file)
  #' file.remove(paste(wd,"/data/raw/","ss.zip",sep=""))
  #' #Return to user's working directory:
  #' setwd(wd0)
  #' #The downloaded files can be found in directory:
  #' print(wd)
  #' }
  #' @export

  ## 1) a set of variables (tx,tn,tg,sd,ss,rr,cc,hu,fg) which represent the variables analyzed at the INDECIS project.They can be either NULL or a working url, expressed
  ## as string. Example: tx=NULL (no download attempt) or tx = "https://www.ecad.eu//utils/downloadfile.php?file=download/ECA_nonblend_tx.zip".
  ## Defaults are set to the current URLs to download ECA&D files.

  ## 2) a second set of variables (tx2,tn2,tg2,sd2,ss2,rr2,cc2,hu2,fg2) to download the respective station files. Again, options are a working ECA&D URL as string or NULL
  ## examples: tx2 = NULL or tx2 =  "https://www.ecad.eu//download/ECA_nonblend_info_tx.txt"
  ## Defaults are set to the current URLs to download ECA&D files.

  if(!dir.exists(homefolder)){dir.create(homefolder)}
  rawfolder=paste0(homefolder,'/raw')
  if(!dir.exists(rawfolder)){dir.create(rawfolder)}
  print(paste(Sys.time(),"Created folders to host downloads"),quote=FALSE)
  variables<-c("tx","tn","tg","sd","ss","rr","pp","cc","hu","fg")
  variables2<-c("tx2","tn2","tg2","sd2","ss2","rr2","pp2","cc2","hu2","fg2")
  n<-length(variables)
  for(i in 1:n){
    target<-eval(parse(text=variables[i]))
    if(!is.null(eval(parse(text=variables[i])))){
      print(paste(Sys.time(),'Downloading & uncompressing', toupper(variables[i]),'Series'),quote=FALSE)
      utils::download.file(target,destfile = paste0(rawfolder,"/",variables[i],".zip"),quiet=TRUE)
      utils::unzip(paste0(rawfolder,"/",variables[i],".zip"),exdir=rawfolder)
    }
    target<-eval(parse(text=variables2[i]))
    if(!is.null(eval(parse(text=variables2[i])))){
      print(paste(Sys.time(),'Downloading', toupper(variables[i]),'Stations File'),quote=FALSE)
      utils::download.file(target,destfile = paste0(homefolder,"/ECA_blend_source_",variables[i],".txt"),quiet=TRUE)
    }
  }
}
