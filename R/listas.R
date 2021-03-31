listas<-function(country='all',name='allstations.txt'){ #NECESITO parametrizar listas. Usar esa parametrizacion par subset de downloads too.

  #' Creates listings for stations ('non-blended' case) linking STAID and SOUID
  #' @description This function takes all the elements and rbinds them into a single list to process
  #' @param country country for which the list is created. If 'all', no country filter.
  #' @param name output file name, do not touch, default is always good.
  #' @return data frame and the list file containing all stations for all elements, linking STAID and SOUID 
  #' and metadata
  #' @examples
  #' #Set a temporal working directory:
  #' wd <- tempdir(); wd0 <- setwd(wd)
  #' #Extract the non-blended ECA&D station files from the example data folder
  #' #Only TX (maximum air temperature) and CC (cloud cover) variables are used in the example
  #' path2txlist<-system.file("extdata", "ECA_blend_source_tx.txt", package = "INQC")
  #' txlist<-readr::read_lines_raw(path2txlist)
  #' readr::write_lines(txlist,'ECA_blend_source_tx.txt')
  #' path2cclist<-system.file("extdata", "ECA_blend_source_cc.txt", package = "INQC")
  #' cclist<-readr::read_lines_raw(path2cclist)
  #' readr::write_lines(cclist,'ECA_blend_source_cc.txt')
  #' options("homefolder"='./')
  #' liston.nb<-listas(country='all',name='allstations.txt')
  #' #The created list file can be found in the directory:
  #' print(wd)
  #' #Return to user's working directory:
  #' setwd(wd0)
  #' @export

  #Get value of 'Global variable' 'homefolder'
  homefolder <- getOption("homefolder")
  variables<- c('TX','TN','TG','RR','HU','PP','SS','FG', 'FX', 'DD','SD', 'CC')
  ene<-length(variables)
  missing= -9999
  ereseunnyu<-0
  for(i in 1:ene){
    list<-paste(homefolder,'ECA_blend_source_',tolower(variables[i]),'.txt',sep='')
    if(file.exists(list)){
      ereseunnyu<-ereseunnyu+1
      x<-utils::read.csv(list,header=FALSE,stringsAsFactors = FALSE,flush=TRUE,strip.white = TRUE)
      names(x)<-c('STAID','SOUID','SOUNAME','CN','LAT','LON','HGTH','ELEI','START','STOP','PARID','PARNAME')
      if(ereseunnyu==1){todas<-x}else{todas<-rbind(todas,x)}
    }
  }
  if(country!='all'){target<-which(todas$CN == country);todas<-todas[target,]}
  utils::write.csv(todas,paste(homefolder,name,sep='')) ## as consequence of the previous action
  return(todas)
}
