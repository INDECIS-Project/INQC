listas<-function(country='all',name='allstations.txt'){ #NECESITO parametrizar listas. Usar esa parametrizacion par subset de downloads too.

  #' Creates listings for stations linking STAID and SOUID
  #' @description This function takes all the elements and rbinds them into a single list to process
  # @param rooty home directory where the "ECA_blend_source*" files are located
  #' @param country country for which the list is created. If all, no country filter.
  #' @param name output file name, do not touch, default is always good.
  #' @return data frame and the list file containing all stations for all elements, linking STAID and SOUID and metadata
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
  utils::write.csv(todas,paste(homefolder,name,sep='')) ## as consecuence of the previous action
  return(todas)
}
