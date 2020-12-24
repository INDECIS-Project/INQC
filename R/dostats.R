dostats<-function(){

  #' Create QC statistical summary
  #' @description This function creates two report files (Mystats.txt and CasesSummary.txt) with a statistical summary of QCs performed over the whole data set
  #' @return files with QC summary
  #' @export

  #Get value of 'Global variable' 'homefolder'
  homefolder <- getOption("homefolder")
  myfolder<-paste0(homefolder,'QCSummary/')
  mycases<-paste0(myfolder,'CasesSummary.txt');if(file.exists(mycases)){file.remove(mycases)}
  mystats<-paste0(myfolder,'Mystats.txt');if(file.exists(mystats)){file.remove(mystats)}
  nyu<-list.files(myfolder)
  nyunames<-substring(nyu,8,21)
  n<-length(nyu)
  for(i in 1:n){
    ja<-utils::read.table(paste0(myfolder,nyu[i]))
    emu<-data.frame(variable=substring(nyunames[i],1,2),STAID=substring(nyunames[i],9,14),ITEM=ja[,1],CASES=ja[,2])
    divide<-which(emu$ITEM=='QC_Code')
    if(divide!=1){
      utils::write.table(emu[1:(divide-1),],mycases,sep='\t',append=TRUE,row.names=FALSE,quote=FALSE,col.names=FALSE)
    }
    utils::write.table(emu[(divide+1):nrow(emu),],mystats,sep='\t',append=TRUE,row.names=FALSE,quote=FALSE,col.names=FALSE)
  }
  nyu<-utils::read.table('./QCSummary/Mystats.txt')
  nyu<-stats::aggregate(nyu[,4],by=list(nyu[,1],nyu[,3]),FUN=sum);names(nyu)<-c('Variable','Code','Freq')
  nyutotal<-stats::aggregate(nyu[,3],list(nyu[,1]),FUN=sum);names(nyutotal)<-c('Variable','Total')
  nyu<-merge(nyu,nyutotal)
  nyu<-nyu[order(nyu[,1],nyu[,2]),]
  nyu$Percentage<-round(nyu$Freq/nyu$Total*100,2)
  utils::write.table(nyu,'./QCSummary/Mystats.txt',sep='\t',row.names=FALSE,quote=FALSE,col.names=FALSE)
  nyi<-utils::read.table('./QCSummary/CasesSummary.txt')
  nyi<-stats::aggregate(nyi[,4],by=list(nyi[,1],nyi[,3]),FUN=sum);names(nyi)<-c('Variable','Test','Freq')
  nyitotal<-stats::aggregate(nyi[,3],list(nyi[,1]),FUN=sum);names(nyitotal)<-c('Variable','Total')
  nyu<-merge(nyi,nyitotal)
  nyi[order(nyi[,1],nyi[,2]),]
  utils::write.table(nyi,'./QCSummary/CasesSummary.txt',sep='\t',row.names=FALSE,quote=FALSE,col.names=FALSE)
}