### THIS CODE IS MADE by ENRIC AGUILAR, Centre for Climate Change, C3, URV, Tarragona, Spain  
### It is provided free under the terms of the GNU Lesser General Public License as published by the Free Software Foundation,
# version 3.0 of the License. It is distributed under the terms of this license 'as-is' and has not been designed or prepared to meet any Licensee's particular requirements. 
# The author and its institution make no warranty, either express or implied, including but not limited to, warranties of merchantability or fitness for a particular
# purpose. In no event will they will be liable for any indirect, special, consequential or other damages attributed to the Licensee's use of The Library. 
# In downloading The Library you understand and agree to these terms and those of the associated LGP License. See the GNU Lesser General Public License for more details.
# <http://www.gnu.org/licenses/lgpl.html> or contact the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.


#Updates December, 2018:
## 1.- Corrected some labels in for the temperature qc output that were erroneous (e.g., toomany instead of toomanymonth and toomanyyear)
## 2.- Added consolidator, a functions which creates a second version of the qc'd data. Creates a replica of the original series, with the same name 
## but with the QC column updated to 0 (OK)
## 3.- Corrected wrong default element flag in selepe (from TN to )
## 4.- Made a few corrections regarding the chain of functions txtn()<-closetxtn()<-listas()<-distHaversine() There were a few errors (hopefully, corrected) and now it is much faster
##     just by limiting the station past as candidates to disthaversine to those in a radius of 1deg lat and 1deg lon from the candidate. For the whole ECA&D, 
##     the performance is down from 14 seconds to less than 2!
## 5.- Further improvements to the closesetation() function: does not give an error if the tries to open a file listed but non-existing
## 6.- Corrected problems in roundprecip and a bad parametrizion in precipip()
## 7.- Modified potpareto() function to avoid problems with some matrices encountered in drywetlong: myfit<-fpot(xx,threshold,std.err=FALSE)	
## 8.- Modified drywetlong to avoid problems with desertic locations with almost no rain events

##Updates January, 2019: 
## 1.- Corrected many problems encountered at roundprecip(). It used to flag most of the values due to an error. This also made the code really slow. Solved. 

## WARNINGS:
# None at this point




# NOTE: TX_SOUID101148.txt
# When you open this file, the header reads: "this is the non-blended series (SOUID: 101148) of station Fokstua, NORWAY (STAID:    330)"
# meanwhile in the text file, reads: 330,109445,FOKSTUA,NO,+62:07:00,+009:16:59, 952, TX2,19570101,19680531,   22,Hans Olav Hygen    
# so, a different ID. The id 101148 was unexisting and this produced an error in txtn file. Now is ressistant to this.

## NOTE: some series (e.g. TEL AVIV) have wrong start and end dates in the station files


if(!require(fitdistrplus)){install.packages('fitdistrplus')}
if(!require(evd)){install.packages('evd')}
if(!require(gdata)){install.packages('gdata')}
if(!require(spatial)){install.packages('spatial')}
if(!require(dplyr)){install.packages('dplyr')}
if(!require(geosphere)){install.packages('geosphere')}
if(!require(zoo)){install.packages('zoo')}
if(!require(Hmisc)){install.packages('Hmisc')}
if(!require(RCurl)){install.packages('RCurl')}
if(!require(utils)){install.packages('unzip')}


require(fitdistrplus)
require(evd)
require(gdata)
require(spatial)
require(dplyr)
require(geosphere)
require(zoo)
require(Hmisc)
require(RCurl)
require(utils)





inqc<-function(homefolder='../Sweden/'){

# OBJECTIVE: wraper for QC'ing all varibales
# PARAMETERS: 
# homefolder: the path to the homefolder, as string
# RETURNS: the QC results, in both formats (verbose and workable file in exact ECA&D format)  
  
if(!dir.exists(paste0(homefolder,'QC'))){dir.create(paste0(homefolder,'QC'))}
if(!dir.exists(paste0(homefolder,'QCConsolidated'))){dir.create(paste0(homefolder,'QCConsolidated'))}
  
#temperature(home=homefolder,element='TX')  
#temperature(home=homefolder,element='TN')
#temperature(home=homefolder,element='TG')
precip(home=homefolder)
relhum(home=homefolder)
selepe(home=homefolder)
snowdepth(home=homefolder)
sundur(home=homefolder)  
windspeed(home=homefolder)
rm(liston)
}

downloadator<-function(homefolder='../ecad_updated',
                     tx="https://www.ecad.eu//utils/downloadfile.php?file=download/ECA_nonblend_tx.zip",
                     tx2="https://www.ecad.eu//download/ECA_blend_source_tx.txt",
                     tn="https://www.ecad.eu//utils/downloadfile.php?file=download/ECA_nonblend_tn.zip",
                     tn2="https://www.ecad.eu//download/ECA_blend_source_tn.txt",
                     tg="https://www.ecad.eu//utils/downloadfile.php?file=download/ECA_nonblend_tg.zip",
                     tg2="https://www.ecad.eu//download/ECA_blend_source_tg.txt",
                     sd="https://www.ecad.eu//utils/downloadfile.php?file=download/ECA_nonblend_sd.zip",
                     sd2="https://www.ecad.eu//download/ECA_blend_source_sd.txt",
                     ss="https://www.ecad.eu//utils/downloadfile.php?file=download/ECA_nonblend_ss.zip",
                     ss2="https://www.ecad.eu//download/ECA_blend_source_ss.txt",
                     rr="https://www.ecad.eu//utils/downloadfile.php?file=download/ECA_nonblend_rr.zip",
                     rr2="https://www.ecad.eu//download/ECA_blend_source_rr.txt",
                     pp="https://www.ecad.eu//utils/downloadfile.php?file=download/ECA_nonblend_pp.zip",
                     pp2="https://www.ecad.eu//download/ECA_blend_source_pp.txt",
                     cc="https://www.ecad.eu//utils/downloadfile.php?file=download/ECA_nonblend_cc.zip",
                     cc2="https://www.ecad.eu//download/ECA_blend_source_cc.txt",
                     hu="https://www.ecad.eu//utils/downloadfile.php?file=download/ECA_nonblend_hu.zip",
                     hu2="https://www.ecad.eu//download/ECA_blend_source_hu.txt",
                     fg="https://www.ecad.eu//utils/downloadfile.php?file=download/ECA_nonblend_fg.zip",
                     fg2="https://www.ecad.eu//download/ECA_blend_source_fg.txt"
                     ){

## OBJECTIVE: downloads latest blended data from ECA&D and saves into "./homefolder/raw (being homefolder configurable)
## PARAMETERS 

## homefolder: in the form "./homefolder" : the function will store there the station files and create ./homefolder/raw and will store there the data  
  
## 1) a set of variables (tx,tn,tg,sd,ss,rr,cc,hu,fg) which represent the variables analyzed at the INDECIS project.They can be either NULL or a working url, expressed
## as string. Example: tx=NULL (no download attempt) or tx = "https://www.ecad.eu//utils/downloadfile.php?file=download/ECA_nonblend_tx.zip" . 
##  Defaults are set to the current URLs to download ECA&D files.   
  
## 2) a second set of variables (tx2,tn2,tg2,sd2,ss2,rr2,cc2,hu2,fg2) to download the respective station files. Again, options are a working ECA&D URL as string or NULL
## examples: tx2 = NULL or tx2 =  "https://www.ecad.eu//download/ECA_nonblend_info_tx.txt" 
##  Defaults are set to the current URLs to download ECA&D files.  
  
  
    
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
    download.file(target,destfile = paste0(rawfolder,"/",variables[i],".zip"),quiet=TRUE)
    unzip(paste0(rawfolder,"/",variables[i],".zip"),exdir=rawfolder)
    }    
    target<-eval(parse(text=variables2[i]))
  if(!is.null(eval(parse(text=variables2[i])))){
    print(paste(Sys.time(),'Downloading', toupper(variables[i]),'Stations File'),quote=FALSE)
    download.file(target,destfile = paste0(homefolder,"/ECA_blend_source_",variables[i],".txt"),quiet=TRUE)
    }    
  }
}

consolidator<-function(home='../Sweden/',filename,x){

header<-readheader(paste0(home,'raw/',filename))
element<-substring(filename,1,2)
x$consolidated = 0

x$consolidated[which(x$repeatedvalue==1)]<-2
x$consolidated[which(x$drywetlong==1)]<-2
x$consolidated[which(x$suspectacumprec==1)]<-2
x$consolidated[which(x$paretogadget==1)]<-2
x$consolidated[which(x$jumps==1)]<-2
x$consolidated[which(x$flat==1)]<-2
x$consolidated[which(x$roundmax==1)]<-2
x$consolidated[which(x$IQRoutliers==1)]<-2
x$consolidated[which(x$toomanymonth==1)]<-2
x$consolidated[which(x$toomanyyear==1)]<-2
x$consolidated[which(x$rounding==1)]<-2
x$consolidated[which(x$large==1)]<-1
x$consolidated[which(x$small==1)]<-1
x$consolidated[which(x$weirddate==1)]<-1
x$consolidated[which(x$dupli==1)]<-1
x$consolidated[which(x$friki==1)]<-1
x$consolidated[which(x$txtn==1)]<-1
grannyu<-ncol(x)
x<-x[,c(1:4,grannyu)]

write.table(header,paste0(home,'QCConsolidated/',filename),quote=FALSE,col.names=FALSE,row.names=FALSE,sep=',',na='')
write.table(x,paste0(home,'QCConsolidated/',filename),col.names=FALSE,row.names=FALSE,sep='\t',quote=FALSE,append=TRUE)
print(paste(Sys.time(),'Wrote QCd file'),quote=FALSE)
  
}



# Data is not organized in folders 

temperature<-function(home='../Sweden/',large=500,small=-500,maxjump=150,maxseq=3,margina=80,
               level=3,window=11,roundmax=10,blocksize=10,step=30,blockmanymonth=15,blockmanyyear=180,
               blocksizeround=20,
               element='TX'){
  # OBJECTIVE: this function will centralize temperature-like qc routines. Will create a file in the folder QC
  # with an additional 0/1 column, where "1" means test failed. 
  # home: path to the home directory
  # element: two-letters ECA&D code for the element (e.g., TX for maximum temperature)  
  # large: value above which the observation is considered physically impossible for the region
  # small: value below which the observation is considered physically impossible for the region    
  # maxseq: maximum number of consecutive repeated values, for flat function (11.1,11.1,11.1 would be 3 consecutives). 
  # roundmax: maximum number of consecutive decimal part value, for flat function (10.0, 11.0, 12.0 would be 3 consecutive)
  # blocksizeround: maximum number of values in a month with the same decimal, for rounding function
  
  lista<-list.files(path=paste(home,'raw',sep=''),pattern='SOUID')
  tx<-lista[which(substring(lista,1,2)==element)];ene<-length(tx)
  for(i in 1:ene){
    
    name<-paste(home,'raw/',tx[i],sep='')
    
    print(paste(name,i,'of',ene),quote=FALSE)
    x<-readecad(input=name) ; print(paste(Sys.time(),'Ended readecad'),quote=FALSE)
    x<-x[,1:4];colnames(x)<-c('STAID','SOUID','date','value')
    #pattern(x[,4])
    
    bad<-weirddate(x[,3:4]);x$weirddate<-0;if(length(bad)!=0){x$weirddate[bad]<-1}; print(paste(Sys.time(),'Ended weirddate'),quote=FALSE)
    bad<-duplas(x$date);x$dupli<-0;if(length(bad)!=0){x$dupli[bad]<-1}; print(paste(Sys.time(),'Ended duplas'),quote=FALSE)
    bad<-physics(x$value,large,1);x$large<-0;if(length(bad)!=0){x$large[bad]<-1}; print(paste(Sys.time(),'Ended physics, large'),quote=FALSE)
    bad<-physics(x$value,small,3);x$small<-0;if(length(bad)!=0){x$small[bad]<-1}; print(paste(Sys.time(),'Ended physics, small'),quote=FALSE)
    bad<-jumps(x$value,maxjump);x$jump<-0;if(length(bad)!=0){x$jump[bad]<-1}; print(paste(Sys.time(),'Ended jumps'),quote=FALSE)
    bad<-flat(x$value,maxseq);x$flat<-0;if(length(bad)!=0){x$flat[bad]<-1}; print(paste(Sys.time(),'Ended flat for values'),quote=FALSE)
    bad<-flat(x$value%%10,roundmax);x$roundmax<-0;if(length(bad)!=0){x$roundmax[bad]<-1}; print(paste(Sys.time(),'Ended flat for decimal part'),quote=FALSE)
    bad<-badfriki(x$date,x$value,margina);x$friki<-0;if(length(bad)!=0){x$friki[bad]<-1} ; print(paste(Sys.time(),'Ended badfriki'),quote=FALSE)
    bad<-IQRoutliers(x$date,x$value,level,window);x$IQRoutliers<-0;if(length(bad)!=0){x$IQRoutliers[bad]<-1}; print(paste(Sys.time(),'Ended IQR outliers'),quote=FALSE)
    bad<-toomany(x[,3:4],blockmanymonth,1);x$toomanymonth<-0;if(length(bad)!=0){x$toomanymonth[bad]<-1}; print(paste(Sys.time(),'Ended toomany, monthly'),quote=FALSE)
    bad<-toomany(x[,3:4],blockmanyyear,2);x$toomanyyear<-0;if(length(bad)!=0){x$toomanyyear[bad]<-1}; print(paste(Sys.time(),'Ended toomany, annual'),quote=FALSE)
    bad<-rounding(x[,3:4],blocksizeround);x$rounding<-0;if(length(bad)!=0){x$rounding[bad]<-1}; print(paste(Sys.time(),'Ended rounding'),quote=FALSE)
    if(element != 'TG'){bad<-txtn(x[,3:4],tx[i],home);x$txtn<-0;if(length(bad)!=0){x$txtn[bad]<-1}; print(paste(Sys.time(),'Ended txtn'),quote=FALSE)}
    
    consolidator(home,tx[i],x)
    write.table(x,paste(home,'QC/qc_',tx[i],sep=''),col.names=TRUE,row.names=FALSE,sep='\t',quote=FALSE); print(paste(Sys.time(),'Wrote QC results'),quote=FALSE)
    
      }
  
}




selepe<-function(home='../Sweden/',large=15000,small=-8000,maxjump=2000,maxseq=3,margina=100,
               level=3,window=11,roundmax=10,blocksize=10,step=30,blockmanymonth=15,blockmanyyear=180,
               blocksizeround=20,
               element='PP'){
  # OBJECTIVE: this function will centralize temperature-like qc routines. Will create a file in the folder QC
  # with an additional 0/1 column, where "1" means test failed. 
  # home: path to the home directory
  # element: two-letters ECA&D code for the element (e.g., TX for maximum temperature)  
  # large: value above which the observation is considered physically impossible for the region
  # small: value below which the observation is considered physically impossible for the region    
  # maxseq: maximum number of consecutive repeated values, for flat function (11.1,11.1,11.1 would be 3 consecutives). 
  # roundmax: maximum number of consecutive decimal part value, for flat function (10.0, 11.0, 12.0 would be 3 consecutive)
  # blocksizeround: maximum number of values in a month with the same decimal, for rounding function
  # blockmanymonth: maximum number of equal values in a month, FUNCTION: toomany
  # blockmanyyear: maximum number of equal values in a yaer, FUCNTION: toomany
  
  
  lista<-list.files(path=paste(home,'raw',sep=''),pattern='SOUID')
  tx<-lista[which(substring(lista,1,2)==element)];ene<-length(tx)
  for(i in 1:ene){
    
    name<-paste(home,'raw/',tx[i],sep='')
    
    print(paste(name,i,'of',ene),quote=FALSE)
    x<-readecad(input=name) ; print(Sys.time())
    x<-x[,1:4];colnames(x)<-c('STAID','SOUID','date','value')
    #pattern(x[,4])
    
    bad<-weirddate(x[,3:4]);x$weirddate<-0;if(length(bad)!=0){x$weirddate[bad]<-1}; print(paste(Sys.time(),'Ended weirddate'),quote=FALSE)
    bad<-duplas(x$date);x$dupli<-0;if(length(bad)!=0){x$dupli[bad]<-1}; print(paste(Sys.time(),'Ended duplas'),quote=FALSE)
    bad<-physics(x$value,large,1);x$large<-0;if(length(bad)!=0){x$large[bad]<-1}; print(paste(Sys.time(),'Ended physics, large'),quote=FALSE)
    bad<-physics(x$value,small,3);x$small<-0;if(length(bad)!=0){x$small[bad]<-1}; print(paste(Sys.time(),'Ended physics, small'),quote=FALSE)
    bad<-jumps(x$value,maxjump);x$jump<-0;if(length(bad)!=0){x$jump[bad]<-1}; print(paste(Sys.time(),'Ended jumps'),quote=FALSE)
    bad<-flat(x$value,maxseq);x$flat<-0;if(length(bad)!=0){x$flat[bad]<-1}; print(paste(Sys.time(),'Ended flat for values'),quote=FALSE) ## this looks at consecutive equal values
    bad<-flat(x$value%%10,roundmax);x$roundmax<-0;if(length(bad)!=0){x$roundmax[bad]<-1}; print(paste(Sys.time(),'Ended flat for decimal part'),quote=FALSE) #this looks at consecutive decimal parts
    bad<-toomany(x[,3:4],blockmanymonth,1);x$toomanymonth<-0;if(length(bad)!=0){x$toomanymonth[bad]<-1}; print(paste(Sys.time(),'Ended toomany, month'),quote=FALSE)
    bad<-toomany(x[,3:4],blockmanyyear,2);x$toomanyyear<-0;if(length(bad)!=0){x$toomanyyear[bad]<-1}; print(paste(Sys.time(),'Ended toomany, year'),quote=FALSE)
    bad<-badfriki(x$date,x$value,margina);x$friki<-0;if(length(bad)!=0){x$flat[bad]<-1} ; print(paste(Sys.time(),'Ended badfriki'),quote=FALSE)
    bad<-IQRoutliers(x$date,x$value,level,window);x$IQRoutliers<-0;if(length(bad)!=0){x$IQRoutliers[bad]<-1}; print(paste(Sys.time(),'Ended IQRoutliers'),quote=FALSE) # check
    bad<-rounding(x[,3:4],blocksizeround);x$rounding<-0;if(length(bad)!=0){x$rounding[bad]<-1}; print(paste(Sys.time(),'Ended rounding'),quote=FALSE)
    write.table(x,paste(home,'QC/qc_',tx[i],sep=''),col.names=TRUE,row.names=FALSE,sep='\t',quote=FALSE); print(paste(Sys.time(),'Wrote QC results'),quote=FALSE)
  }
  
}



relhum<-function(home='../Sweden/', element='HU',maxseq=3,blocksizeround=20,blockmanymonth=15,blockmanyyear=180){
  # OBJECTIVE: this function will centralize temperature-like qc routines. Will create a file in the folder QC
  # with an additional 0/1 column, where "1" means test failed. 
  # home: path to the home directory
  # element: two-letters ECA&D code for the element (e.g., TX for maximum temperature)  
  # large: value above which the observation is considered physically impossible for the region, FUNCTION: physics
  # small: value below which the observation is considered physically impossible for the region, FUNCTION: physics     
  # maxseq: maximum number of consecutive repeated values, FUNCTION: flat  (11.1,11.1,11.1 would be 3 consecutives).  
  # blocksizeround: maximum number of values in a month with the same decimal, FUNCTION:  rounding 
  # blockmanymonth: maximum number of equal values in a month, FUNCTION: toomany
  # blockmanyyear: maximum number of equal values in a yaer, FUCNTION: toomany
  
  lista<-list.files(path=paste(home,'raw',sep=''),pattern='SOUID')
  tx<-lista[which(substring(lista,1,2)==element)];ene<-length(tx)
  for(i in 1:ene){
    name<-paste(home,'raw/',tx[i],sep='')
    print(paste(name,i,'of',ene),quote=FALSE)
    x<-readecad(input=name) ; print(Sys.time())
    x<-x[,1:4];colnames(x)<-c('STAID','SOUID','date','value')
    bad<-duplas(x$date);x$dupli<-0;if(length(bad)!=0){x$dupli[bad]<-1} ; print(paste(Sys.time(),'Ended duplas'),quote=FALSE)
    bad<-weirddate(x[,3:4]);x$weirddate<-0;if(length(bad)!=0){x$weirddate[bad]<-1}; print(paste(Sys.time(),'Ended weirddate'),quote=FALSE)
    bad<-toomany(x[,3:4],blockmanymonth,1);x$toomanymonth<-0;if(length(bad)!=0){x$toomanymonth[bad]<-1}; print(paste(Sys.time(),'Ended toomany, month'),quote=FALSE)
    bad<-toomany(x[,3:4],blockmanyyear,2);x$toomanyyear<-0;if(length(bad)!=0){x$toomanyyear[bad]<-1}; print(paste(Sys.time(),'Ended toomany, year'),quote=FALSE)
    bad<-flat(x$value,maxseq);x$flat<-0;if(length(bad)!=0){x$flat[bad]<-1}; print(paste(Sys.time(),'Ended flat for values'),quote=FALSE)## this looks at consecutive equal values
    bad<-flat(x$value%%10,roundmax);x$roundmax<-0;if(length(bad)!=0){x$roundmax[bad]<-1}; print(paste(Sys.time(),'Ended flat for decimal part'),quote=FALSE) #this looks at consecutive decimal parts
    bad<-rounding(x[,3:4],blocksizeround);x$rounding<-0;if(length(bad)!=0){x$rounding[bad]<-1}; print(paste(Sys.time(),'Ended rounding'),quote=FALSE)
    bad<-physics(x$value,100,1);x$large<-0;if(length(bad)!=0){x$large[bad]<-1}; print(paste(Sys.time(),'Ended physics, large'),quote=FALSE)
    bad<-physics(x$value,0,3);x$small<-0;if(length(bad)!=0){x$small[bad]<-1}; print(paste(Sys.time(),'Ended physics, small'),quote=FALSE)
    write.table(x,paste(home,'QC/qc_',tx[i],sep=''),col.names=TRUE,row.names=FALSE,sep='\t',quote=FALSE); print(paste(Sys.time(),'Wrote QC results'),quote=FALSE)
  }
  
}


sundur<-function(home='../Sweden/', element='SS',maxseq=3,blocksizeround=20,blockmanymonth=15,blockmanyyear=180){
  # OBJECTIVE: this function will centralize temperature-like qc routines. Will create a file in the folder QC
  # with an additional 0/1 column, where "1" means test failed. 
  # home: path to the home directory
  # element: two-letters ECA&D code for the element (e.g., TX for maximum temperature)  
  # large: value above which the observation is considered physically impossible for the region, FUNCTION: physics
  # small: value below which the observation is considered physically impossible for the region, FUNCTION: physics     
  # maxseq: maximum number of consecutive repeated values, FUNCTION: flat  (11.1,11.1,11.1 would be 3 consecutives).  
  # blocksizeround: maximum number of values in a month with the same decimal, FUNCTION:  rounding 
  # blockmanymonth: maximum number of equal values in a month, FUNCTION: toomany
  # blockmanyyear: maximum number of equal values in a yaer, FUCNTION: toomany
  
  lista<-list.files(path=paste(home,'raw',sep=''),pattern='SOUID')
  tx<-lista[which(substring(lista,1,2)==element)];ene<-length(tx)
  for(i in 1:ene){
    name<-paste(home,'raw/',tx[i],sep='')
    print(paste(name,i,'of',ene),quote=FALSE)
    x<-readecad(input=name) ; print(Sys.time())
    x<-x[,1:4];colnames(x)<-c('STAID','SOUID','date','value')
    bad<-duplas(x$date);x$dupli<-0;if(length(bad)!=0){x$dupli[bad]<-1} ; print(paste(Sys.time(),'Ended duplas'),quote=FALSE)
    bad<-weirddate(x[,3:4]);x$weirddate<-0;if(length(bad)!=0){x$weirddate[bad]<-1}; print(paste(Sys.time(),'Ended weirddate'),quote=FALSE)
    bad<-toomany(x[,3:4],blockmanymonth,1);x$toomanymonth<-0;if(length(bad)!=0){x$toomanymonth[bad]<-1}; print(paste(Sys.time(),'Ended toomany, month'),quote=FALSE)
    bad<-toomany(x[,3:4],blockmanyyear,2);x$toomanyyear<-0;if(length(bad)!=0){x$toomanyyear[bad]<-1}; print(paste(Sys.time(),'Ended toomany, year'),quote=FALSE)
    bad<-flat(x$value,maxseq);x$flat<-0;if(length(bad)!=0){x$flat[bad]<-1}; print(paste(Sys.time(),'Ended flat for values'),quote=FALSE)
    bad<-flat(x$value%%10,roundmax);x$roundmax<-0;if(length(bad)!=0){x$roundmax[bad]<-1}; print(paste(Sys.time(),'Ended flat for decimal part'),quote=FALSE) #this looks at consecutive decimal parts
    bad<-rounding(x[,3:4],blocksizeround);x$rounding<-0;if(length(bad)!=0){x$rounding[bad]<-1}; print(paste(Sys.time(),'Ended rounding'),quote=FALSE)
    bad<-physics(x$value,240,1);x$large<-0;if(length(bad)!=0){x$large[bad]<-1}; print(paste(Sys.time(),'Ended physics, large'),quote=FALSE)
    bad<-physics(x$value,0,3);x$small<-0;if(length(bad)!=0){x$small[bad]<-1}; print(paste(Sys.time(),'Ended physics, small'),quote=FALSE)
    write.table(x,paste(home,'QC/qc_',tx[i],sep=''),col.names=TRUE,row.names=FALSE,sep='\t',quote=FALSE); print(paste(Sys.time(),'Wrote QC results'),quote=FALSE)
  }
}

clocov<-function(home='../Sweden/', element='CC',maxseq=3,blocksizeround=20,blockmanymonth=20,blockmanyyear=200){
  # OBJECTIVE: this function will centralize temperature-like qc routines. Will create a file in the folder QC
  # with an additional 0/1 column, where "1" means test failed. 
  # home: path to the home directory
  # element: two-letters ECA&D code for the element (e.g., TX for maximum temperature)  
  # large: value above which the observation is considered physically impossible for the region, FUNCTION: physics
  # small: value below which the observation is considered physically impossible for the region, FUNCTION: physics     
  # maxseq: maximum number of consecutive repeated values, FUNCTION: flat  (11.1,11.1,11.1 would be 3 consecutives).  
  # blocksizeround: maximum number of values in a month with the same decimal, FUNCTION:  rounding 
  # blockmanymonth: maximum number of equal values in a month, FUNCTION: toomany
  # blockmanyyear: maximum number of equal values in a yaer, FUCNTION: toomany
  
  lista<-list.files(path=paste(home,'raw',sep=''),pattern='SOUID')
  tx<-lista[which(substring(lista,1,2)==element)];ene<-length(tx)
  for(i in 1:ene){
    name<-paste(home,'raw/',tx[i],sep='')
    print(paste(name,i,'of',ene),quote=FALSE)
    x<-readecad(input=name) ; print(Sys.time())
    x<-x[,1:4];colnames(x)<-c('STAID','SOUID','date','value')
    bad<-duplas(x$date);x$dupli<-0;if(length(bad)!=0){x$dupli[bad]<-1} ; print(paste(Sys.time(),'Ended duplas'),quote=FALSE)
    bad<-weirddate(x[,3:4]);x$weirddate<-0;if(length(bad)!=0){x$weirddate[bad]<-1}; print(paste(Sys.time(),'Ended weirddate'),quote=FALSE)
    bad<-toomany(x[,3:4],blockmanymonth,1);x$toomanymonth<-0;if(length(bad)!=0){x$toomanymonth[bad]<-1}; print(paste(Sys.time(),'Ended toomany, month'),quote=FALSE)
    bad<-toomany(x[,3:4],blockmanyyear,2);x$toomanyyear<-0;if(length(bad)!=0){x$toomanyyear[bad]<-1}; print(paste(Sys.time(),'Ended toomany, year'),quote=FALSE)
    bad<-flat(x$value,maxseq);x$flat<-0;if(length(bad)!=0){x$flat[bad]<-1}; print(paste(Sys.time(),'Ended flat for values'),quote=FALSE)
    bad<-rounding(x[,3:4],blocksizeround);x$rounding<-0;if(length(bad)!=0){x$rounding[bad]<-1}; print(paste(Sys.time(),'Ended rounding'),quote=FALSE)
    bad<-physics(x$value,8,1);x$large<-0;if(length(bad)!=0){x$large[bad]<-1}; print(paste(Sys.time(),'Ended physics, large'),quote=FALSE)
    bad<-physics(x$value,0,3);x$small<-0;if(length(bad)!=0){x$small[bad]<-1}; print(paste(Sys.time(),'Ended physics, small'),quote=FALSE)
    write.table(x,paste(home,'QC/qc_',tx[i],sep=''),col.names=TRUE,row.names=FALSE,sep='\t',quote=FALSE); print(paste(Sys.time(),'Wrote QC results'),quote=FALSE)
  }
}



windspeed<-function(home='../Sweden/', element='FG',maxseq=3,blocksizeround=20,blockmanymonth=20,blockmanyyear=200,large=3000){
  # OBJECTIVE: this function will centralize temperature-like qc routines. Will create a file in the folder QC
  # with an additional 0/1 column, where "1" means test failed. 
  # home: path to the home directory
  # element: two-letters ECA&D code for the element (e.g., TX for maximum temperature)  
  # large: value above which the observation is considered physically impossible for the region, FUNCTION: physics
  # small: value below which the observation is considered physically impossible for the region, FUNCTION: physics     
  # maxseq: maximum number of consecutive repeated values, FUNCTION: flat  (11.1,11.1,11.1 would be 3 consecutives).  
  # blocksizeround: maximum number of values in a month with the same decimal, FUNCTION:  rounding 
  # blockmanymonth: maximum number of equal values in a month, FUNCTION: toomany
  # blockmanyyear: maximum number of equal values in a year, FUCNTION: toomany
  
  lista<-list.files(path=paste(home,'raw',sep=''),pattern='SOUID')
  tx<-lista[which(substring(lista,1,2)==element)];ene<-length(tx)
  for(i in 1:ene){
    name<-paste(home,'raw/',tx[i],sep='')
    print(paste(name,i,'of',ene),quote=FALSE)
    x<-readecad(input=name) ; print(Sys.time())
    x<-x[,1:4];colnames(x)<-c('STAID','SOUID','date','value')
    bad<-duplas(x$date);x$dupli<-0;if(length(bad)!=0){x$dupli[bad]<-1} ; print(paste(Sys.time(),'Ended duplas'),quote=FALSE)
    bad<-weirddate(x[,3:4]);x$weirddate<-0;if(length(bad)!=0){x$weirddate[bad]<-1}; print(paste(Sys.time(),'Ended weirddate'),quote=FALSE)
    bad<-toomany(x[,3:4],blockmanymonth,1);x$toomanymonth<-0;if(length(bad)!=0){x$toomanymonth[bad]<-1}; print(paste(Sys.time(),'Ended toomany, month'),quote=FALSE)
    bad<-toomany(x[,3:4],blockmanyyear,2);x$toomanyyear<-0;if(length(bad)!=0){x$toomanyyear[bad]<-1}; print(paste(Sys.time(),'Ended toomany, year'),quote=FALSE)
    bad<-flat(x$value,maxseq);x$flat<-0;if(length(bad)!=0){x$flat[bad]<-1}; print(paste(Sys.time(),'Ended flat for values'),quote=FALSE)
    bad<-rounding(x[,3:4],blocksizeround);x$rounding<-0;if(length(bad)!=0){x$rounding[bad]<-1}; print(paste(Sys.time(),'Ended rounding'),quote=FALSE)
    bad<-physics(x$value,large,1);x$large<-0;if(length(bad)!=0){x$large[bad]<-1}; print(paste(Sys.time(),'Ended physics, large'),quote=FALSE)
    bad<-physics(x$value,0,3);x$small<-0;if(length(bad)!=0){x$small[bad]<-1}; print(paste(Sys.time(),'Ended physics, small'),quote=FALSE)
    write.table(x,paste(home,'QC/qc_',tx[i],sep=''),col.names=TRUE,row.names=FALSE,sep='\t',quote=FALSE); print(paste(Sys.time(),'Wrote QC results'),quote=FALSE)
  }
}



snowdepth<-function(home='../Sweden/', element='SD',maxseq=3,blocksizeround=20,blockmanymonth=20,blockmanyyear=200,large=5000){
  # OBJECTIVE: this function will centralize temperature-like qc routines. Will create a file in the folder QC
  # with an additional 0/1 column, where "1" means test failed. 
  # home: path to the home directory
  # element: two-letters ECA&D code for the element (e.g., TX for maximum temperature)  
  # large: value above which the observation is considered physically impossible for the region, FUNCTION: physics
  # small: value below which the observation is considered physically impossible for the region, FUNCTION: physics     
  # maxseq: maximum number of consecutive repeated values, FUNCTION: flat  (11.1,11.1,11.1 would be 3 consecutives).  
  # blocksizeround: maximum number of values in a month with the same decimal, FUNCTION:  rounding 
  # blockmanymonth: maximum number of equal values in a month, FUNCTION: toomany
  # blockmanyyear: maximum number of equal values in a yaer, FUCNTION: toomany
  
  lista<-list.files(path=paste(home,'raw',sep=''),pattern='SOUID')
  tx<-lista[which(substring(lista,1,2)==element)];ene<-length(tx)
  for(i in 1:ene){
    name<-paste(home,'raw/',tx[i],sep='')
    print(paste(name,i,'of',ene),quote=FALSE)
    x<-readecad(input=name) ; print(Sys.time())
    x<-x[,1:4];colnames(x)<-c('STAID','SOUID','date','value')
    bad<-duplas(x$date);x$dupli<-0;if(length(bad)!=0){x$dupli[bad]<-1} ; print(paste(Sys.time(),'Ended duplas'),quote=FALSE)
    bad<-weirddate(x[,3:4]);x$weirddate<-0;if(length(bad)!=0){x$weirddate[bad]<-1}; print(paste(Sys.time(),'Ended weirddate'),quote=FALSE)
#    bad<-toomany(x[,3:4],blockmanymonth,1);x$toomany<-0;if(length(bad)!=0){x$toomany[bad]<-1}; print(Sys.time())
#    bad<-toomany(x[,3:4],blockmanyyear,2);x$toomany<-0;if(length(bad)!=0){x$toomany[bad]<-1}; print(Sys.time())
    bad<-flat(x$value,maxseq);x$flat<-0;if(length(bad)!=0){x$flat[bad]<-1}; print(paste(Sys.time(),'Ended flat for values'),quote=FALSE)
    bad<-rounding(x[,3:4],blocksizeround);x$rounding<-0;if(length(bad)!=0){x$rounding[bad]<-1}; print(paste(Sys.time(),'Ended rounding'),quote=FALSE)
    bad<-physics(x$value,large,1);x$large<-0;if(length(bad)!=0){x$large[bad]<-1}; print(paste(Sys.time(),'Ended physics, large'),quote=FALSE)
    bad<-physics(x$value,0,3);x$small<-0;if(length(bad)!=0){x$small[bad]<-1}; print(paste(Sys.time(),'Ended physics, small'),quote=FALSE)
    write.table(x,paste(home,'QC/qc_',tx[i],sep=''),col.names=TRUE,row.names=FALSE,sep='\t',quote=FALSE); print(paste(Sys.time(),'Wrote QC results'),quote=FALSE)
  }
}




precip<-function(home='../Sweden/',large=5000,small=0,element='RR',ret=500,retornoracha=1000,margin=20,friki=150,blocksizeround=20, excluido=0,
                 blockmanymonth=15,blockmanyyear=180,exclude=0,limit=2000,tolerance=10){
# OBJECTIVE: this function will centralize precipitation-like qc routines. Will create a file in the folder QC
# with an additional 0/1 column, where "1" means test failed. 
# INPUT: 
# home: path to the home directory
# element: two-letters ECA&D code for the element (e.g., RR for precipitation)  
# large: value above which the observation is considered physically impossible for the region
# small: value below which the observation is considered physically impossible for the region    
# ret: pseudo-return period for the pareto outliers  
# retornoracha: return period for the calculation of the maximum dry and wet spell  
# margin: frequency difference between consecutive values for repeatedvalue()
# friki: minimum value to be considered by repeatedvalue()  
# blockmanymonth: maximum number of equal values in a month, FUNCTION: toomany
# blockmanyyear: maximum number of equal values in a yaer, FUCNTION: toomany
# exclude: values to be excluded, FUNCTION toomany
# limit: cut threshold for FUNCTION suspectacumprec
# tolerance: number of NA or 0s before allowed before the limit, FUNCTION suspectacumprec 
# blocksizeround: the maximum number of repeated values, FUNCTION roundprecip
# excluido: values to be excluded, FUNCTION roundprecip  
  
  lista<-list.files(path=paste(home,'raw',sep=''),pattern='SOUID')
  tx<-lista[which(substring(lista,1,2)==element)];ene<-length(tx)
  for(i in 1:ene){
  
    name<-paste(home,'raw/',tx[i],sep='')
    print(paste(name,i,'of',ene),quote=FALSE)
    x<-readecad(input=name) ; print(paste(Sys.time(),'Ended readecad'),quote=FALSE)
    
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
    bad<-toomany(x[,3:4],blockmanymonth,1,exclude);x$toomanymonth<-0;if(length(bad)!=0){x$toomany[bad]<-1}; print(paste(Sys.time(),'Ended toomany, month'),quote=FALSE)
    bad<-toomany(x[,3:4],blockmanyyear,2,exclude);x$toomanyyear<-0;if(length(bad)!=0){x$toomany[bad]<-1}; print(paste(Sys.time(),'Ended toomany, year'),quote=FALSE)
    
    write.table(x,paste(home,'QC/qc_',tx[i],sep=''),col.names=TRUE,row.names=FALSE,sep='\t',quote=FALSE); print(paste(Sys.time(),'Wrote QC results'),quote=FALSE)
  }
  
  
}


weirddate<-function(x){
  fy<-as.numeric(substring(x[1,1],1,4))
  ly<-as.numeric(substring(x[nrow(x),1],1,4))
  x$year<-as.numeric(substring(x[,1],1,4));x$month<-as.numeric(substring(x[,1],5,6));x$day<-as.numeric(substring(x[,1],7,8))
  k1<-which(x$year < fy | x$year > ly)
  k2<-which(x$day > 31)
  k3<-which(x$day > 30 & x$month %in% c(4,6,9,11))
  k4<-which(x$day > 29 & x$month ==2)
  k5<-which(x$day > 28 & x$month == 2 & x$year%%4 == 0 & x$year !=1900)
  k6<-which(x$month < 1 | x$month > 12)
  bad<-c(k1,k2,k3,k4,k5,k6)
  return(bad)
  }


roundprecip<-function(y,blocksize=20,exclude=0){
  # Objective: splits data by month and looks if a decimal value is repeated too many times
  # INPUT: 
  # y: two columns with date and data
  # blocksize: the maximum number of repeated values
  # exclude: the value to be excluded (zero for precip)
  # RETURNS:
  # bad: list of positions which do not pass qc
  bad<-NULL 
  y[,1]<-as.numeric(substring(y[,1],1,6))
  ### Achtuuung!!!
  y[,3]<-as.integer(substring(y[,2],nchar(y[,2]),nchar(y[,2]))) ### This is a better way to find the decimal part in ECA&D: it is always the last character. 
  
  zerapio<-which(y[,2] %in% exclude | is.na(y[,2]))
  
  
  z<-y[-zerapio,]
  
  

  #### Warning! This line was erroneous and was: 
  #nyu<-as.data.frame(table(y[,1],y[,2])) ### this is good, it can identify those which are over the blocksize, but need to now how to extract/label the values
  # This was causing the code to be really slow, as it was looking at "y" instead of "z". This was labeling most of the values as erroneous and the next loop was taking
  # forever. 
  nyu<-as.data.frame(table(z[,1],z[,3])) ### this is good, it can identify those which are over the blocksize, but need to now how to extract/label the values
  
  target<-which(nyu[,3] >= blocksize)
  ene<-length(target)
  browser()
  if(ene > 0){
    for(i in 1:ene){
      tirget<-nyu[target[i],]   
      fecha<-tirget[,1]
      valor<-tirget[,2]
      wanted<-which(y[,1] == fecha & y[,3] == valor) ## Modified: y[,3] now contains the "decimal" part. 
      if(i == 1){bad<-wanted}else{bad<-c(bad,wanted)}
    }  
  }
  bad<-unique(bad)
  browser()
  return(bad)
}


repeatedvalue<-function(x,margin=20,friki=150){
  # OBJECTIVE: this function looks for a value which repeats too many times and, given the decaying shape of preciptiation empirical distro,
  ### is considered too large to happen that many times
  # INPUT: 
  # x: preciptiation time series
  # margin: the difference in frequency the nearest value
  # friki: the minimum value to be considered
  # RETURNS:
  # target: list of values which do not pass QC
  
  target<-NULL
  nyu<-table(x) # table of values
  prospect<-which(diff(nyu) > margin) ### we look at which values present high differences in frequency. We do not expect it to happen in small values
  ### as we expect decaying freqs. It may happen because of rounding or randomly a few times, so we will look when this happens in values larger than 
  ### friki
   valores<-as.numeric(names(prospect)) ### extract the values
   ene<-length(valores)
   
   #### Achtung! Acomodar para length 0 de ene
   cutposition<-first(which(valores > friki))
   #cutposition<-first(which(diff(valores) > friki))+1
   if(length(cutposition)==0){return(NULL)} ### if there are no values longer than cutposition, returns NULL. Revisit
   if(!is.na(cutposition)){
   ojete<-valores[cutposition:ene]
   target<-which(x %in% ojete)
   }
  return(target)
  
}





###### Adapt this three functions to the structure of using only "x"
paretogadget<-function(x,ret){
  # x is the series you are going to use (will take precip)
  # ret is the pseudo return period (remember is pareto and I destroyed the years)  
  # Auxiliated by potpareto, returnpotpareto, computecal  

  nyu<-potpareto(x);if(is.null(nyu)){return(NULL)}
  mus<-returnpotpareto(nyu,ret)
  target<-which(x > mus)
  return(target)
  }

potpareto<-function(y,thres=0.99){
  target<-which(!is.na(y) & y!=0)
  xx<-y[target]          
 
   threshold<-quantile(xx,thres,na.rm=TRUE)
  if( threshold >= max(xx)){return(NULL)}
  
  #### Now, it is necessary to make this ressistant to desertic places with small number of rainy days
  
 
  ## myfit<-fpot(xx,threshold). Changed to the line below, as with some matrices the std.err=TRUE gives problems
  myfit<-fpot(xx,threshold,std.err=FALSE)	
  
  
  
  
  
  #  name<-'mypareto.jpg'
  #  name<-paste('potpareto',substring(id,1,10),'%03d.jpg',sep='')
  #  jpeg(name)
  #  plot(myfit)
  #  dev.off()
  return(myfit)
}

returnpotpareto<-function(pato,ret=retorno,w=1.65){
  loc<-pato$threshold
  shape<-pato$estimate[2]	
  scale<-pato$estimate[1]
  quanty<-1-(1/ret*w)	
  mle<-qgpd(quanty,loc=loc,shape=shape,scale=scale)	
  return(as.numeric(mle))
}


suspectacumprec<-function(datos,limit=2000,tolerance=10){
  # OBJETIVE: detect values above limit preceded by a number of "non precip days", given by tolerance
  # pretende la detección de precipitación acumulada
  # INPUT: 
  # datos: a two columns vector, date and data, in ECA&D format
  # limit: el valor de corte para estudiar la precipitacion
  # tolerance: cuantos días consecutivos con 0 o NA necesita para saltar
  # OUTPUT:
  # $chungos: el indice de los días que no pasan el qc
  # $px: la serie rellenada con computecal (es decir, sin huecos)
  datos$year<-as.numeric(substring(datos[,1],1,4))
  datos$month<-as.numeric(substring(datos[,1],5,6))
  datos$day<-as.numeric(substring(datos[,1],7,8))
  datos<-datos[,c(3,4,5,2)]
  
  ni<-tolerance+1
  
  y<-datos
  x<-datos
  fy<-min(x[,1],na.rm=TRUE)
  ly<-max(x[,1],na.rm=TRUE)
  p<-computecal(fy,ly)
  x<-x[,1:4]
  px<-merge(p,x,by.x=c(1,2,3),by.y=c(1,2,3),all.x=TRUE,all.y=TRUE)
  x<-px[,4]
  target<-which(x>=limit)
  ne<-length(target)
  chungo<-0
  if(ne!=0){
    for(i in 1:ne){
      if(target[i]>tolerance){		
        nyu<-target[i]-tolerance
        nyi<-target[i]-1
        k<-sum(x[nyu:nyi],na.rm=TRUE)
        if(k==0){chungo<-c(chungo,nyu:target[i])}
        #			if(k==0){chungo<-c(chungo,target[i])}
      }
    }
  }
  if(length(chungo)>1){
    chungo<-chungo[-1]}
  for(jj in 1:length(chungo)){ 
    busco<-which(y[,1] == px[chungo[jj],1] & y[,2] == px[chungo[jj],2] & y[,3] == px[chungo[jj],3])
    if(jj==1){bisco<-busco}else{bisco<-c(bisco,busco)}
    } # there must be a more elegant way to da this than with a loop ... but works
  
  #Qachtung
  return(bisco) ### need to revert this to real indices, it seems that they're altered by computecal! [CHECK!!!!!!!!! ACHTUUUUNGGGGG!!!!!]
    
}



drywetlong<-function(x,ret=300){
  
# OBJECTIVE: to detect episodes of too many consecutive wet or dry days
# INPUT: 
# x: vector with  values
# ret: pseudo-return period (pareto-based) to compute the maximum tolerable spell
# RETURNS:
# bad: list of positions which do not pass qc
  
  
seco<-which( x< 10) # using the 1 mm threshold as in RClimdex
mojao<-which(x >=10)
nohay<-which(is.na(x))
y<-x
## creating a binary variable
y[seco]<-0  ### dry values set to 0
y[nohay]<-9 ### NAs set to 9
y[mojao]<-1 ### wet days set to 1
nyu<-rle(y)
wetspell<-nyu$lengths[which(nyu$values==1)]
nyi<-potpareto(wetspell)

if(!is.null(nyi)){wetlim<-returnpotpareto(nyi,ret)}else{wetlim<-999999999999999}
dryspell<-nyu$lengths[which(nyu$values==0)]
nyi<-potpareto(dryspell)
if(!is.null(nyi)){drylim<-returnpotpareto(nyi,ret)}else{drylim<-999999999999999}

wetchungo<-which(nyu$lengths > wetlim & nyu$values == 1)
drychungo<-which(nyu$lengths > drylim & nyu$values == 0)


chungos<-c(wetchungo,drychungo)
if(length(chungos) !=0 ){
ene<-length(chungos)
for(i in 1:ene){
  start<-sum(nyu$lengths[1:chungos[i]]) 
  end<-start+nyu$lengths[chungos[i]]-1
  if(i==1){todo<-c(start:end)}else{todo<-c(todo,start:end)}
}

return(todo)
}

}










txtn<-function(y,id,home){
  
  # Objective: compares tx an tn. First looks for the closest station and then merges both dataframes
  # INPUT: 
  # y: two columns with date and data
  # id: the id we are working with
  # home: home folder, need to add "raw" inside the fucntion
  # RETURNS:
  # bad: list of positions which do not pass qc
  
  

  whoami<-substring(id,1,2);if(whoami=='TX'){targetvariable='TN'}else{targetvariable='TX'}
  cerca<-closetxtn(id,targetvar = targetvariable, home)
  if(!is.null(cerca)){
  name<-paste(home,'raw/',cerca,sep='')
  x<-readecad(name);x<-x[,3:4]
  if(whoami == 'TX'){names(x)<-c('date','tn');names(y)<-c('date','tx')}
  if(whoami == 'TN'){names(y)<-c('date','tn');names(x)<-c('date','tx')}
  z<-merge(x,y,all.y=TRUE,all.x=FALSE)
  bad<-which(z$tx <= z$tn)
  } else{
    bad<-NULL #### this section ensure that the could will not break if there is no suitable station
  }
}



toomany<-function(y,blockmany=15,scope=1,exclude=NULL){
  # Objective: splits data by month and looks if a  value is repeated too many times
  # INPUT: 
  # y: two columns with date and data
  # blockmany: the maximum number of repeated values in a month, year, or season
  # scope: monthly (1), annual (2)
  # exclude: values to exclude, e.g. if precip, 0 must be excluded
  # RETURNS:
  # bad: list of positions which do not pass qc
  
  
 bad<-NULL 
 
 
 if(scope == 1){y[,1]<-as.numeric(substring(y[,1],1,6))}
 if(scope == 2){y[,1]<-as.numeric(substring(y[,1],1,4))}
 nyu<-as.data.frame(table(y[,1],y[,2])) ### this is good, it can identify those which are over the blocksize, but need to now how to extract/label the values
 if(!is.null(exclude)){target<-which(nyu[,3] >= blockmany & nyu[,2] %nin% exclude)}
 else{target<-which(nyu[,3] >= blockmany)}
 ### notice I am using the Hmisc operator %nin%, as !%in% does not exist as negation of the base %in%


 ene<-length(target)
 if(ene > 0){
   for(i in 1:ene){
     tirget<-nyu[target[i],]   
     fecha<-tirget[,1]
     valor<-tirget[,2]
        wanted<-which(y[,1] == fecha & y[,2] == valor)
        if(i == 1){bad<-wanted}else{bad<-c(bad,wanted)}
   }  
 }
 bad<-unique(bad)
 return(bad)
}



rounding<-function(y,blocksize=20){
  # Objective: splits data by month and looks if a decimal value is repeated too many times
  # INPUT: 
  # y: two columns with date and data
  # blocksize: the maximum number of repeated values
  # RETURNS:
  # bad: list of positions which do not pass qc
  
  bad<-NULL 
  y[,1]<-as.numeric(substring(y[,1],1,6))
  y[,2]<-y[,2]/10;y[,2]<-y[,2]-floor(y[,2])
  
  nyu<-as.data.frame(table(y[,1],y[,2])) ### this is good, it can identify those which are over the blocksize, but need to now how to extract/label the values
  target<-which(nyu[,3] >= blocksize)
  ene<-length(target)
  if(ene > 0){
    for(i in 1:ene){
      tirget<-nyu[target[i],]   
      fecha<-tirget[,1]
      valor<-tirget[,2]
      wanted<-which(y[,1] == fecha & y[,2] == valor)
      if(i == 1){bad<-wanted}else{bad<-c(bad,wanted)}
    }  
  }
  bad<-unique(bad)
  return(bad)
}










physics<-function(x,nyu=0,compare=1){
  # OBJECTIVE: given a data vector, will compare the values to the specified threshold
  # INPUT: 
  # @nyu : threshold, numeric 
  # recommended levels: 
  # TX 55 / -40 ; TN 35 / -70 ; RR 1000 / 0 ; DD 0/360 
  # @compare : logical operation to apply over the threshold
  # 1 larger; 2 larger equal; 3 smaller; 4 smaller equal; 5 equal
  # OUTPUT: 
  # @bad: list of positions not passing the test
  # Date: 7/12/2017
  if(compare==1){bad<-which(x > nyu)}
  if(compare==2){bad<-which(x >= nyu)}
  if(compare==3){bad<-which(x < nyu)}
  if(compare==4){bad<-which(x <= nyu)}
  if(compare==4){bad<-which(x == nyu)}
  return(bad)
}

duplas<-function(x){
  # OBJECTIVE: detects duplicated dates
  # PARAMETERS:
  # x: a vector of dates in ECA&D format (YYYMMDD)
  # RETURNS: 
  # total: a list of positions which do not pass QC
  
  antes<- which(duplicated(x,fromLast=TRUE)==TRUE)
  despues<- which(duplicated(x,fromLast=FALSE)==TRUE)
  if(length(antes!=0)){
  }
  total<-c(antes,despues)
  return(total)
}

putjulian<-function(x){
  # OBJECTIVE: merge julian days to a yyyy,mm,dd and data
  # INPUT:
  # x: nxn with year,month, day and data columns
  # OUTPputjulianUT:
  # y: nxn+1 columns with year, month, day, julian and data 
  mes<-c(31,29,31,30,31,30,31,31,30,31,30,31)
  cumes<-cumsum(mes)
  cumes<-c(0,cumes)
  p<-cumes[x[,2]]+x[,3]
  colnum<-ncol(x)
  x<-cbind(x[,1:3],p,x[,4:colnum])
  return(x)
}

jumps = function(x,maxjump) {
  # OBJETIVO: to label interdiurnal differences considered to large
  # INPUT: 
  # x: vector of values
  # maxjump: max difference allowed
  
      diffy<-diff(x)
      chungo<-which(abs(diffy)>maxjump)
      chungo<-unique(sort(c(chungo,chungo+1)))

    return(chungo)
  
}


flat<-function(y,maxseq){
  # OBJECTIVE: detects consecutive equal values. Can be adapted to detect consecutive equal decimal part of the values
  # PAREMETERS: 
  # y: a data vector
  # maxseq: the maximum number of contiguous repetitions of a value (e.g., if 3, sequences of 4 will be flagged)
  # RETURN: 
  # target: list of positions which do not pass qc
  
  
  mu<-rle(y)$lengths # sequences
  cumu<-cumsum(mu) # cumulative sum to find real posistion of a sequence
  longbad<-mu[which(mu>maxseq)] # length of each sequence
  # make resistant to nullbad
  if(length(longbad)!=0){
  bad<-cumu[which(mu>maxseq)] # last point of a sequence
  firstbad=(bad-longbad)+1 # first point of a sequence
  ene<-length(firstbad)
  for(i in 1:ene){
    if(i==1){target<-firstbad[i]:bad[i]}else{target<-c(target,firstbad[i]:bad[i])}
    
  }
return(target)}
}




badfriki<-function(date,value,margina=80){
  
  # OBJECTIVE: isolates values which are not continuous in the  distribution. If the gap is larger than a preset big margin, 
  #		the value is flagged
  # INPUT: 
  #@date: a vector with yyyymmdd
  #@values: a vector wih values 
  #@margina: the tolerence margin 
  
  # OUTPUT:
   
   y<-data.frame(date,value)
  for(ij in 1:12){
  position<-which(as.numeric(substring(y$date,5,6))==ij) # find the positions for the month ij
  mes<-y[position,]  # subset the positions
  sorted<-sort(mes$value)  # sort the values
  diffy<-(diff(sorted)) # create the first difference of the sorted values
  malo<-which(diffy > margina) # isolate the values 
  clint<-which(malo > length(diffy)/2);malo[clint]<-malo[clint]+1 # when the position is in the right tail, it is further away of the median, 
                                                                  # the bad value is the most exterior
  chungo<-sorted[malo] # isolate the values 
  datechungo<-y$date[which(y$value %in% chungo)] 
  if(exists('fechas')){fechas<-c(fechas,datechungo)}else{fechas<-datechungo}
  }  
  bad<-which(date%in% fechas)  
  return(bad)
}


computecal=function(fy,ly){
  # OBJECTIVE:prepares a calendar frame and returns it as year,month,day (i.e., the 3 first columns of RClimdex format
  # PARAMETERS:
  # fy: first year to work with (past)
  # ly: last year to work with (present)
  # RETURNS:
  # calframe: 3 columns containing year,month, day
  
  fy<-paste(fy,1,1,sep='/')
  ly<-paste(ly,12,31,sep='/')
  pepe<-seq(as.Date(fy),as.Date(ly),"days")
  jose<-as.numeric(substring(pepe,1,4))
  lito<-as.numeric(substring(pepe,6,7))
  lote<-as.numeric(substring(pepe,9,10))
  calframe<-cbind(jose,lito,lote)
  return(calframe)
}


IQRoutliers<-function(date,value,level=3,window=11){
  
  # OBJECTIVE:computes outliers centralized around a day, using a number of days around it
  # PARAMETERS:
  # date: vector with dates
  # values: vector with values
  # level: number of IQRs
  # window: number of days to be considered (including the target)
  # RETURNS:
  # bad: positions which do not pass this test
  
  
  
  
  year<-as.numeric(substring(date,1,4))
  month<-as.numeric(substring(date,5,6))
  day<-as.numeric(substring(date,7,8))
  modulo<-(window-1)/2
  y<-data.frame(year,month,day,date,value)
  y<-putjulian(y)
  y$qc<-0
  julios<-1:366
  luego<-1:modulo
  antes<-(366-modulo+1):366
  julios<-c(antes,julios,luego)
  for(j in (modulo+1):(modulo+366)){
    target<-julios[(j-5):(j+5)]
    valores<-y$value[y$p %in% target] # hasta consigue seleccionar valores centrados en el dia juliano definido por j
    valores<-y[y$p %in% target,] # hasta consigue seleccionar valores centrados en el dia juliano definido por j
    p75<-quantile(valores$value,0.75,na.rm=TRUE)
    p25<-quantile(valores$value,0.25,na.rm=TRUE)
    iqr<-IQR(valores$value,na.rm=TRUE)
    above<-p75+level*iqr
    below<-p25-level*iqr
    bud<-which(valores$value > above | valores$value < below)
    if(length(bud != 0 )){
      if(exists('fecha')){fecha<-c(fecha,valores$date[bud])}else{fecha<-valores$date[bud]}
    }
  }
  if(exists('fecha')){bad<-which(y$date %in% fecha)
  return(bad)
  }
}














########### UTILS ###


closetxtn<-function(station='TX_SOUID136678.txt',targetvar='TN',home){
  
  ## OBJECTIVE: looks the closest tx station for a tn station and vice versa 
  ## $station: a SOUID station, as the default
  ## $targetvar: the target var for which we want the closest neighbor.
  ## $home, to pass it on to listas()
  ## $filtertxtn: if set to TRUE, liston will be cut to tx and tn 
  
  ## OUTPUT
  ## $closest: the name of the closest station and the associated distance
  ## WARNING: depends on listas() to work with the default options  
  
  
  
  if(!exists("liston")){#### this takes quite a lot of time, and better if ran only once. Then, the list is declared as a global variable
    liston<-listas(rooty=home)
    lat<-apply(as.data.frame(liston$LAT),1,FUN=decimaldegrees)  
    lon<-apply(as.data.frame(liston$LON),1,FUN=decimaldegrees) 
    coordinates<-data.frame(lat,lon) 
    liston$LAT<-lat
    liston$LON<-lon

    assign("liston",liston,envir = .GlobalEnv)
    
  }
  
  
  mylist<-unique(liston[,2:ncol(liston)]) #### getting rid of duplicates
  souid<-substring(station,9,14);target<-which(mylist$SOUID==souid)
  reference<-mylist[target,] ### this is the station we're working with
  
  
  if(nrow(reference)==0){return(NULL)} 
  if(nrow(reference) !=0){
  mylist<-mylist[which(substring(mylist$ELEI,1,2) == targetvar),] ## subseting the list to have only the stations of the target variable
  ### lets put an additional filter: only stations in a radius of 1 degrees 
  mylist$diflat<-(reference$LON-mylist$LON)**2
  mylist$diflon<-(reference$LON-mylist$LON)**2
  mylist$EUCLIDIAN<-sqrt((reference$LAT-mylist$LAT)**2+(reference$LON-mylist$LON)**2)
  mylist<-mylist[which(mylist$EUCLIDIAN < 0.5),] ### first limit
  if(nrow(mylist) == 0 ){return(NULL)}
  mylist$DISTHAVER<-distHaversine(p2=cbind(mylist$LON,mylist$LAT),p1=cbind(reference$LON,reference$LAT))
  # Apparently, no need to use apply. Apply was collapsing when only one station was prefiltered. 
  #mylist$DISTHAVER<-apply(mylist,1,function(x) distHaversine(p2=cbind(mylist$LON,mylist$LAT),p1=cbind(reference$LON,reference$LAT)))[,1] ## computing distances
  mylist<-mylist[which(mylist$DISTHAVER < 10000),] ### this limits the data search to a radius 10K
   # theoretical overlap preparation [this is good if lists are updated, which is not the case]; the larger, the better
 # computed but not used.   
  if(nrow(mylist) == 0 ){return(NULL)}
  mylist$DATE1<-as.Date(as.character(mylist$START),format="%Y%m%d") 
  mylist$REFDATE1<-as.Date(as.character(reference$START),format="%Y%m%d")
  mylist$DATE2<-as.Date(as.character(mylist$STOP),format="%Y%m%d") 
  mylist$REFDATE2<-as.Date(as.character(reference$STOP),format="%Y%m%d")
  mylist$MAXSTART<-as.Date(apply(cbind(mylist$DATE1,mylist$REFDATE1),1,max))
  mylist$MINEND<-as.Date(apply(cbind(mylist$DATE2,mylist$REFDATE2),1,min))
  mylist$OVERLAP<-as.numeric(mylist$MINEND-mylist$MAXSTART)
  
  ### check it is the corresponding variable variable 
  varcode<-as.numeric(substring(reference$ELEI,3,3))
  mylist$ISELEMENT<-as.numeric(substring(reference$ELEI,3,3))-as.numeric(substring(mylist$ELEI,3,3))
  
  ## height (value to compute 3D euclidian)
  mylist$DROP<-mylist$HGTH-reference$HGTH
  
  
  
  #### id matching implementation; the smaller, the better
  mylist$IDDIST<-as.numeric(adist(reference$SOUNAME,mylist$SOUNAME))
  mylist$DIFID<-abs(mylist$SOUID-reference$SOUID)

  
  
  
  perfect<-mylist[mylist$DISTHAVER == min(mylist$DISTHAVER),] ## FIRST,choosing the closest 
  # NOTE: removed element check; need to value if this is a requirement, e.g., if are different computations 
  
  if(length(perfect)==0){return(NULL)} ### this means that there is no good station
  if(length(perfect)> 1){perfect<-perfect[which(perfect$DROP == min(perfect$DROP)),]} # TIE BREAKER 1: the ID should be close (removes GTS stations)
  if(length(perfect)> 1){perfect<-perfect[which(perfect$DIFID == min(perfect$DIFID)),]} # TIE BREAKER 1: the ID should be close (removes GTS stations)
  if(length(perfect)> 1){perfect<-perfect[which(perfect$OVERLAP == max(perfect$OVERLAP)),]} # TIE BREAKER 2: overlap, good for stations made of segments. 
  if(length(perfect)> 1){perfect<-perfect[which(perfect$IDDIST == min(perfect$IDDIST)),]}
  perfect<-perfect$SOUID
  closest<-sprintf('%s_SOUID%06d.txt', targetvar, as.integer(perfect)) # composes the name Thanks Jose Guijarro for the format tip!
  filclose<-paste0(home,'raw/',closest)
  if(file.exists(filclose)){return(closest)}else{return(NULL)}
  }
  }
  

listas<-function(rooty='../Sweden/',country='all',name='allstations.txt'){ #### NECESITO parametrizar listas. Usar esa parametrizacion par subset de downloads too. 
  # OBJECTIVE:  create listings for stations linking STAID and SOUID
  # it takes all the elements and rbinds them. 
  # INPUT: 
  # $rooty: home directory where the "ECA_blend_source*" files are located
  # $name: output name, do not touch, default is always good
  # $country: country for which the list is created. If all, no country filter. 
  # OUTPUT: a data.frame containing all the stations for all elements, linking STAID and SOUID and metadata
  # 
  # a file named as specified by name
  
  variables<- c('TX','TN','TG','RR','HU','PP','SS','FG', 'FX', 'DD','SD', 'CC') 
  ene<-length(variables)
  missing= -9999
  
  for(i in 1:ene){
    list<-paste(rooty,'ECA_blend_source_',tolower(variables[i]),'.txt',sep='')
    if(file.exists(list)){
    x<-read.fwf(list,widths=15)
    nyu<-grep('STAID,',x[,1])
    nkono=c(5,1,6,1,40,1,2,1,9,1,10,1,4,1,4,1,8,1,8,1,5,1,51)
    theocusters=c(5,6,40,2,9,10,4,4,8,8,5,51)
    x<-read.fwf(list,na.strings=missing,widths=nkono,skip=nyu+1,stringsAsFactors=FALSE,strip.white = TRUE)
    x<-x[,c(1,3,5,7,9,11,13,15,17,19,21,23)]
    names(x)<-c('STAID','SOUID','SOUNAME','CN','LAT','LON','HGTH','ELEI','START','STOP','PARID','PARNAME')
    if(i==1){todas<-x}else{todas<-rbind(todas,x)}
   
    }
  }
  #todas<-todas[order(todas[,c(1,2)]),]
  if(country!='all'){target<-which(todas$CN == country);todas<-todas[target,]}
  #tidos<-rbind(names(x),todas) ### this seems to be inncessary, as duplicates the header. Commented
  #write.fwf(tidos,paste(rooty,name,sep=''),sep=' ',colnames=FALSE,rownames=FALSE,quote=FALSE,width=theocusters) ## as a consecuence of the previous comment
  write.fwf(todas,paste(rooty,name,sep=''),sep=' ',colnames=FALSE,rownames=FALSE,quote=FALSE,width=theocusters) ## as consecuence of the previous action
    return(todas)
}



decimaldegrees <- function(dms,sep = ":") {
  # OJECTIVE: takes sexagesimal degrees in ECA&D format and converts them into decimal degrees. 
  # Initial idea taken from :https://modtools.wordpress.com/2013/09/25/dms2dec/
  # dms: ONE ELEMENT from the LAT or LON field in ECA&D listings
  # sep: the separator between elements, in ECA&D ":"
  deg <- as.numeric(unlist(strsplit(dms, split = sep))[1])
  min <- as.numeric(unlist(strsplit(dms, split = sep))[2])
  sec <- as.numeric(unlist(strsplit(dms, split = sep))[3])
  
  if(deg<0){negative=TRUE}else{negative=FALSE}
  dec <- as.numeric(deg) + (as.numeric(min) / 60) + (as.numeric(sec) / 3600)
  if(negative){dec <- -1 * dec}
  return(dec)
}  


readecad<-function(input="SS_STAID000143.txt",missing= -9999){
  # OBJECTIVE:  reads one eca&d file and puts it in yyyy/mm/dd/value. Data is NOT divided by 10, to transform it into true units
  # INPUT: 
  # input: the filename
  # missing: missing value code, set to the default eca&d mvc
  # OUTPUT: 
  # x: a series with yyyy/mm/dd/value format: n rows; 4 columns
  x<-read.fwf(input,widths=15)
  nyu<-grep('STAID,',x[,1])
  x<-read.table(input,na.strings=missing,skip=nyu+1,sep=',',header=FALSE,stringsAsFactors=FALSE)
  #datecol=3
  #parcol=datecol+1
  #year<-(as.numeric(substring(x[,datecol],1,4)))
  #month<-(as.numeric(substring(x[,datecol],5,6)))
  #day<-(as.numeric(substring(x[,datecol],7,8)))
  #value<-x[,parcol]
  #x<-cbind(any,mes,dia,round(x[,parcol]/10,1))
  #x<-cbind(year,month,day,value)# not diveded, not rounded. 
  return(x)
}









### DEPRECATED

# REPLACED by toomany!
blocksmonthly<-function(y,blocksize=10){ 
  # Objective: splits data by month and looks if a single value is repeated a number of times (blocksize)
  # INPUT: 
  # y: two columns with date and data
  # blocksize: the maximum number of repeated values
  # RETURNS:
  # bad: list of positions which do not pass qc
  
  bad<-NULL 
  y[,1]<-as.numeric(substring(y[,1],1,6))
  nyu<-as.data.frame(table(y[,1],y[,2])) ### this is good, it can identify those which are over the blocksize, but need to now how to extract/label the values
  target<-which(nyu[,3] >= blocksize)
  ene<-length(target)
  if(ene > 0){
    for(i in 1:ene){
      tirget<-nyu[target[i],]   
      fecha<-tirget[,1]
      valor<-tirget[,2]
      wanted<-which(y[,1] == fecha & y[,2] == valor)
      if(i == 1){bad<-wanted}else{bad<-c(bad,wanted)}
    }  
  }
  bad<-unique(bad)
  return(bad)
}





# NOT USED: IT IS TOO SLOW!!!
blocks<-function(y,blocksize=10,step=30){####
  # Objective: detects if in a segment of "step" there are "blocksize" equal values, then flags all the
  # the values in the segment which represent the higest frequency
  # INPUT: 
  # y: a numeric vector
  # blocksize: the maximum number of repeated values
  # step: segment size, if =30, it takes blocks fof 30 values 
  # RETURNS:
  # bad: list of positions which do not pass qc
  
  ene<-length(y)-step+1
  count=0
  bad<-NULL 
  for(i in 1:ene){
    
    target<-i:(i+step-1) # the positions
    criterio<-max(table(y[target])) # the frequency of the most repeated value; 
    positron<-as.numeric(names(which.max(table(y[target])))) # the value which is most repeated
    if(criterio>blocksize){
      malos<-i-1+which(y[target]==positron)
      count=count+1;if(count==1){bad<-malos}else{bad<-c(bad,malos)}
    }  
  }
  bad<-unique(bad)
  return(bad)
}


### new in beta_v2.0


readheader<-function(input="SS_STAID000143.txt"){
  # OBJECTIVE:  reads one eca&d file and returns the header so it can be written in the same way
  # INPUT: 
  # input: the filename
  # missing: missing value code, set to the default eca&d mvc
  # OUTPUT: 
  # x: a series with yyyy/mm/dd/value format: n rows; 4 columns
  x<-read.fwf(input,widths=300,stringsAsFactors=FALSE)
  nyu<-grep('STAID,',x[,1])
  return(x[1:nyu,1])
}






###### NOT USED,but retained for future implementation

patternal<-function(v,patlength=30){
  print(Sys.time())
  nota<-which(is.na(v))
  v<-v[-nota]
  ene<-length(v)-patlength+1  
  for(j in 1:ene){
    x<-v[j:(j+patlength-1)]
    idx <- which(v == x[1])
    nyu<-idx[sapply(idx, function(i) all(v[i:(i+(length(x)-1))] == x))]
    if(length(nyu)!=1){browser()}    
  }
  print(Sys.time())
}



pattern<-function(v,patlength=30){
  print(Sys.time())
  nota<-which(is.na(v))
  v[nota]<- -9999
  ene<-length(v)-patlength+1  
  print(ene)
  #browser()
  for(j in 1:ene){
    #print(j)
    #if(j == 10410){browser()}
    x<-v[j:(j+patlength-1)]
    frank(v,x)
    #symbOpt(v,x)
  }
  print(Sys.time())
}




frank <- function(v,x) {
  l <- length(x);
  w = seq_along(v);
  for (i in seq_along(x)) w = w[v[w+i-1L] == x[i]];
  rep(w, each = l) + 0:(l-1)
}

