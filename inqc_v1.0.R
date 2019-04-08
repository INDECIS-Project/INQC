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
## 2.- Made drywetlong() resistant to precip series with almost no data (RR_SOUID102169.txt has only ONE valid value, the rest is NA!!!)
## 3.- roundprecip() made ressistant to series with no data different than 0. Returns NULL 
## 4.- potpareto() made ressistant to cases were all values = 0 (e.g, RR_SOUID107781.txt, with < 800 values, and all them 0)
## 5.- drywetlong(): there was a mistake in rle() usage. Example: rle$values 1 2 2 5, it was interpreted as if the srike labelled with "5" was starting in position 10, when actually
##     runs from 6 to 10!!!. 
## 6.- Corrected error in bad<-flat(x$value%%10,roundmax) for some variables: roundmax was not parametrized


## Updates February, 2019 
## 1.- Variable calls made ressistant to the existance of 0 series (i.e.: if TG does not exist, the call to temperature(element='TG' 
##    should not break the code anymore))
## 2.- sundur() had a parameter omited (roundmax) which was causing the code to break. Solved


## Updates March, 2019
## 1.- Function flat allows to exlude sequenes for a value, i.e., excluding 0, will allow to work flats for precip
## 2.- CORRECTED ERROR in selepe: results of badfriki were assigned to $flat in the log files. 
## 3.- Corrected error in selepe parametrization, unadvertedly, it was -8000 instead of 8000
## 4.- Defaulting retornoracha = 500 instead of = 1000 in precip (fine tuning with benchmark, need to evaluate false positives)
## 5.- Corrected error in the statement : rm(liston), which is executed at the end of the code. It has to be rm(liston,envir=GlobalEnv)
##     This error was preventing the generation of new station lists and using previous ones. This caused txtn to not perform as expected  
## 6.- Corrected problems with closetxtn. Problems that mostly affected Baboon data, due to its peculiarities. Changes should not affect real data
## if this is not the case, revert to a february version
## 7.- Added function sunafterdark: it compares susnhine duration to the maximum possible according to calendar and position
## 8.- Modified IQR outliers function to exlude values (e.g., 0 for preciptiation)
## 9.- Added IQRoutliers to the precip routine, with very wide limits to avoid flagging excessive number of values
## 10.- Flaggs for the consolidated file are being reshaped: 
## 11.- Correcting error: consolidator() was only being applied to temperature!
## 12.- Corrected error in weirddate that was flagging as bad the 29th of February of leap years
## 13.- listas() made resistant to small format changes in the input files (e.g. no header or different widhts than the "official" specification)
## 14.- Evaluation of pressure results denoted that badfriki was not working well. Replaced by newfriki for pressure and, most likely, will be deprecated for other variables too
## 15.- jumps2 replaces jumps. Can be ran in two modes: split by month and with thresholds computed from the distro of the differences or with an 
##      absolute threshold. In both cases, jumps2 does not flag BOTH elements in a jump, only the one which is most likely the culprit
## 16.- Output in QCConsolidated, adds reference to this software and is "true" ECA&D format, with corrected widhts and comma-separated
## 17- Changes to drywetlong: sueco: threshold for dividing dry and wet. This is useful to label other binary sequences, e.g. for 0 radiation. 
##    Now it is <= and >, instead of < and >=
## 18.- txtn improved: 
##  a) closetxtn is not used anymore (too slow) and the pair is located by staid, eleid and parid. If sticking to ECA&D, safe enough. 
##  b) once values are flagged as "tx <= tn", we look if any value in the pair is central and the other extreme to their monthly distrons. Then, only the extreme is flagged
## 19.- Added inithome(). Creates all the folders and updates stations list
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
if(!require(utils)){install.packages('utils')}
if(!require(suncalc)){install.packages('suncalc')}


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
require(suncalc)





inqc<-function(homefolder='../Sweden_benchmark_test1/'){

# OBJECTIVE: wraper for QC'ing all varibales
# PARAMETERS: 
# homefolder: the path to the homefolder, as string
# RETURNS: the QC results, in both formats (verbose and workable file in exact ECA&D format)  
  
if(!dir.exists(paste0(homefolder,'QC'))){dir.create(paste0(homefolder,'QC'))}
if(!dir.exists(paste0(homefolder,'QCConsolidated'))){dir.create(paste0(homefolder,'QCConsolidated'))}

  
  
listonator(casa=homefolder)  
temperature(home=homefolder,element='TX')  
temperature(home=homefolder,element='TN')
temperature(home=homefolder,element='TG')
precip(home=homefolder)
relhum(home=homefolder)
selepe(home=homefolder)
snowdepth(home=homefolder)
sundur(home=homefolder)  
windspeed(home=homefolder)
clocov(home=homefolder)
rm(liston,envir=.GlobalEnv)
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

if(!dir.exists(paste0(home,'QCConsolidated/'))){dir.create(paste0(home,'QCConsolidated/'))}
if(!dir.exists(paste0(home,'QCSummary/'))){dir.create(paste0(home,'QCSummary/'))}
if(!dir.exists(paste0(home,'QC/'))){dir.create(paste0(home,'QC/'))}
  
# 1: ERROR
# 2: ALMOST CERTAIN, ERROR
# 3: OUTLIER, SUSPECT
# 4: COLLECTIVELY SUSPECT  
  
header<-readheader(paste0(home,'raw/',filename))
element<-substring(filename,1,2)
stationsummary<-apply(x[,5:ncol(x)],2,sum)
failed<-as.data.frame(stationsummary[which(stationsummary!=0)])
write.table(failed,paste0(home,'/QCSummary/Summary',filename),quote=FALSE,sep='\t',col.names=FALSE)

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

# almost certain, error (2)
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
write.table(resumen,paste0(home,'/QCSummary/Summary',filename),quote=FALSE,sep='\t',append=TRUE,col.names=TRUE,row.names=FALSE)

anchos<-c(6,6,8,5,5)
grannyu<-ncol(x)
x<-x[,c(1:4,grannyu)]
write.table('Quality control with INQC V1.0, INDECIS Project, C3/URV',paste0(home,'QCConsolidated/',filename),quote=FALSE,col.names=FALSE,row.names=FALSE,sep=',',na='')
write.table(header,paste0(home,'QCConsolidated/',filename),quote=FALSE,col.names=FALSE,row.names=FALSE,sep=',',na='',append=TRUE)
write.fwf(x,paste0(home,'QCConsolidated/',filename),colnames=FALSE,rownames=FALSE,sep=',',quote=FALSE,append=TRUE,na='-9999',width=anchos)
print(paste(Sys.time(),'Wrote QCd file'),quote=FALSE)
}

inithome<-function(home){
if(!dir.exists(paste0(home,'QC'))){dir.create(paste0(home,'QC)'))}
if(!dir.exists(paste0(home,'QCSummary'))){dir.create(paste0(home,'QCSummary)'))}
if(!dir.exists(paste0(home,'QCConsolidated'))){dir.create(paste0(home,'QCConsolidated)'))}
rm(liston,envir=.GlobalEnv)  
listonator(home)
}

# Data is not organized in folders 

temperature<-function(home='../Sweden/',large=500,small=-500,maxjump=200,maxseq=3,margina=0.999,
               level=4,window=11,roundmax=10,blocksize=10,step=30,blockmanymonth=15,blockmanyyear=180,
               blocksizeround=20,qjump=0.999,tjump=1.5,
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
  # margina: quantile for newfriki
  # maxjump: forcing for jump2 in absolute mode (in the same units of the variable)
  # qjump: quantile for jump2 in quantile mode
  # tjump: times the quantile is mutiplied for ju,p2
  
  inithome(home)
  lista<-list.files(path=paste(home,'raw',sep=''),pattern='SOUID')

  tx<-lista[which(substring(lista,1,2)==element)];ene<-length(tx)
  if(length(tx)==0){return()}
    
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
    bad<-jumps2(x$date,x$value,force=maxjump);x$jumpABS<-0;if(length(bad)!=0){x$jumpABS[bad]<-1}; print(paste(Sys.time(),'Ended jumps ABSOLUTE'),quote=FALSE)
    bad<-jumps2(x$date,x$value,qjump,tjump);x$jumpQUANT<-0;if(length(bad)!=0){x$jumpQUANT[bad]<-1}; print(paste(Sys.time(),'Ended jumps QUANTILE'),quote=FALSE)
    bad<-flat(x$value,maxseq);x$flat<-0;if(length(bad)!=0){x$flat[bad]<-1}; print(paste(Sys.time(),'Ended flat for values'),quote=FALSE)
    bad<-flat(x$value%%10,roundmax);x$roundmax<-0;if(length(bad)!=0){x$roundmax[bad]<-1}; print(paste(Sys.time(),'Ended flat for decimal part'),quote=FALSE)
    bad<-newfriki(x$date,x$value,margina,times=3);x$friki<-0;if(length(bad)!=0){x$friki[bad]<-1} ; print(paste(Sys.time(),'Ended newfriki for errors'),quote=FALSE)
    bad<-newfriki(x$date,x$value,margina,times=1.5);x$frikilight<-0;if(length(bad)!=0){x$frikilight[bad]<-1} ; print(paste(Sys.time(),'Ended newfriki for suspect'),quote=FALSE)
    bad<-IQRoutliers(x$date,x$value,level,window);x$IQRoutliers<-0;if(length(bad)!=0){x$IQRoutliers[bad]<-1}; print(paste(Sys.time(),'Ended IQR outliers'),quote=FALSE)
    bad<-toomany(x[,3:4],blockmanymonth,1);x$toomanymonth<-0;if(length(bad)!=0){x$toomanymonth[bad]<-1}; print(paste(Sys.time(),'Ended toomany, monthly'),quote=FALSE)
    bad<-toomany(x[,3:4],blockmanyyear,2);x$toomanyyear<-0;if(length(bad)!=0){x$toomanyyear[bad]<-1}; print(paste(Sys.time(),'Ended toomany, annual'),quote=FALSE)
    bad<-rounding(x[,3:4],blocksizeround);x$rounding<-0;if(length(bad)!=0){x$rounding[bad]<-1}; print(paste(Sys.time(),'Ended rounding'),quote=FALSE)
    if(element != 'TG'){bad<-txtn(x[,3:4],tx[i],home);x$txtn<-0;if(length(bad)!=0){x$txtn[bad]<-1}; print(paste(Sys.time(),'Ended txtn'),quote=FALSE)}
    
    consolidator(home,tx[i],x)
    write.table(x,paste(home,'QC/qc_',tx[i],sep=''),col.names=TRUE,row.names=FALSE,sep='\t',quote=FALSE); print(paste(Sys.time(),'Wrote QC results'),quote=FALSE)
    
      }
  
}



selepe<-function(home='../Sweden/',large=15000,small=8000,maxjump=2000,maxseq=3,margina=0.999,
               level=5,window=30,roundmax=10,blocksize=10,step=30,blockmanymonth=15,blockmanyyear=180,
               blocksizeround=20,qjump=0.999,tjump=1.5,
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
  # level: number of IQRs for IQR outliers
  # window: window, in days, for IQR outliers
  # qjump: quantile for the calculated maximum jump allowed
  # tjump: factor to multiply the qjump computed differences
  # margina: quantile for newfriki function  
  lista<-list.files(path=paste(home,'raw',sep=''),pattern='SOUID')
  
  tx<-lista[which(substring(lista,1,2)==element)];ene<-length(tx)
  if(length(tx)==0){return()}
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
    consolidator(home,tx[i],x)
    write.table(x,paste(home,'QC/qc_',tx[i],sep=''),col.names=TRUE,row.names=FALSE,sep='\t',quote=FALSE); print(paste(Sys.time(),'Wrote QC results'),quote=FALSE)
  }
  
}



relhum<-function(home='../Sweden/', element='HU',maxseq=3,blocksizeround=20,blockmanymonth=15,blockmanyyear=180,roundmax=10){
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
  # roundmax: maximum number of consecutive decimal part value, for flat function (10.0, 11.0, 12.0 would be 3 consecutive)
  
  lista<-list.files(path=paste(home,'raw',sep=''),pattern='SOUID')
  
  tx<-lista[which(substring(lista,1,2)==element)];ene<-length(tx)
  if(length(tx)==0){return()}
  for(i in 1:ene){
    name<-paste(home,'raw/',tx[i],sep='')
    print(paste(name,i,'of',ene),quote=FALSE)
    x<-readecad(input=name) ; print(Sys.time())
    x<-x[,1:4];colnames(x)<-c('STAID','SOUID','date','value')
    bad<-duplas(x$date);x$dupli<-0;if(length(bad)!=0){x$dupli[bad]<-1} ; print(paste(Sys.time(),'Ended duplas'),quote=FALSE)
    bad<-weirddate(x[,3:4]);x$weirddate<-0;if(length(bad)!=0){x$weirddate[bad]<-1}; print(paste(Sys.time(),'Ended weirddate'),quote=FALSE)
    bad<-toomany(x[,3:4],blockmanymonth,1,exclude=100);x$toomanymonth<-0;if(length(bad)!=0){x$toomanymonth[bad]<-1}; print(paste(Sys.time(),'Ended toomany, month'),quote=FALSE)
    bad<-toomany(x[,3:4],blockmanyyear,2,exclude=100);x$toomanyyear<-0;if(length(bad)!=0){x$toomanyyear[bad]<-1}; print(paste(Sys.time(),'Ended toomany, year'),quote=FALSE)
    bad<-flat(x$value,maxseq,exclude=100);x$flat<-0;if(length(bad)!=0){x$flat[bad]<-1}; print(paste(Sys.time(),'Ended flat for values'),quote=FALSE)## this looks at consecutive equal values
    bad<-flat(x$value%%10,roundmax,exclude=100);x$roundmax<-0;if(length(bad)!=0){x$roundmax[bad]<-1}; print(paste(Sys.time(),'Ended flat for decimal part'),quote=FALSE) #this looks at consecutive decimal parts
    bad<-rounding(x[,3:4],blocksizeround);x$rounding<-0;if(length(bad)!=0){x$rounding[bad]<-1}; print(paste(Sys.time(),'Ended rounding'),quote=FALSE)
    bad<-physics(x$value,100,1);x$large<-0;if(length(bad)!=0){x$large[bad]<-1}; print(paste(Sys.time(),'Ended physics, large'),quote=FALSE)
    bad<-physics(x$value,0,3);x$small<-0;if(length(bad)!=0){x$small[bad]<-1}; print(paste(Sys.time(),'Ended physics, small'),quote=FALSE)
    consolidator(home,tx[i],x)
    write.table(x,paste(home,'QC/qc_',tx[i],sep=''),col.names=TRUE,row.names=FALSE,sep='\t',quote=FALSE); print(paste(Sys.time(),'Wrote QC results'),quote=FALSE)
  }
  
}


sundur<-function(home='../Sweden/', element='SS',maxseq=3,blocksizeround=20,blockmanymonth=15,blockmanyyear=180,roundmax=10){
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
  # roundmax: maximum number of consecutive decimal part value, for flat function (10.0, 11.0, 12.0 would be 3 consecutive)
  
  
  lista<-list.files(path=paste(home,'raw',sep=''),pattern='SOUID')
  tx<-lista[which(substring(lista,1,2)==element)];ene<-length(tx)
  if(length(tx)==0){return()}
  for(i in 1:ene){
    name<-paste(home,'raw/',tx[i],sep='')
    print(paste(name,i,'of',ene),quote=FALSE)
    x<-readecad(input=name) ; print(Sys.time())
    x<-x[,1:4];colnames(x)<-c('STAID','SOUID','date','value')
    bad<-duplas(x$date);x$dupli<-0;if(length(bad)!=0){x$dupli[bad]<-1} ; print(paste(Sys.time(),'Ended duplas'),quote=FALSE)
    bad<-weirddate(x[,3:4]);x$weirddate<-0;if(length(bad)!=0){x$weirddate[bad]<-1}; print(paste(Sys.time(),'Ended weirddate'),quote=FALSE)
    bad<-toomany(x[,3:4],blockmanymonth,1,exclude=0);x$toomanymonth<-0;if(length(bad)!=0){x$toomanymonth[bad]<-1}; print(paste(Sys.time(),'Ended toomany, month'),quote=FALSE)
    bad<-toomany(x[,3:4],blockmanyyear,2,exclude=0);x$toomanyyear<-0;if(length(bad)!=0){x$toomanyyear[bad]<-1}; print(paste(Sys.time(),'Ended toomany, year'),quote=FALSE)
    bad<-flat(x$value,maxseq);x$flat<-0;if(length(bad)!=0){x$flat[bad]<-1}; print(paste(Sys.time(),'Ended flat for values'),quote=FALSE)
    bad<-flatsun(x[,3:4],maxseq,tx[i],home);x$flatsun<-0;if(length(bad)!=0){x$flatsun[bad]<-1}; print(paste(Sys.time(),'Ended flatsun'),quote=FALSE)
    bad<-flat(x$value%%10,roundmax);x$roundmax<-0;if(length(bad)!=0){x$roundmax[bad]<-1}; print(paste(Sys.time(),'Ended flat for decimal part'),quote=FALSE) #this looks at consecutive decimal parts
    bad<-roundprecip(x[,3:4],blocksizeround,exclude=0);x$rounding<-0;if(length(bad)!=0){x$rounding[bad]<-1}; print(paste(Sys.time(),'Ended rounding'),quote=FALSE)
    bad<-physics(x$value,240,1);x$large<-0;if(length(bad)!=0){x$large[bad]<-1}; print(paste(Sys.time(),'Ended physics, large'),quote=FALSE)
    bad<-physics(x$value,0,3);x$small<-0;if(length(bad)!=0){x$small[bad]<-1}; print(paste(Sys.time(),'Ended physics, small'),quote=FALSE)
    bad<-sunafterdark(x[3:4],substring(tx[i],9,14),home);x$maxsun<-0;if(length(bad)!=0){x$maxsun[bad]<-1}; print(paste(Sys.time(),'Ended sunafterdark'),quote=FALSE)
    consolidator(home,tx[i],x)
    write.table(x,paste(home,'QC/qc_',tx[i],sep=''),col.names=TRUE,row.names=FALSE,sep='\t',quote=FALSE); print(paste(Sys.time(),'Wrote QC results'),quote=FALSE)
  }
}

clocov<-function(home='../Sweden/', element='CC',maxseq=8,blocksizeround=20,blockmanymonth=20,blockmanyyear=200){
  # OBJECTIVE: this function will centralize temperature-like qc routines. Will create a file in the folder QC
  # with an additional 0/1 column, where "1" means test failed. 
  # home: path to the home directory
  # element: two-letters ECA&D code for the element (e.g., TX for maximum temperature)  
  # large: value above which the observation is considered physically impossible for the region, FUNCTION: physics
  # small: value below which the observation is considered physically impossible for the region, FUNCTION: physics     
  # maxseq: maximum number of consecutive repeated values, FUNCTION: flat  (11.1,11.1,11.1 would be 3 consecutives).
  #          higher than usual, as it is a discrete variable 
  # blocksizeround: maximum number of values in a month with the same decimal, FUNCTION:  rounding 
  # blockmanymonth: maximum number of equal values in a month, FUNCTION: toomany
  # blockmanyyear: maximum number of equal values in a yaer, FUCNTION: toomany
  
  lista<-list.files(path=paste(home,'raw',sep=''),pattern='SOUID')
  tx<-lista[which(substring(lista,1,2)==element)];ene<-length(tx)
  if(length(tx)==0){return()}
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
    bad<-flatsun(x[,3:4],maxseq,tx[i],home,FALSE);x$flatsun<-0;if(length(bad)!=0){x$flatsun[bad]<-1}; print(paste(Sys.time(),'Ended flatsun'),quote=FALSE)
    bad<-rounding(x[,3:4],blocksizeround);x$rounding<-0;if(length(bad)!=0){x$rounding[bad]<-1}; print(paste(Sys.time(),'Ended rounding'),quote=FALSE)
    bad<-physics(x$value,8,1);x$large<-0;if(length(bad)!=0){x$large[bad]<-1}; print(paste(Sys.time(),'Ended physics, large'),quote=FALSE)
    bad<-physics(x$value,0,3);x$small<-0;if(length(bad)!=0){x$small[bad]<-1}; print(paste(Sys.time(),'Ended physics, small'),quote=FALSE)
    consolidator(home,tx[i],x)
    write.table(x,paste(home,'QC/qc_',tx[i],sep=''),col.names=TRUE,row.names=FALSE,sep='\t',quote=FALSE); print(paste(Sys.time(),'Wrote QC results'),quote=FALSE)
  }
}



windspeed<-function(home='../Sweden/', element='FG',maxseq=3,blocksizeround=20,blockmanymonth=20,blockmanyyear=200,large=3000,roundmax=10,level=5,window=30,ret=500,margina=0.999){
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
  # roundmax: maximum number of consecutive decimal part value, for flat function (10.0, 11.0, 12.0 would be 3 consecutive)
  # level: number of IQRs for IQR outliers
  # window: window, in days, for IQR outliers
  # ret: pseudo-return period for the pareto outliers 
  # margina: quantile for newfriki function
  
  
  lista<-list.files(path=paste(home,'raw',sep=''),pattern='SOUID')
  tx<-lista[which(substring(lista,1,2)==element)];ene<-length(tx)
  if(length(tx)==0){return()}
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
    bad<-IQRoutliers(x$date,x$value,level,window);x$IQRoutliers<-0;if(length(bad)!=0){x$IQRoutliers[bad]<-1}; print(paste(Sys.time(),'Ended IQRoutliers'),quote=FALSE) # check
    bad<-paretogadget(x[,4],ret);x$paretogadget<-0;if(length(bad)!=0){x$paretogadget[bad]<-1} ; print(paste(Sys.time(),'Ended paretogadget'),quote=FALSE)
    bad<-newfriki(x$date,x$value,margina,times=3);x$friki<-0;if(length(bad)!=0){x$friki[bad]<-1} ; print(paste(Sys.time(),'Ended newfriki for errors'),quote=FALSE)
    bad<-newfriki(x$date,x$value,margina,times=1.5);x$frikilight<-0;if(length(bad)!=0){x$frikilight[bad]<-1} ; print(paste(Sys.time(),'Ended newfriki for suspect'),quote=FALSE)
    bad<-physics(x$value,large,1);x$large<-0;if(length(bad)!=0){x$large[bad]<-1}; print(paste(Sys.time(),'Ended physics, large'),quote=FALSE)
    bad<-physics(x$value,0,3);x$small<-0;if(length(bad)!=0){x$small[bad]<-1}; print(paste(Sys.time(),'Ended physics, small'),quote=FALSE)
    consolidator(home,tx[i],x)
    write.table(x,paste(home,'QC/qc_',tx[i],sep=''),col.names=TRUE,row.names=FALSE,sep='\t',quote=FALSE); print(paste(Sys.time(),'Wrote QC results'),quote=FALSE)
  }
}



snowdepth<-function(home='../Sweden/', element='SD',maxseq=20,blocksizeround=20,blockmanymonth=20,blockmanyyear=200,large=5000,exclude=0){
  # OBJECTIVE: this function will centralize temperature-like qc routines. Will create a file in the folder QC
  # with an additional 0/1 column, where "1" means test failed. 
  # home: path to the home directory
  # element: two-letters ECA&D code for the element (e.g., TX for maximum temperature)  
  # large: value above which the observation is considered physically impossible for the region, FUNCTION: physics
  # small: value below which the observation is considered physically impossible for the region, FUNCTION: physics     
  # maxseq: maximum number of consecutive repeated values, FUNCTION: flat  (11.1,11.1,11.1 would be 3 consecutives).  
  # exclude: value to be excluded from a function (in this case, 0 for flats. )
  # blocksizeround: maximum number of values in a month with the same decimal, FUNCTION:  rounding 
  # blockmanymonth: maximum number of equal values in a month, FUNCTION: toomany
  # blockmanyyear: maximum number of equal values in a yaer, FUCNTION: toomany
  
  lista<-list.files(path=paste(home,'raw',sep=''),pattern='SOUID')
  tx<-lista[which(substring(lista,1,2)==element)];ene<-length(tx)
  if(length(tx)==0){return()}
  for(i in 1:ene){
    name<-paste(home,'raw/',tx[i],sep='')
    print(paste(name,i,'of',ene),quote=FALSE)
    x<-readecad(input=name) ; print(Sys.time())
    x<-x[,1:4];colnames(x)<-c('STAID','SOUID','date','value')
    bad<-duplas(x$date);x$dupli<-0;if(length(bad)!=0){x$dupli[bad]<-1} ; print(paste(Sys.time(),'Ended duplas'),quote=FALSE)
    bad<-weirddate(x[,3:4]);x$weirddate<-0;if(length(bad)!=0){x$weirddate[bad]<-1}; print(paste(Sys.time(),'Ended weirddate'),quote=FALSE)
    bad<-toomany(x[,3:4],blockmanymonth,1,exclude=0);x$toomanymonth<-0;if(length(bad)!=0){x$toomany[bad]<-1}; print(paste(Sys.time(),'Ended toomany, month'),quote=FALSE)
    bad<-toomany(x[,3:4],blockmanyyear,2,exclude=0);x$toomanyyear<-0;if(length(bad)!=0){x$toomany[bad]<-1}; print(paste(Sys.time(),'Ended toomany, year'),quote=FALSE)
    bad<-roundprecip(x[,3:4],blocksizeround,exclude =0);x$rounding<-0;if(length(bad)!=0){x$rounding[bad]<-1};  print(paste(Sys.time(),'Ended roundsnow'),quote=FALSE)
    bad<-physics(x$value,large,1);x$large<-0;if(length(bad)!=0){x$large[bad]<-1}; print(paste(Sys.time(),'Ended physics, large'),quote=FALSE)
    bad<-physics(x$value,0,3);x$small<-0;if(length(bad)!=0){x$small[bad]<-1}; print(paste(Sys.time(),'Ended physics, small'),quote=FALSE)
    bad<-flat(x$value,maxseq,exclude=0);x$flat<-0;if(length(bad)!=0){x$flat[bad]<-1}; print(paste(Sys.time(),'Ended flat for values'),quote=FALSE)
 
    consolidator(home,tx[i],x)
    write.table(x,paste(home,'QC/qc_',tx[i],sep=''),col.names=TRUE,row.names=FALSE,sep='\t',quote=FALSE); print(paste(Sys.time(),'Wrote QC results'),quote=FALSE)
  }
}




precip<-function(home='../Sweden/',large=5000,small=0,element='RR',ret=500,retornoracha=500,margin=20,friki=150,blocksizeround=20,
                 blockmanymonth=15,blockmanyyear=180,limit=1500,tolerance=8,maxseq=3,roundmax=10,level=15,window=30,
                 margina=0.999){
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
# limit: cut threshold for FUNCTION suspectacumprec
# tolerance: number of NA or 0s before allowed before the limit, FUNCTION suspectacumprec 
# blocksizeround: the maximum number of repeated values, FUNCTION roundprecip
# maxseq: maximum number of consecutive repeated values, FUNCTION: flat  (11.1,11.1,11.1 would be 3 consecutives).  
# roundmax: maximum number of consecutive decimal part value, for flat function (10.0, 11.0, 12.0 would be 3 consecutive)
# level: level for IQRoutliers
# window: window for IQRoutliers
  
# NOTE: parameters exclude, excluido and alike are not included anymore. No need to paramatrize the obvious and unchanging: always
# need to exclude 0 for precipitation  
    
  lista<-list.files(path=paste(home,'raw',sep=''),pattern='SOUID')
  tx<-lista[which(substring(lista,1,2)==element)];ene<-length(tx)
  if(length(tx)==0){return()}
  ### provisional
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
    bad<-toomany(x[,3:4],blockmanymonth,1,0);x$toomanymonth<-0;if(length(bad)!=0){x$toomany[bad]<-1}; print(paste(Sys.time(),'Ended toomany, month'),quote=FALSE)
    bad<-toomany(x[,3:4],blockmanyyear,2,0);x$toomanyyear<-0;if(length(bad)!=0){x$toomany[bad]<-1}; print(paste(Sys.time(),'Ended toomany, year'),quote=FALSE)
    bad<-newfriki(x$date,x$value,margina,times=3);x$friki<-0;if(length(bad)!=0){x$friki[bad]<-1} ; print(paste(Sys.time(),'Ended newfriki for errors'),quote=FALSE)
    bad<-newfriki(x$date,x$value,margina,times=1.5);x$frikilight<-0;if(length(bad)!=0){x$frikilight[bad]<-1} ; print(paste(Sys.time(),'Ended newfriki for suspect'),quote=FALSE)
    bad<-flat(x$value,maxseq,exclude=0);x$flat<-0;if(length(bad)!=0){x$flat[bad]<-1}; print(paste(Sys.time(),'Ended flat for values. Sequences with 0 are excluded'),quote=FALSE)
    bad<-flat(x$value%%10,roundmax,exclude=0);x$roundmax<-0;if(length(bad)!=0){x$roundmax[bad]<-1}; print(paste(Sys.time(),'Ended flat for decimal part'),quote=FALSE) #this looks at consecutive decimal parts
    bad<-IQRoutliers(x[,3],x[,4],exclude=0,window=window,level = level);x$IQRoutliers<-0;if(length(bad)!=0){x$IQRoutliers[bad]<-1}; print(paste(Sys.time(),'Ended flat for decimal part'),quote=FALSE)
    consolidator(home,tx[i],x)
    write.table(x,paste(home,'QC/qc_',tx[i],sep=''),col.names=TRUE,row.names=FALSE,sep='\t',quote=FALSE); print(paste(Sys.time(),'Wrote QC results'),quote=FALSE)
  }
  
  
}


weirddate<-function(x){
  bad<-NULL
  fy<-as.numeric(substring(x[1,1],1,4))
  ly<-as.numeric(substring(x[nrow(x),1],1,4))
  x$year<-as.numeric(substring(x[,1],1,4));x$month<-as.numeric(substring(x[,1],5,6));x$day<-as.numeric(substring(x[,1],7,8))
  k1<-which(x$year < fy | x$year > ly)
  k2<-which(x$day > 31)
  k3<-which(x$day > 30 & x$month %in% c(4,6,9,11))
  k4<-which(x$day > 29 & x$month ==2)
  k5<-which(x$day > 28 & x$month == 2 & x$year%%4 !=0 )
  k6<-which(x$day > 28 & x$month == 2 & x$year == 1900 )
  k7<-which(x$month < 1 | x$month > 12)
  bad<-c(k1,k2,k3,k4,k5,k6,k7)
  return(bad)
  }


sunafterdark<-function(y,code='991274',casa){
## OBJECTIVE: compares SUNSHINE data to the maximum theoretical sunshine for lat, lon, day
## maximum sunshine hours are computed from suncalc package, using "night" and "dawn" parameters
## This contrasts quite a lot with other functions computing "daylength". This formulation
## is more conservative  
## INPUT:
## y: ECA&D style two columns: date (yyyymmdd, values (0.1 hours)
##code (the "numeric" part of the SOUID, expressed as character, to avoid trouble with leading zeroes)
## OUTPUT:
## bad: list of values which do nat pass QC
## WARNING: 
## depends on the availability of "liston"  

  bad<-NULL
  if(!exists("liston")){#### this takes quite a lot of time, and better if ran only once. Then, the list is declared as a global variable
    liston<-listas(rooty=casa)
    lat<-apply(as.data.frame(liston$LAT),1,FUN=decimaldegrees)  
    lon<-apply(as.data.frame(liston$LON),1,FUN=decimaldegrees) 
    coordinates<-data.frame(lat,lon) 
    liston$LAT<-lat
    liston$LON<-lon
    
    assign("liston",liston,envir = .GlobalEnv)
    
  }
  
  

  lat<-first(liston$LAT[which(liston$SOUID==code & substring(liston$ELEI,1,2)=='SS')]) ## prefixing with first because some stations are repeated. Assume lat/lon
  lon<-first(liston$LON[which(liston$SOUID==code & substring(liston$ELEI,1,2)=='SS')]) ## will not be too different in "susnhine" terms  
  names(y)<-c('fecha','value')
  y$year<-substring(y$fecha,1,4)
  y$month<-substring(y$fecha,5,6)
  y$day<-substring(y$fecha,7,8)
  y<-y[,c(3:5,1:2)]
  targetyear<-round(mean(as.numeric(y$year)))
  while(targetyear%%4 != 0 & targetyear!=1900){targetyear=targetyear+1}
  targetstart<-as.Date(paste(targetyear,'01','01',sep='-'))
  targetdates<-seq(targetstart,targetstart+365,1)
  totest<-data.frame(targetdates,lat,lon);names(totest)<-c('date','lat','lon')
  totest$maxdur<-as.numeric(getSunlightTimes(data = totest)$night-getSunlightTimes(data = totest)$dawn)*10
  y$monthday<-paste(y$month,y$day,sep='-')
  totest$monthday<-substring(totest$date,6,10)
  zzpaff<-merge(y,totest);zzpaff<-zzpaff[order(zzpaff$fecha),]
  bad<-which(zzpaff$maxdur<zzpaff$value)
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
  
  if(nrow(z) == 0){return(NULL)} ### This is to make it ressistant to series with no values different than zero!

  #### Warning! This line was erroneous and was: 
  #nyu<-as.data.frame(table(y[,1],y[,2])) ### this is good, it can identify those which are over the blocksize, but need to now how to extract/label the values
  # This was causing the code to be really slow, as it was looking at "y" instead of "z". This was labeling most of the values as erroneous and the next loop was taking
  # forever. 
  nyu<-as.data.frame(table(z[,1],z[,3])) ### this is good, it can identify those which are over the blocksize, but need to now how to extract/label the values
  
  target<-which(nyu[,3] >= blocksize)
  ene<-length(target)
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
  return(bad)
}


repeatedvalue<-function(x,margin=20,friki=150){
  
  ### WARNING: eval this function, as probably does the same than newfriki, but in a less efficient and more time consuming way
  ### also, parametrization is less objective
  
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
  target<-NULL
  nyu<-potpareto(x);if(is.null(nyu)){return(NULL)}
  mus<-returnpotpareto(nyu,ret)
  target<-which(x > mus)
  return(target)
  }

potpareto<-function(y,thres=0.99){

  target<-which(!is.na(y) & y!=0)
  xx<-y[target]          
  if(length(target) == 0){return(NULL)} #### this is activated if all the values are 0!!!
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
  bisco<-NULL
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



drywetlong<-function(x,ret=300,sueco=9.9,dry=TRUE,wet=TRUE){
  
# OBJECTIVE: to detect episodes of too many consecutive wet or dry days
# INPUT: 
# x: vector with  values
# ret: pseudo-return period (pareto-based) to compute the maximum tolerable spell
# RETURNS:
# bad: list of positions which do not pass qc
# sueco: threshold for dividing dry and wet. This is useful to label other binary sequences, e.g. for 0 radiation. Now it is <= and >, instead of < and >=
# wet: if set to TRUE, wet sequences are sent to result; if FALSE, omitted
# dry: same as previous, for dry sequences   
  
    
todo<-NULL  
seco<-which( x<= sueco) # using the 1 mm threshold as in RClimdex
mojao<-which(x >sueco)
nohay<-which(is.na(x))
y<-x
## creating a binary variable
y[seco]<-0  ### dry values set to 0
y[nohay]<-9 ### NAs set to 9
y[mojao]<-1 ### wet days set to 1
nyu<-rle(y)


wetspell<-nyu$lengths[which(nyu$values==1)]
if(length(wetspell) > 0){ ### some VERY short series do not have even a wetspell
nyi<-potpareto(wetspell)
if(!is.null(nyi)){wetlim<-returnpotpareto(nyi,ret)}else{wetlim<-999999999999999} ### when the paretp apparel does not find a value higher than the quantile, fails. Solved (needs to be improved)
wetchungo<-which(nyu$lengths > wetlim & nyu$values == 1)
}else{wetchungo=NULL}

dryspell<-nyu$lengths[which(nyu$values==0)]
if(length(dryspell) > 0){ ### some VERY short series do not have even a dryspell
nyi<-potpareto(dryspell)
if(!is.null(nyi)){drylim<-returnpotpareto(nyi,ret)}else{drylim<-999999999999999}### when the paretp apparel does not find a value higher than the quantile, fails. Solved (needs to be improved)
drychungo<-which(nyu$lengths > drylim & nyu$values == 0)
}else{drychungo=NULL}

if(dry & wet){chungos<-c(wetchungo,drychungo)}
if(dry & !wet){chungos<-drychungo}
if(!dry & wet){chungos<-wetchungo}
if(length(chungos) !=0 ){
ene<-length(chungos)

#### ACHTUUUNG!!! Corrected in January 2019: it was erroneously considered that rle "values" were signaling begining of strike and they are END OF STRIKE. 
for(i in 1:ene){
  rocha<-nyu$lengths[chungos[i]] ### the value exceeding the tolerated strike
  end<-sum(nyu$lengths[1:chungos[i]]) 
  start<-end-rocha+1
  if(i==1){todo<-c(start:end)}else{todo<-c(todo,start:end)}
}
return(todo)
}

}

txtn<-function(y,id,home){
  
  # Objective: compares tx an tn. First looks for the closest station and then merges both dataframes
  ### If one value is flagged, looks at the ecdfs of tx and tn. 
  ## If the target variable (e.g tx) is central (between quantiles 0.2 and 0.8) and the other variable (e.g. tn) is outside this range, the value is not flagged, 
  ## assuming the other variable is the culprit
  # INPUT: 
  # y: two columns with date and data
  # id: the id we are working with
  # home: home folder, need to add "raw" inside the fucntion
  # RETURNS:
  # bad: list of positions which do not pass qc
  
  bad<-NULL
  if(!exists("liston")){listonator(home)} ### warning! not working here :-()
  whoami<-substring(id,1,2);if(whoami=='TX'){targetvariable='TN'}else{targetvariable='TX'}
  me<-first(which(liston$SOUID == substring(id,9,14)))
  em<-first(which(liston$STAID == liston$STAID[me] & liston$PARID==liston$PARID[me] & liston$ELEI==paste0(targetvariable,substring(liston$ELEI[me],3))))
  ### comprobar si tenemos estaciones. Pensar si hay mas de una que se hace. 
  if(length(em)==0){return(bad)}
  cerca<-sprintf('%s_SOUID%06d.txt', targetvariable, liston$SOUID[em]) 
  name<-paste(home,'raw/',cerca,sep='')
  if(!file.exists(name)){return(bad)}
  x<-readecad(name);x<-x[,3:4]
  if(whoami == 'TX'){names(x)<-c('date','tn');names(y)<-c('date','tx')}
  if(whoami == 'TN'){names(y)<-c('date','tn');names(x)<-c('date','tx')}
  z<-merge(x,y,all.y=TRUE,all.x=FALSE)
  bad<-which(z$tx <= z$tn)
  if(length(bad)==0){return(bad)}
  
  z$month<-as.factor(as.numeric(substring(z$date,5,6)))
  z$bad<-0;z$bad[bad]<-1
    for(i in 1:length(bad)){
      
    mes<-which(z$month==z$month[bad[i]])
    mes<-mes[-bad[i]]
    etx<-abs(ecdf(z$tx[mes])(z$tx[bad[i]])-1);etn<-abs(ecdf(z$tn[mes])(z$tn[bad[i]])-1)
    if(targetvariable=='TX' & etn > 0.8 & etx < 0.8){z$bad[bad[i]]==0}
    if(targetvariable=='TN'& etx > 0.8 & etn < 0.8){z$bad[bad[i]]==0}
}
return(bad)
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
 if(nrow(nyu)==0){return(NULL)}
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
  if(nrow(nyu)==0){return(NULL)}
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
  bad<-NULL
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
  total<-NULL
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



jumps2<-function(date,value,quanty=0.999,times=1,force=NULL) {
  # OBJETIVO: to label interdiurnal differences considered to large
  # INPUT: 
  # x: vector of values
  # maxjump: max difference allowed
  # RETURN:
  # chungo: both values involved in the jump are returned. Need an additional function to decide which is the culprit
  chungo<-NULL
  x<-data.frame(month=as.numeric(substring(date,5,6)),value,date) ### create a data.frame with month, value, date

  x$difs<-0;x$difs[1]<-NA;x$difs[2:nrow(x)]<-diff(x$value) # creating the difs field
  targets<-aggregate(x$difs,by=list(x$month),quantile,probs=quanty,na.rm=TRUE);names(targets)<-c("month","threshold") # targets, by month
  ## merging, threshold is multiplied by times
  x<-merge(x,targets,all.x=TRUE)
  x<-x[order(x$date),]
  x$threshold=x$threshold*times
  
  
  if(!is.null(force)){x$threshold=force}
  

  
  ## this isolates  differences over the threshold
  chungo<-which(abs(x$difs)>x$threshold)
  ene<-length(chungo)
  if(ene==0){return(NULL)}
  # here, we decided which member of the pair involved in a large difference is the culprit
  for(i in 1:ene){
    # rationale is , one by one, we look at which of both is most extreme in the ecdf, computed without them

    left=chungo[i]-1
    center=chungo[i]
    koko<-ecdf(x$value[-(left:center)])      
    ecenter<-min(1-koko(x$value[center]),koko(x$value[center]))
    eleft<-min(1-koko(x$value[left]),koko(x$value[left]))
    if(eleft < ecenter){chungo[i]<-left}
      }
    # need to use unique, as some values might be there twice (large difference with the previous and the following)
    chungo<-unique(chungo)
    return(chungo)
}








flatsun<-function(x,maxseq,id,casa,modonube=FALSE){
  
  
  ### OBJECTIVE: it is a flat (actually, uses flat) modified with "smart" comparison with clouds. If close to 8 and close to 0 clouds, allowed; if close to maxsundur 
  ### and clouds near 0, allowed
  
  ### CATION: While E. Coyote Genius Masterpiece
  ### updated to test cloud flats; Rationale: add "modononube"; if TRUE, changes are made to end up with "sun" in x and "clouds" in y
  ## therafter, everything remains the same. 
  
  
  
  # x: data.frame date/value (need dates in this implementation of flat)
  # maxseq: the maximum number of contiguous repetitions of a value (e.g., if 3, sequences of 4 will be flagged)
  ### id: SOUID_SSxxxxxx.txt
  ### casa: does not include raw, e.g. '../mickeymouse/'
  
  # RETURN: 
  # bad: list of positions which do not pass qc
  
  
  
  bad<-NULL
  if(!exists("liston")){listonator(casa)}
  milista<-unique(liston)
  if(!modonube){me<-which(milista$SOUID == substring(id,9,14) & substring(milista$ELEI,1,2)=='SS')}
  if(modonube){me<-which(milista$SOUID == substring(id,9,14) & substring(milista$ELEI,1,2)=='CC')}
  if(!modonube){cloud<-which(milista$STAID == milista$STAID[me] & milista$PARID == milista$PARID[me] & substring(milista$ELEI,1,2)=='CC')}
  
  if(length(cloud)==0){bad<-flat(x[,2],maxseq,exclude=100);return(bad)}
  if(!is.null(cloud)){
    if(!modonube){y<-readecad(paste0(casa,'raw/CC_SOUID',milista$SOUID[cloud],'.txt'))[,3:4]}
    if(modonube){y<-x;x<-readecad(paste0(casa,'raw/SS_SOUID',milista$SOUID[cloud],'.txt'))[,3:4]}
    
    
    
    
    names(x)<-c('date','sun');names(y)<-c('date','cloud')
    z<-merge(x,y,all.x=TRUE,all.y=FALSE)
    maximo<-length(which(!is.na(z[,2])));real<-length(which(!is.na(z[,2]) & !is.na(z[,3])))
    evaluation=real/maximo
    if(evaluation < 0.80){bad<-flat(x[,2],maxseq,exclude=100);return(bad)}
    if(!modonube){bid<-flat(z[,2],maxseq)}
    if(modonube){bid<-flat(z[,3],maxseq)}
    if(length(bid)==0){return(bad)}
    z$flat<-0;z$flat[bid]<-1
    z$difs<-0;z$difs[1]<-NA;z$difs[2:nrow(z)]<-diff(z$flat)
    if(z$flat[1]==1){z$difs[1]<-1}
    start=which(z$difs == 1)
    end=which(z$difs == -1)-1
    if(z$flat[nrow(z)]==1){end<-c(end,nrow(z))}
    juntos<-data.frame(start,end)
    for(i in 1:nrow(juntos)){juntos$sol[i]<-mean(z$sun[juntos$start[i]:juntos$end[i]],na.rm=TRUE)}
    for(i in 1:nrow(juntos)){juntos$nube[i]<-mean(z$cloud[juntos$start[i]:juntos$end[i]],na.rm=TRUE)}
    for(i in 1:nrow(juntos)){if(!is.na(juntos$sol[i]) & !is.na(juntos$nube[i])){if(juntos$sol[i]< 1 & juntos$nube[i] > 7){z$flat[juntos$start[i]:juntos$end[i]]<-0}}}
    juntos$date<-z$date[start]
    juntos$date<-as.Date(paste(substring(juntos$date,1,4),substring(juntos$date,5,6),substring(juntos$date,7,8),sep='-'))
    juntos$lat<-milista$LAT[me];juntos$lon<-milista$LON[me]
     totest<-data.frame(juntos$date,juntos$lat,juntos$lon);names(totest)<-c('date','lat','lon')     
    juntos$maxsun<-juntos$sol/(as.numeric(getSunlightTimes(data=totest)$night-getSunlightTimes(data=totest)$dawn)*10)*100       ## proportion to the maximum sun          
    #### If proportion to maximum sun is larger than 90 and clouds smaller than 1, allowed
    juntos$maxsun[which(is.na(juntos$maxsun))]<-0 ### need this beacuse getSunlight times gives an unexplained NA sometimes
    for(i in 1:nrow(juntos)){if(!is.na(juntos$sol[i]) & !is.na(juntos$nube[i])){if(juntos$maxsun[i]> 90 & juntos$nube[i] < 2){z$flat[juntos$start[i]:juntos$end[i]]<-0}}}
    bad<-which(z$flat == 1)
    return(bad)
}}




flat<-function(y,maxseq,exclude=NULL){
  # OBJECTIVE: detects consecutive equal values. Can be adapted to detect consecutive equal decimal part of the values
  # PAREMETERS: 
  # y: a data vector
  # maxseq: the maximum number of contiguous repetitions of a value (e.g., if 3, sequences of 4 will be flagged)
  #
  # RETURN: 
  # target: list of positions which do not pass qc
  
  target<-NULL
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
  if(!is.null(exclude)){
    quitar<-which(y[target] == exclude)
    target<-target[-quitar]
  }
return(target)}
}


newfriki<-function(date,value,margina=0.999,times=2){
  
  # OBJECTIVE: isolates values which are not continuous in the  distribution. If the gap is larger than a preset big margin, 
  #		the values above (or below) are flagged
  # INPUT: 
  #date: a vector with yyyymmdd
  #values: a vector wih values 
  #margina: the tolerence margin, expressed as quantile of the differences
  #times: multiplicator to the tolerance margin. Intended usage is to run this twice. Once with times = 1 and flag values as suspect; once with times = 2 and flag as error
  
  # OUTPUT:
  bad<-NULL
  y<-data.frame(date,value)
  for(ij in 1:12){
    position<-which(as.numeric(substring(y$date,5,6))==ij) # find the positions for the month ij
    mes<-y[position,]  # subset the positions
    sorted<-sort(mes$value)  # sort the values
    diffy<-(diff(sorted)) # create the first difference of the sorted values
    margen<-quantile(diffy,margina)
    malo<-which(diffy > times*margen) # isolate the values 
    if(length(malo!=0)){
    chungo<-sorted[malo] # isolate the values 
    qchungo<-ecdf(sorted)(chungo) 
    for(k in 1:length(qchungo)){
      if(qchungo[k] > 0.5){fechachungo<-y$date[which(y$value > chungo[k] & as.numeric(substring(y$date,5,6))==ij)]} #### If it is in the second half of the distro
      if(qchungo[k] <=0.5){fechachungo<-y$date[which(y$value <= chungo[k] & as.numeric(substring(y$date,5,6))==ij)]} #### It if is in the first half of the distro 
      if(exists('fechas')){fechas<-c(fechas,fechachungo)}else{fechas<-fechachungo}
    }  
    }}
  if(exists('fechas')){bad<-which(date%in% fechas)}  
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


IQRoutliers<-function(date,value,level=3,window=11,exclude=NULL){
  
  # OBJECTIVE:computes outliers centralized around a day, using a number of days around it
  # PARAMETERS:
  # date: vector with dates
  # values: vector with values
  # level: number of IQRs
  # window: number of days to be considered (including the target)
  # exclude: if is not null, the code will exlcude this value from the analyisis (i.e., good to exclude 0 for precip)
  # RETURNS:
  # bad: positions which do not pass this test
  
  
  bad<-NULL
  
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
  yy<-y ### this works the magic for exclude.
  if(!is.null(exclude)){target<-which(y$value != exclude);y<-y[target,]}
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
  #if(exists('fecha')){bad<-which(y$date %in% fecha)
  if(exists('fecha')){bad<-which(yy$date %in% fecha) ### this makes shure that, even using exclude, the returned bad values are correct
  return(bad)
  }
}














########### UTILS ###

listonator<-function(casa,check=TRUE){
if(!exists("liston") & isTRUE(check)){#### this takes quite a lot of time, and better if ran only once. Then, the list is declared as a global variable
  liston<-listas(rooty=casa)
  lat<-apply(as.data.frame(liston$LAT),1,FUN=decimaldegrees)  
  lon<-apply(as.data.frame(liston$LON),1,FUN=decimaldegrees) 
  coordinates<-data.frame(lat,lon) 
  liston$LAT<-lat
  liston$LON<-lon
  
  assign("liston",liston,envir = .GlobalEnv)
  
}}






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
  
  if(nrow(reference)> 1){reference<-reference[1,]}
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
  
  # This is not needed: done when filtering mylist
  #if(length(perfect)==0){return(NULL)} ### this means that there is no good station
  if(nrow(perfect)> 1){perfect<-perfect[which(perfect$DROP == min(perfect$DROP)),]} # TIE BREAKER 1: the ID should be close (removes GTS stations)
  if(nrow(perfect)> 1){perfect<-perfect[which(perfect$DIFID == min(perfect$DIFID)),]} # TIE BREAKER 1: the ID should be close (removes GTS stations)
  if(nrow(perfect)> 1){perfect<-perfect[which(perfect$OVERLAP == max(perfect$OVERLAP)),]} # TIE BREAKER 2: overlap, good for stations made of segments. 
  if(nrow(perfect)> 1){perfect<-perfect[which(perfect$IDDIST == min(perfect$IDDIST)),]}
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
  ereseunnyu<-0
  for(i in 1:ene){
    list<-paste(rooty,'ECA_blend_source_',tolower(variables[i]),'.txt',sep='')
    if(file.exists(list)){
    ereseunnyu<-ereseunnyu+1
    x<-read.fwf(list,widths=15)
    nyu<-grep('STAID,',x[,1])
    if(length(nyu)==0){nyu<--1}

    
    ##### WARNING: Whie E. Coyote genius piece!!!! Dynamic allocation of format to read ECA&D station files. Scan first line as character and derive widths :-)
    ## replaces direct assignment. Necessary as some transfers had an extra column in the first field. 
    
    urruti<-scan(list,what='character',nlines=1,sep=',',quiet=TRUE)
    custers<-nchar(urruti)
    nkono=c(custers[1],1,custers[2],1,custers[3],1,custers[4],1,custers[5],1,custers[6],1,custers[7],1,custers[8],1,custers[9],1,custers[10],1,custers[11],1,custers[12])
#    nkono=c(5,1,6,1,40,1,2,1,9,1,10,1,4,1,4,1,8,1,8,1,5,1,51)
    
    #custers=c(5,6,40,2,9,10,4,4,8,8,5,51)
    x<-read.fwf(list,na.strings=missing,widths=nkono,skip=nyu+1,stringsAsFactors=FALSE,strip.white = TRUE)
    x<-x[,c(1,3,5,7,9,11,13,15,17,19,21,23)]
    names(x)<-c('STAID','SOUID','SOUNAME','CN','LAT','LON','HGTH','ELEI','START','STOP','PARID','PARNAME')
    if(ereseunnyu==1){todas<-x}else{todas<-rbind(todas,x)}
   
    }
  }
  #todas<-todas[order(todas[,c(1,2)]),]
  if(country!='all'){target<-which(todas$CN == country);todas<-todas[target,]}
  #tidos<-rbind(names(x),todas) ### this seems to be inncessary, as duplicates the header. Commented
  #write.fwf(tidos,paste(rooty,name,sep=''),sep=' ',colnames=FALSE,rownames=FALSE,quote=FALSE,width=custers) ## as a consecuence of the previous comment
  write.fwf(todas,paste(rooty,name,sep=''),sep=' ',colnames=FALSE,rownames=FALSE,quote=FALSE,width=custers) ## as consecuence of the previous action
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
### replaced by jumps2
txtn_old<-function(y,id,home){
  # DEPRECATED
  # Objective: compares tx an tn. First looks for the closest station and then merges both dataframes
  # INPUT: 
  # y: two columns with date and data
  # id: the id we are working with
  # home: home folder, need to add "raw" inside the fucntion
  # RETURNS:
  # bad: list of positions which do not pass qc
  
  bad<-NULL
  if(!exists("liston")){listonator(home)}
  
  
  whoami<-substring(id,1,2);if(whoami=='TX'){targetvariable='TN'}else{targetvariable='TX'}
  
  #replacing the following line by a search by element and parid
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






jumps<-function(x,maxjump) {
  # OBJETIVO: to label interdiurnal differences considered to large
  # INPUT: 
  # x: vector of values
  # maxjump: max difference allowed
  # RETURN:
  # chungo: both values involved in the jump are returned. Need an additional function to decide which is the culprit
  
  
  
  diffy<-diff(x)
  chungo<-which(abs(diffy)>maxjump)
  chungo<-unique(sort(c(chungo,chungo+1)))
  return(chungo)
  
  
  
  
  return(chungo)
  
}









# TO BE REPLACED by newfriki

badfriki<-function(date,value,margina=80){
  
  #### WARNING: this formulation does not work well. Most likely, to be replaced with newfriki. Already replaced for precip
  
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

