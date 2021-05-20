# INQC v2.0.6

### Updates May, 2021:
* Two functions (i.e., `climdex2ecad()` and `cost2ecad()`) have been added. They provide useful tools to convert data to the ECA&D format (the main format of input data in INQC). The conversion can be performed for the COST Home and the RClimDex formats. 

### Updates March, 2021:
* Examples for almost all INQC functions were provided

### Updates December, 2020:
* Technical conversion to a CRAN package

### Updates October, 2020:
* NEW TO VERSION2: working on blended data instead of non-blended (i.e., STAID instead of SOUID): still `./raw` used to store the data. Station file is always `stations.txt` and as pass through `readecad()`. `stations.txt` must contain all stations to be processed. ECA&D seems to to have a comprehensive station file only subset if country sets are taken
* Using assign to `homofolder`, so it is declared at first and does not need to be passed on from routine to routine. Implementing it progressively once it is completed, will remove this mention
* `readecad()` changed to adapt the ever changing ECA&D formats
* `temperature()` starts adapted to cope with the fact that some ECA&D formats have 4 columns (i.e., not STAID), which is added
* Added `txtnblend()`, which works or blended series. Here, no big efforts to isolate the "right" station: is is the same staid changing the variable name ... oh, man, I love blended series!!!
* Same strategy applied to `readecad()`, implemented with `readheader()` 
* Added `dostats()`, function which iterates QCSummary and provides metastatistics
* WARNINGS: None at this point

# INQC v1.0.0

### Updates August, 2020:
* `jummps2()` improved to avoid very short series to cause error. If the series has less than 100 valid values, it is not checked
* `flatsun()` has been corrected. When ran in `modonube`, the values returned in bad where referred to the sun file instead of the cloud file. Solved by replacing: 
`z<-merge(x,y,all.x=TRUE,all.y=FALSE)` with `if(!modonube){z<-merge(x,y,all.x=TRUE,all.y=FALSE)};  if(modonube){z<-merge(x,y,all.x=FALSE,all.y=TRUE)}`
* Now, `listas()` reads station files as csv files. Realized this is not smart for the autodownloading function, but works for sent files

### Updates March, 2019:
* Function `flat` allows to exclude sequences for a value, i.e., excluding 0, will allow to work `flat` for precipitation
* Corrected error in `selepe`: results of `badfriki` were assigned to `flat` in the log files. 
* Corrected error in `selepe` parametrization, inadvertently, it was -8000 instead of 8000
* Defaulting `retornoracha = 500` instead of `retornoracha = 1000` in `precip` (fine tuning with benchmark, need to evaluate false positives)
* Corrected error in the statement: `rm(liston)`, which is executed at the end of the code. It has to be `rm(liston,envir=GlobalEnv)`. This error was preventing the generation of new station lists and using previous ones. This caused `txtn` to not perform as expected  
* Corrected problems with `closetxtn`. Problems that mostly affected Baboon data, due to its peculiarities. Changes should not affect real data if this is not the case, revert to a February version
* Added function `sunafterdark`: it compares sunshine duration to the maximum possible according to calendar and position
* Modified `IQRoutliers` function to exclude values (e.g., 0 for precipitation)
* Added `IQRoutliers` to the `precip` routine, with very wide limits to avoid flagging excessive number of values
* Flags for the consolidated file are being reshaped: 
* Correcting error: `consolidator()` was only being applied to temperature!
* Corrected error in `weirddate` that was flagging as bad the 29-th of February of leap years
* `listas()` made resistant to small format changes in the input files (e.g. no header or different widths than the "official" specification)
* Evaluation of pressure results denoted that `badfriki` was not working well. Replaced by `newfriki` for pressure and, most likely, will be deprecated for other variables too
* `jumps2` replaces `jumps`. Can be ran in two modes: split by month and with thresholds computed from the distribution of the differences or with an absolute threshold. In both cases, `jumps2` does not flag BOTH elements in a jump, only the one which is most likely the culprit
* Output in `QCConsolidated`, adds reference to this software and is "true" ECA&D format, with corrected widths and comma-separated
* Changes to `drywetlong`: the parameter `sueco` is the threshold for dividing dry and wet. This is useful to label other binary sequences, e.g. for 0 radiation. Now it is <= and >, instead of < and >=
* `txtn` improved:
  a) `closetxtn` is not used anymore (too slow) and the pair is located by `staid`, `eleid` and `parid`. If sticking to ECA&D, safe enough. 
  b) once values are flagged as "tx <= tn", we look if any value in the pair is central and the other extreme to their monthly distributions. Then, only the extreme is flagged
* Added `inithome()`. Creates all the folders and updates stations list

### Updates February, 2019:
* Variable calls made resistant to the existence of 0 series (i.e.: if `TG` does not exist, the call to `temperature(element='TG')` should not break the code anymore)
* `sundur()` had a parameter omitted (`roundmax`) which was causing the code to break. Solved

### Updates January, 2019:
* Corrected many problems encountered at `roundprecip()`. It used to flag most of the values due to an error. This also made the code really slow. Solved. 
* Made `drywetlong()` resistant to precipitation series with almost no data (RR_SOUID102169.txt has only ONE valid value, the rest is NA!!!)
* `roundprecip()` made resistant to series with no data different than 0. Returns NULL 
* `potpareto()` made resistant to cases were all values = 0 (e.g, RR_SOUID107781.txt, with < 800 values, and all them 0)
* `drywetlong()`: there was a mistake in `rle()` usage. Example: rle$values 1 2 2 5, it was interpreted as if the strike labeled with "5" was starting in position 10, when actually runs from 6 to 10!!!
* Corrected error in `bad<-flat(x$value%%10,roundmax)` for some variables: `roundmax` was not parameterized

### Updates December, 2018:
* Corrected some labels in for the temperature QC output that were erroneous (e.g., `toomany` instead of `toomanymonth` and `toomanyyear`)
* Added `consolidator`, a functions which creates a second version of the QCed data. Creates a replica of the original series, with the same name but with the QC column updated to 0 (OK)
* Corrected wrong default element flag in `selepe`
* Made a few corrections regarding the chain of functions `txtn()<-closetxtn()<-listas()<-distHaversine()` There were a few errors (hopefully, corrected) and now it is much faster just by limiting the station past as candidates to disthaversine to those in a radius of 1deg lat and 1deg lon from the candidate. For the whole ECA&D, the performance is down from 14 seconds to less than 2!
** Further improvements to the `closesetation()` function: does not give an error if the tries to open a file listed but non-existing
* Corrected problems in `roundprecip` and a bad parameterization in `precipip()`
* Modified `potpareto()` function to avoid problems with some matrices encountered in `drywetlong`: `myfit<-fpot(xx,threshold,std.err=FALSE)`	
* Modified `drywetlong` to avoid problems with desertic locations with almost no rain events
