# INQC

<!-- badges: start -->
[![R-CMD-check](https://github.com/OlegSkrynyk/INQC/workflows/R-CMD-check/badge.svg)](https://github.com/OlegSkrynyk/INQC/actions)
<!-- badges: end -->

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
