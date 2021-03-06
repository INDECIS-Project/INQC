% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/climdex2ecad.R
\name{climdex2ecad}
\alias{climdex2ecad}
\title{Converter from the ClimDex format into the ECA&D format (blended version)}
\usage{
climdex2ecad(
  homefolder = "./",
  stationlist = "stations.csv",
  countrycode = "DE"
)
}
\arguments{
\item{homefolder}{path to the home directory which should contain the subdirectory 'raw_ClimDex' with
files in the ClimDex format}

\item{stationlist}{list (as 'csv'-file) of climatological stations to be considered. Each line should be in the format:
lat (as dec. degree), lon (as dec. degree), height, staname}

\item{countrycode}{two character country code}
}
\value{
station and data files in the ECA&D format stored in the subdirectory 'raw'
}
\description{
This function will convert station and data files in ClimDex format into
corresponding station and data files in the ECA&D format (blended version)
}
\examples{
#Set a temporal working directory:
wd <- tempdir(); wd0 <- setwd(wd)
#Create subdirectory where raw data files in the ClimDex format have to be located
dir.create(file.path(wd, "raw_ClimDex"))
#Extract the ClimDex data and station files from the example data folder
path2stalist<-system.file("extdata", "stations.csv", package = "INQC")
stalist<-readr::read_lines_raw(path2stalist)
readr::write_lines(stalist,file=paste0(wd,"/raw_ClimDex/stations.csv"))
path2data1<-system.file("extdata", "Deuselbach.txt", package = "INQC")
data1<-readr::read_lines_raw(path2data1)
readr::write_lines(data1, file=paste0(wd,"/raw_ClimDex/Deuselbach.txt"))
path2data2<-system.file("extdata", "Staname.txt", package = "INQC")
data2<-readr::read_lines_raw(path2data2)
readr::write_lines(data2, file=paste0(wd,"/raw_ClimDex/Staname.txt"))
#Call the converter
climdex2ecad(homefolder = "./",stationlist = "stations.csv",countrycode = "DE")
#The results can be found in the directory:
print(wd)
#Return to user's working directory:
setwd(wd0)
}
