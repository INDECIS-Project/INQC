% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/listas.R
\name{listas}
\alias{listas}
\title{Creates listings for stations ('non-blended' case) linking STAID and SOUID}
\usage{
listas(country = "all", name = "allstations.txt")
}
\arguments{
\item{country}{country for which the list is created. If 'all', no country filter.}

\item{name}{output file name, do not touch, default is always good.}
}
\value{
data frame and the list file containing all stations for all elements, linking STAID and SOUID
and metadata
}
\description{
This function takes all the elements and rbinds them into a single list to process
}
\examples{
#Set a temporal working directory:
wd <- tempdir(); wd0 <- setwd(wd)
#Extract the non-blended ECA&D station files from the example data folder
#Only TX (maximum air temperature) and CC (cloud cover) variables are used in the example
path2txlist<-system.file("extdata", "ECA_blend_source_tx.txt", package = "INQC")
txlist<-readr::read_lines_raw(path2txlist)
readr::write_lines(txlist,'ECA_blend_source_tx.txt')
path2cclist<-system.file("extdata", "ECA_blend_source_cc.txt", package = "INQC")
cclist<-readr::read_lines_raw(path2cclist)
readr::write_lines(cclist,'ECA_blend_source_cc.txt')
options("homefolder"='./')
liston.nb<-listas(country='all',name='allstations.txt')
#The created list file can be found in the directory:
print(wd)
#Return to user's working directory:
setwd(wd0)
}
