% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/decimaldegrees.R
\name{decimaldegrees}
\alias{decimaldegrees}
\title{Converter for geographical coordinates from the ECA&D format into decimal degrees}
\usage{
decimaldegrees(dms, sep = ":")
}
\arguments{
\item{dms}{ONE ELEMENT from the LAT or LON field in ECA&D listings}

\item{sep}{the separator between elements, in ECA&D ":"}
}
\value{
geographical coordinates (latitude or longitude) in decimal degrees
}
\description{
This function takes sexagesimal degrees in the ECA&D format and converts them into decimal degrees.
Initial idea was taken from: https://modtools.wordpress.com/2013/09/25/dms2dec/
}
\examples{
dms<-'+48:03:00'
dec<-decimaldegrees(dms)

dms<-'-015:03:00'
dec<-decimaldegrees(dms)
}
