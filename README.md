
<!-- README.md is generated from README.Rmd. Please edit that file -->

# INQC

<!-- badges: start -->

[![R-CMD-check](https://github.com/INDECIS-Project/INQC/workflows/R-CMD-check/badge.svg)](https://github.com/INDECIS-Project/INQC/actions)
<!-- badges: end -->

The goal of INQC is to perform quality control (QC) of climatological
daily time series. INQC consists of a number of functions which provide
a wide range of QC tests for almost all types of errors in daily data
and almost all climate variables such as air temperature, atmospheric
precipitation, pressure etc. Originally, INQC was created in order to QC
the [ECA\&D](https://www.ecad.eu/dailydata/predefinedseries.php) station
data in the frame of the INDECIS project. However, INQC’s functions can
be used to deal with quality control problems in any other
climatological data set with the daily time resolution.

## Installation

You can install the released version of INQC from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("INQC")
```

Or the development version from
[GitHub](https://github.com/INDECIS-Project/INQC) with:

``` r
install.packages("devtools")
devtools::install_github("INDECIS-Project/INQC")
```
