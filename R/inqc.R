inqc<-function(homefolder='./',blend=TRUE){

  #' Wrapper for QC'ing all variables
  #' @description This function calls functions which perform QC for all climate variables
  #' @param homefolder path to the homefolder, as string
  #' @param blend logical flag which means performing (if TRUE) QC on blended time series
  #' @return QC results, in both formats (verbose and workable file in exact ECA&D format)
  #' @examples
  #' #Set a temporal working directory:
  #' wd <- tempdir()
  #' wd0 <- setwd(wd)
  #' #Create subdirectory where raw data files have to be located
  #' dir.create(file.path(wd, 'raw'))
  #' #Extract the ECA&D data and station files from the example data folder
  #' #Only TX (maximum air temperature) and CC (cloud cover) data are used in the example
  #' path2txlist<-system.file("extdata", "ECA_blend_source_tx.txt", package = "INQC")
  #' txlist<-readr::read_lines_raw(path2txlist)
  #' readr::write_lines(txlist,'ECA_blend_source_tx.txt')
  #' path2txdata<-system.file("extdata", "TX_SOUID132734.txt", package = "INQC")
  #' txdata<-readr::read_lines_raw(path2txdata)
  #' readr::write_lines(txdata, file=paste(wd,'/raw/TX_SOUID132734.txt',sep=''))
  #' path2cclist<-system.file("extdata", "ECA_blend_source_cc.txt", package = "INQC")
  #' cclist<-readr::read_lines_raw(path2cclist)
  #' readr::write_lines(cclist,'ECA_blend_source_cc.txt')
  #' path2ccdata<-system.file("extdata", "CC_SOUID132727.txt", package = "INQC")
  #' ccdata<-readr::read_lines_raw(path2ccdata)
  #' readr::write_lines(ccdata, file=paste(wd,'/raw/CC_SOUID132727.txt',sep=''))
  #' #This is the MAIN starting point of the INQC software calculation:
  #' #inqc(homefolder='./',blend=TRUE) #work with 'blended' ECA&D data
  #' inqc(homefolder='./',blend=FALSE) #work with 'non-blended' ECA&D data
  #' #Remove some temporary files
  #' list = list.files(pattern = "Rfwf")
  #' file.remove(list)
  #' #Return to user's working directory:
  #' setwd(wd0)
  #' #The downloaded files can be found in the directory:
  #' print(wd)
  #' @export

  ##Two command below cause a note message during the check. It seems that packages aren't supposed to assign values to the global environment
  ##See for instance (https://stackoverflow.com/questions/28180989/r-cmd-check-found-the-following-assignments-to-the-global-environment)
  #assign("homefolder",homefolder,envir = .GlobalEnv)
  #assign("blend",blend,envir = .GlobalEnv)
  ##However, at the same site there is an example which should solve the problem. I tried to use this solution (see two commands below
  ##and my new function 'assign2global()')
  #assign2global("homefolder",homefolder,1L)
  #assign2global("blend",blend,1L)
  ##Unfortunately, this did not help.
  ##Here (https://stackoverflow.com/questions/12598242/global-variables-in-packages-in-r) there should be appropriate solutions for this problem (see below)
  #pkg.env <- new.env(parent = emptyenv())
  #pkg.env$homefolder <- './'
  #assign('homefolder', './', pkg.env)
  #pkg.env$blend <- TRUE
  #assign('blend', TRUE, pkg.env)
  #pkg.env$times.changed <- 0
  #assign('times.changed', 0, pkg.env)
  ##Unfortunately, this does not work as well. Actually, it is checked without any errors/warnings/notes but when running the package (after installation)
  ##pkg.env is not seen (it causes an error message).
  ##Here is another solution from the same site (https://stackoverflow.com/questions/12598242/global-variables-in-packages-in-r)
  
  #Suppress warning messages
  oldwarn <- getOption("warn")
  options(warn = -1)
  
  options("homefolder"=homefolder)
  options("blend"=blend)
  inithome()
  temperature(element='TX')
  temperature(element='TN')
  temperature(element='TG')
  precip()
  relhum()
  selepe()
  snowdepth()
  sundur()
  windspeed()
  clocov()
  dostats()
  
  options(warn = oldwarn)
}
