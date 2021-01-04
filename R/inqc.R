inqc<-function(homefolder='./',blend=TRUE){

  #' Wrapper for QC'ing all variables
  #' @description This function calls functions which perform QC for all climate variables
  #' @param homefolder a path to the homefolder, as string
  #' @param blend a logical flag which means performing (if TRUE) QC on blended time series
  #' @return QC results, in both formats (verbose and workable file in exact ECA&D format)
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
}
