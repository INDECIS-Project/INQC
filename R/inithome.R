inithome<-function(){

  #' Create necessary folders (if not exist)
  #' @description This function will checks if all necessary folders ('QCSummary, QC and QCConsolidated) exist and if not, creates them. Not intended as a stand-alone function.
  #' Called from other routinees
  # @param homefolder homefolder, defaulted to './'
  #' @return does not return any values, creates the described folders if they do not exist
  #' @export

  ##This is old version (v1.0)
  ##**************************
  #if(!dir.exists(paste0(home,'QC'))){dir.create(paste0(home,'QC)'))}
  #if(!dir.exists(paste0(home,'QCSummary'))){dir.create(paste0(home,'QCSummary)'))}
  #if(!dir.exists(paste0(home,'QCConsolidated'))){dir.create(paste0(home,'QCConsolidated'))}
  #OS: I commented one line below
  #rm(liston,envir=.GlobalEnv)
  #listonator(home)
  ##**************************

  #Get value of 'Global variable' 'homefolder'
  homefolder <- getOption("homefolder")
  if(!dir.exists(paste0(homefolder,'QC'))){dir.create(paste0(homefolder,'QC'))}
  if(!dir.exists(paste0(homefolder,'QCSummary'))){dir.create(paste0(homefolder,'QCSummary'))}
  if(!dir.exists(paste0(homefolder,'QCConsolidated'))){dir.create(paste0(homefolder,'QCConsolidated'))}
  #OS: I commented one line below
  #rm(liston,envir=.GlobalEnv)
  listonator()
}
