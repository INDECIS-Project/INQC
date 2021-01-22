inithome<-function(){

  #' Creates necessary folders (if not exist)
  #' @description This function will checks if all necessary folders ('QCSummary, QC and QCConsolidated) exist and if not, creates them.
  #' Not intended as a stand-alone function. Called from other routines.
  # @param homefolder homefolder, defaulted to './'
  #' @return it does not return any values, just creates the described folders if they do not exist
  #' @export

  #Get value of 'Global variable' 'homefolder'
  homefolder <- getOption("homefolder")
  if(!dir.exists(paste0(homefolder,'QC'))){dir.create(paste0(homefolder,'QC'))}
  if(!dir.exists(paste0(homefolder,'QCSummary'))){dir.create(paste0(homefolder,'QCSummary'))}
  if(!dir.exists(paste0(homefolder,'QCConsolidated'))){dir.create(paste0(homefolder,'QCConsolidated'))}
  suppressWarnings(listonator())
}
