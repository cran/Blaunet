blaunetgui <- function(){
  oldwd <- getwd()
  on.exit(setwd(oldwd))
  setwd(paste(.libPaths(), "/Blaunet/scripts/", sep=""))
  source("blaunetgui.R")
}
