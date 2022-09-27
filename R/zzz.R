
.onAttach <- function(libname, pkgname){
  msg<-paste('BlauNet: Calculate and Analyze Blau Status for (Covert) Organizations\n')
  msg<-paste(msg,"Copyright (c) 2014-2022, Cheng Wang, Wayne State University\n",
"                         Michael Genkin, Singapore Management University\n",
"                         George Berry, Cornell University\n",
"                         Liyuan Chen\n",
"                         Matthew E. Brashears, University of South Carolina\n",sep="")
  msg<-paste(msg,'Type "blaunetgui()" to run analysis in graphic user interface (GUI).\n')
  packageStartupMessage(msg)
}