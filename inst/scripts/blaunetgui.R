
rm(list=ls())

require(Blaunet)
require(digest)
require(gWidgets2)
require(gWidgets2tcltk)
require(plot3D)
require(plot3Drgl)
require(network)
require(sna)
require(ergm)
require(haven)
require(foreign)

rm(list=ls())
oldwd <- getwd()
on.exit(setwd(oldwd))
blnetevn <- new.env()
clearmemory <- function(h,...) {
  rm(list=setdiff(ls(envir=.GlobalEnv),c(objls,"objls")),envir=.GlobalEnv)
  gmessage("The computer memory is successfully cleared.", parent = window)
  }

source('open.R')
source('browse.R')
source('network.R')
source('graph.R')
source('dimensions.R')
source('nicheplot.R')
source('analysis.R')
source('dynamics.R')
source('blaububbles.R')
showabout <- function(h,...) gmessage("Blaunet graphic package 2.2.1", parent = window)
commandpdf <- function(h,...) {
  if (Sys.info()[1]=="Windows") shell.exec(paste(.libPaths(), "/Blaunet/scripts/command.pdf", sep="")) 
  else if (Sys.info()[1]=="Darwin") system(paste("open ",.libPaths(), "/Blaunet/scripts/command.pdf", sep=""))
  else if (Sys.info()[1]=="Linux") system(paste("xdg-open ",.libPaths(), "/Blaunet/scripts/command.pdf", sep=""))
}
graphicpdf <- function(h,...) {
  if (Sys.info()[1]=="Windows") shell.exec(paste(.libPaths(), "/Blaunet/scripts/graphic.pdf", sep="")) 
  else if (Sys.info()[1]=="Darwin") system(paste("open ",.libPaths(), "/Blaunet/scripts/graphic.pdf", sep=""))
  else if (Sys.info()[1]=="Linux") system(paste("xdg-open ",.libPaths(), "/Blaunet/scripts/graphic.pdf", sep=""))
}
######################################################################
window <- gwindow("Blaunet", width = 1024, height = 600, visible=FALSE)
action_list = list(
  OpenFile = gaction(label = "Open Attribute File", handler = loadfile, parent = window),
  OpenFile1 = gaction(label = "Open Attribute File", icon = "open", icon.col=10, handler = loadfile, parent = window),
  OpenNet = gaction(label = "Open Network File", handler = loadnet, parent = window),
  OpenNet1 = gaction(label = "Open Network File", icon = "arrows", icon.col=2, handler = loadnet, parent = window),
  clear = gaction(label = "Clear Memory", handler = clearmemory, parent = window),
  clear1 = gaction(label = "Clear Memory", icon = "clear", icon.col=2, handler = clearmemory, parent = window),
  quit = gaction(label = "Quit", handler = function(...) dispose(window), parent = window),
  quit1 = gaction(label = "Quit", icon = "quit", icon.col=2, handler = function(...) dispose(window), parent = window),
  browsefile = gaction(label = "Browse Attribute File", handler = brattr, parent = window),
  browseadj = gaction(label = "Browse Adjcency Matrix", handler = bradj, parent = window),
  browseel = gaction(label = "Browse Network Edgelist", handler = brel, parent = window),
  Info = gaction(label = "Info", handler = showinfo, parent = window),
  Density = gaction(label = "Density", handler = showdensity, parent = window),
  Centrality = gaction(label = "Centrality", handler = showcentrality, parent = window),
  Dyad_census = gaction(label = "Dyad Census", handler = showdcensus, parent = window),
  ReciprocityIndex = gaction(label = "Reciprocity Index", handler = showreciprocityindex, parent = window),
  Triad_census = gaction(label = "Triad Census", handler = showtcensus, parent = window),
  GlobalClustering = gaction(label = "Global Clustering Coefficient", handler = showglobalcustering, parent = window),
  LocalClustering = gaction(label = "Local Clustering Coefficient", handler = showlocalcustering, parent = window),
  Graph = gaction(label = "Network Graph", handler = showgraph, parent = window),
  HistogramOutdegree = gaction(label = "Histogram Out-degree", handler = showhoutdegree, parent = window),
  HistogramIndegree = gaction(label = "Histogram In-degree", handler = showhindegree, parent = window),
  Dimensions = gaction(label = "Salient Dimensions", handler = showdimensions, parent = window), 
  Nicheplot= gaction(label = "Niche Plot", handler = nicheplot, parent = window),  
  Analysis = gaction(label = "Niche Analysis", handler = showanalysis, parent = window),
  Dynamics = gaction(label = "Niche Dynamics", handler = showdynamics, parent = window), 
  Blaububbles = gaction(label = "Blau Bubbles", handler = showblaububble, parent = window),  
  About = gaction(label = "About", handler = showabout, parent = window),
  Commandpdf = gaction(label = "Command Line Manual", handler = commandpdf, parent = window),
  Graphicpdf = gaction(label = "Graphic Package Manual", handler = graphicpdf, parent = window)
  )
tool_bar_list<- c(action_list$OpenFile1, 
                 sep = gseparator(),
                 action_list$OpenNet1,
                 sep = gseparator(),				 
                 action_list$clear1,
                 sep = gseparator(), 
                 action_list$quit1)
menu_bar_list <- list(Data = list(
             OpenFile = action_list$OpenFile,
             OpenNet = action_list$OpenNet,
             sep = gseparator(),
             Clear = action_list$clear,
             sep = gseparator(),
             Quit = action_list$quit
             ),
           Browse = list(
             attribute = action_list$browsefile,
             adjacency = action_list$browseadj,
             edgelist = action_list$browseel
             ),
           Network = list(
             Info = action_list$Info,
             Density = action_list$Density,
             Centrality = action_list$Centrality,
             DyadCensus = action_list$Dyad_census,
             ReciprocityIndex = action_list$ReciprocityIndex,
             TriadCcensus = action_list$Triad_census,
             GlobalClustering = action_list$GlobalClustering,
             LocalClustering = action_list$LocalClustering
             ),
           Graph = list(
             Graph = action_list$Graph,
             HistogramOutdegree = action_list$HistogramOutdegree,
             HistogramIndegree = action_list$HistogramIndegree
             ),
           Analysis = list(
             Dimensions = action_list$Dimensions,
             Nicheplot = action_list$Nicheplot,
             Analysis = action_list$Analysis,
             Dynamics = action_list$Dynamics,
             Blaububble = action_list$Blaububble
             ),
           Help = list(
             About = action_list$About,
             Gpdf = action_list$Graphicpdf,
             Cpdf = action_list$Commandpdf
             )
           )
group <- ggroup(horizontal = FALSE, spacing=1, cont = window)
menu_bar <- gmenu(menu_bar_list, cont = window)
tool_bar <- gtoolbar(tool_bar_list, style = c("both-horiz"), expand=TRUE, cont = window)
no_changes <- c("save","save.as","cut")
if (Sys.info()[1]=="Windows") {
  setwd(gsub("scripts", "data", getwd()))
  txt <- gedit(getwd(), cont = group)
  button <- gbutton("Set Working Directory", cont = group, handler = function(h, ...) setwd(choose.dir()))
  addHandlerChanged(button, handler=function(h,...) svalue(txt) <- getwd())
} else {
  setwd(gsub("scripts", "data", getwd()))
  txt <- gedit(getwd(), cont = group)
  button <- gbutton("Set working directory", cont = group, handler = function(h, ...) setwd(svalue(txt)))
}
glabel("Title: A Toolkit for Calculating, Visualizing, and Analyzing Social Distance Using Blau Status Analysis ", container=group, anchor=c(-1,1))
glabel("Depends: R (>= 3.0.0)", container=group, anchor=c(-1,1))
glabel("Imports: digest, gWidgets2, gWidgets2tcltk, plot3D, plot3Drgl, rgl, network, sna, ergm, statnet.common, haven, foreign", container=group, anchor=c(-1,1))
glabel("Version: 2.2.1", container=group, anchor=c(-1,1))
glabel("Authors: Cheng Wang*, Michael Genkin*, George Berry, Liyuan Chen, Matthew Brashears *Both authors contributed equally to this work and their names are randomly ordered", container=group, anchor=c(-1,1))
glabel("Maintainer: Cheng Wang <chengwang@wayne.edu>", container=group, anchor=c(-1,1))
glabel("Description: An integrated set of tools to calculate, visualize, and analyze positions in social distance between individuals belonging to (covert) organizational groups. 
                      Relational (network) data may be incorporated for additional analysis.", container=group, anchor=c(-1,1))
glabel("License: GPL-3", container=group, anchor=c(-1,1))
glabel("Blaunet Users Facebook group: https://www.facebook.com/groups/425015561030239/", container=group, anchor=c(-1,1))
glabel("Funding: This project is supported by Defense Threat Reduction Agency (DTRA) Grant HDTRA-10-1-0043.", container=group, anchor=c(-1,1))
glabel("Repository: CRAN", container=group, anchor=c(-1,1))
glabel("Date/Publication: 2022-09-26 09:28:06", container=group, anchor=c(-1,1))
sb <- gstatusbar("", container=window)
#id <- addHandlerUnrealize(window, handler = function(h,...) {!gconfirm("Really close", parent = h$obj)})
objls <- ls(envir=.GlobalEnv) 
visible(window) <- TRUE
