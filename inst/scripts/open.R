loadfile <- function(h,...) {
  f <- gfile(text="Select a file", type="open")
  if (f=="") return;
  if (grepl('[:punct:.]rda',f) | grepl('[:punct:.]Rdata',f) | grepl('[:punct:.]dta',f) | grepl('[:punct:.]csv',f) | grepl('[:punct:.]sav',f) | grepl('[:punct:.]xpt',f) | grepl('[:punct:.]dat',f) | grepl('[:punct:.]DAT',f) | grepl('[:punct:.]txt',f)) {
    if (grepl('[:punct:.]rda',f) | grepl('[:punct:.]Rdata',f)) {
      c1 <- load(f)
      load(f,envir=blnetevn)
      if (class(get(c1))=="data.frame" | class(get(c1))=="matrix") {
        assign("cov",get(c1),envir=blnetevn)
      } else if (class(get(c1))=="list") {
        assign("adj",get(c1)$adj,envir=blnetevn)
        assign("el",get(c1)$el,envir=blnetevn)
        assign("cov",get(c1)$square.data,envir=blnetevn)
      }
      gmessage(paste("Congratulations! Your attribute file ",f," is now loaded",sep=''), parent = window)
    } else if (grepl('[:punct:.]dta',f)) {
      c1 <- data.frame(read_dta(f))
      assign("cov",c1,envir=blnetevn)
      gmessage(paste("Congratulations! Your attribute file ",f," is now loaded",sep=''), parent = window)
    } else if (grepl('[:punct:.]csv',f)) {
      c1 <- read.csv(f,header=T,sep=",")
      assign("cov",c1,envir=blnetevn)
      gmessage(paste("Congratulations! Your attribute file ",f," is now loaded",sep=''), parent = window)
    } else if (grepl('[:punct:.]sav',f)) {
      c1 <- read.spss(f)
      assign("cov",c1,envir=blnetevn)
      gmessage(paste("Congratulations! Your attribute file ",f," is now loaded",sep=''), parent = window)
    } else if (grepl('[:punct:.]xpt',f)) {
      c1 <- read.xport(f)
      assign("cov",c1,envir=blnetevn)
      gmessage(paste("Congratulations! Your attribute file ",f," is now loaded",sep=''), parent = window)
    } else if (grepl('[:punct:.]dat',f) | grepl('[:punct:.]DAT',f) | grepl('[:punct:.]txt',f)) {
      c1 <- read.table(f)
      assign("cov",c1,envir=blnetevn)
      gmessage(paste("Congratulations! Your attribute file ",f," is now loaded",sep=''), parent = window)
    }
  } else gmessage("Sorry! Unknown file format.", parent = window)
}

loadnet <- function(h,...) {
  f <- gfile(text="Select a file", type="open")
  if (f=="") return;
  if (grepl('[:punct:.]rda',f) | grepl('[:punct:.]Rdata',f) | grepl('[:punct:.]paj',f) | grepl('[:punct:.]dta',f) | grepl('[:punct:.]csv',f) | grepl('[:punct:.]sav',f) | grepl('[:punct:.]xpt',f) | grepl('[:punct:.]dat',f) | grepl('[:punct:.]DAT',f) | grepl('[:punct:.]net',f) | grepl('[:punct:.]txt',f)) {
    if (grepl('[:punct:.]rda',f) | grepl('[:punct:.]Rdata',f)) {
	  f1 <- get(load(f))
	  if (class(f1)=="data.frame") f1 <-as.matrix(f1)
      net <- network(f1)
    } else if (grepl('[:punct:.]paj',f)) {
	  f1 <- read.paj(f)
	  if (class(f1)=="data.frame") f1 <-as.matrix(f1)
      net <- network(f1)
    } else if (grepl('[:punct:.]dta',f)) {
	  f1 <- data.frame(read_dta(f))
	  f1 <-as.matrix(f1)
      net <- network(f1)
    } else if (grepl('[:punct:.]csv',f)) {
	  f1 <- read.csv(f,header=F,sep=",")
	  if (class(f1)=="data.frame") f1 <-as.matrix(f1)
      net <- network(f1)
    } else if (grepl('[:punct:.]sav',f)) {
	  f1 <- read.spss(f)
	  if (class(f1)=="data.frame") f1 <-as.matrix(f1)
      net <- network(f1)
    } else if (grepl('[:punct:.]xpt',f)) {
	  f1 <- read.xport(f)
	  if (class(f1)=="data.frame") f1 <-as.matrix(f1)
      net <- network(f1)
    } else if (grepl('[:punct:.]dat',f) | grepl('[:punct:.]DAT',f) | grepl('[:punct:.]net',f) | grepl('[:punct:.]txt',f)) {
      f1 <- read.table(f)
	  if (class(f1)=="data.frame") f1 <-as.matrix(f1)
	  net <- network(f1)
    }
    el <- data.frame(as.matrix(net,matrix.type='edgelist'))
    names(el) <- c("i","j") 
    adj <- data.frame(as.matrix(net))
    rownames(adj) <- colnames(adj) <- attr(as.matrix(net,matrix.type='edgelist'),"vnames")
    assign("adj",adj,envir=blnetevn)
    assign("el",el,envir=blnetevn)
    gmessage(paste("Congratulations! Your network file ",f," is now loaded",sep=''), parent = window)
  } else gmessage("Sorry! Unknown file format.", parent = window)
}


