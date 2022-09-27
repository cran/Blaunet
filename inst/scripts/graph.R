showgraph <- function(h,...) {
  if ("adj" %in% ls(envir=blnetevn)) {
    n <- network(as.matrix(blnetevn$adj))
    assign("g1",0,envir=blnetevn)
    assign("g2",0,envir=blnetevn)
    assign("g3",0,envir=blnetevn)
    assign("g4",0,envir=blnetevn)
    assign("g5","fruchtermanreingold",envir=blnetevn)
    toplevel <- gwindow("Plot Graph", width = 400, height = 220, parent = window)
    cg <- ggroup(cont = toplevel, use.scrollwindow=T, horizontal = FALSE)
    tbl <- glayout(cont = cg)
    i <- 1
    tbl[i,1] <- gcheckbox("Show vertex name", cont=tbl, label="Show vertex name", handler = function(h,...) {
       assign("g1",svalue(h$obj),envir=blnetevn)
    })
	i <- i + 1
	tbl[i,1] <- "Vertex color by"
	tbl[i,2] <- gcombobox(c("None",names(blnetevn$cov)), selected = 1, cont = tbl, 
      handler = function(h,...){
        if (svalue(h$obj) %in% names(blnetevn$cov)) {
          assign("g2",which(names(blnetevn$cov) %in% svalue(h$obj)),envir=blnetevn)
        } else {assign("g2",0,envir=blnetevn)}
    })
	i <- i + 1
    tbl[i,1] <- "Vertex side by"
    tbl[i,2] <- gcombobox(c("None",names(blnetevn$cov)), selected = 1, cont = tbl, 
      handler = function(h,...){
        if (svalue(h$obj) %in% names(blnetevn$cov)) {
          assign("g3",which(names(blnetevn$cov) %in% svalue(h$obj)),envir=blnetevn)
        } else {assign("g3",0,envir=blnetevn)}
    })
	i <- i + 1
    tbl[i,1] <- "Vertex size by"
    tbl[i,2] <- gcombobox(c("None",names(blnetevn$cov)), selected = 1, cont = tbl, 
      handler = function(h,...){
        if (svalue(h$obj) %in% names(blnetevn$cov)) {
          assign("g4",which(names(blnetevn$cov) %in% svalue(h$obj)),envir=blnetevn)
        } else {assign("g4",0,envir=blnetevn)}
    })
	i <- i + 1
    tbl[i,1] <- "Layout"
    tbl[i,2] <- gcombobox(c("fruchtermanreingold","kamadakawai","spring","circle","eigen","hall","mds","princoord","target","random"), cont = tbl, 
       handler = function(h,...)assign("g5",svalue(h$obj),envir=blnetevn)
    )
    button <- gbutton("Plot Graph", cont = cg, handler = function(h, ...) {
      add_legend <- function(...) {
	    par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0),mar=c(0, 0, 0, 0), new=TRUE)
        plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
        legend(...)
      }
      if (blnetevn$g2>0) {
        temp1 <- sort(unique(blnetevn$cov[,blnetevn$g2]))
        temp2 <- as.character(temp1)
        temp3 <- rep("",nrow(blnetevn$cov))
        for (i in 1:nrow(blnetevn$cov)) temp3[i] <- rainbow(length(temp1))[which(temp1 %in% blnetevn$cov[i,blnetevn$g2])]
      }
      if (blnetevn$g3>0) {
        temp4 <- sort(unique(blnetevn$cov[,blnetevn$g3]))
        temp5 <- as.character(temp4)
        temp6 <- rep(0,nrow(blnetevn$cov))
        for (i in 1:nrow(blnetevn$cov)) temp6[i] <- which(temp4 %in% blnetevn$cov[i,blnetevn$g3])+2
        temp7 <- c("triangle","diamond","pentagon","hexagon","heptagon","octagon","nonagon","decagon","hendecagon","dodecagon")[1:length(temp5)]
        temp8 <- rep("",length(temp5))
        for (i in 1:length(temp5)) temp8[i] <- paste(temp7[i],":",temp5[i],sep="")
      }
	  oldpar <- par(no.readonly = TRUE) 
      on.exit(par(oldpar))
      par(mar = c(5, 4, 1.4, 0.2))
      if (blnetevn$g1==FALSE & blnetevn$g2==0 & blnetevn$g3==0 & blnetevn$g4==0) gplot(n,mode=blnetevn$g5)
      if (blnetevn$g1==TRUE & blnetevn$g2==0 & blnetevn$g3==0 & blnetevn$g4==0) gplot(n, label=network.vertex.names(n),mode=blnetevn$g5)
      if (blnetevn$g1==TRUE & blnetevn$g2==0 & blnetevn$g3>0 & blnetevn$g4==0) gplot(n, label=network.vertex.names(n), vertex.sides=temp6,vertex.cex=1.5,mode=blnetevn$g5)
      if (blnetevn$g1==FALSE & blnetevn$g2>0 & blnetevn$g3==0 & blnetevn$g4==0) gplot(n, vertex.col=temp3,mode=blnetevn$g5)
      if (blnetevn$g1==FALSE & blnetevn$g2>0 & blnetevn$g3>0 & blnetevn$g4==0) gplot(n, vertex.col=temp3,vertex.sides=temp6,vertex.cex=1.5,mode=blnetevn$g5)
      if (blnetevn$g1==FALSE & blnetevn$g2==0 & blnetevn$g3==0 & blnetevn$g4>0) gplot(n, vertex.cex=blnetevn$cov[,blnetevn$g4]/mean(blnetevn$cov[,blnetevn$g4]),mode=blnetevn$g5)
      if (blnetevn$g1==FALSE & blnetevn$g2==0 & blnetevn$g3>0 & blnetevn$g4>0) gplot(n, vertex.cex=blnetevn$cov[,g4]/mean(blnetevn$cov[,g4]),vertex.sides=temp6,mode=blnetevn$g5)
      if (blnetevn$g1==FALSE & blnetevn$g2>0 & blnetevn$g3==0 & blnetevn$g4>0) gplot(n, vertex.col=temp3, vertex.cex=blnetevn$cov[,g4]/mean(blnetevn$cov[,g4]),mode=blnetevn$g5)
      if (blnetevn$g1==FALSE & blnetevn$g2>0 & blnetevn$g3>0 & blnetevn$g4>0) gplot(n, vertex.col=temp3, vertex.cex=blnetevn$cov[,g4]/mean(blnetevn$cov[,g4]),vertex.sides=temp6,mode=blnetevn$g5)
      if (blnetevn$g1==TRUE & blnetevn$g2>0 & blnetevn$g3==0 & blnetevn$g4==0) gplot(n, label=network.vertex.names(n), vertex.col=temp3,mode=blnetevn$g5)
      if (blnetevn$g1==TRUE & blnetevn$g2>0 & blnetevn$g3>0 & blnetevn$g4==0) gplot(n, label=network.vertex.names(n), vertex.col=temp3,vertex.sides=temp6,vertex.cex=1.5,mode=blnetevn$g5)
      if (blnetevn$g1==TRUE & blnetevn$g2==0 & blnetevn$g3==0 & blnetevn$g4>0) gplot(n, label=network.vertex.names(n), vertex.cex=blnetevn$cov[,blnetevn$g4]/mean(blnetevn$cov[,blnetevn$g4]),mode=blnetevn$g5)
      if (blnetevn$g1==TRUE & blnetevn$g2==0 & blnetevn$g3>0 & blnetevn$g4>0) gplot(n, label=network.vertex.names(n), vertex.cex=blnetevn$cov[,blnetevn$g4]/mean(blnetevn$cov[,blnetevn$g4]),vertex.sides=temp6,mode=blnetevn$g5)
      if (blnetevn$g1==TRUE & blnetevn$g2>0 & blnetevn$g3==0 & blnetevn$g4>0) gplot(n, label=network.vertex.names(n), vertex.col=temp3, vertex.cex=blnetevn$cov[,blnetevn$g4]/mean(blnetevn$cov[,blnetevn$g4]),mode=blnetevn$g5)
      if (blnetevn$g1==TRUE & blnetevn$g2>0 & blnetevn$g3>0 & blnetevn$g4>0) gplot(n, label=network.vertex.names(n), vertex.col=temp3, vertex.cex=blnetevn$cov[,blnetevn$g4]/mean(blnetevn$cov[,blnetevn$g4]),vertex.sides=temp6,mode=blnetevn$g5)
      if (blnetevn$g2>0) {
        add_legend("topleft",temp2,text.col=rainbow(length(temp1)),bty='n',title = names(blnetevn$cov)[blnetevn$g2])
      }
      if (blnetevn$g3>0) {
        add_legend("bottomleft",temp8,bty='n',title = names(blnetevn$cov)[blnetevn$g3])
      }
      par(mar=c(5, 4, 4, 2) + 0.1)
    })
  } else gmessage("Sorry! Network file is not loaded.", parent = window)
}

showhoutdegree <- function(h,...) {
  if ("adj" %in% ls(envir=blnetevn)) {
    outdegree <- degree(network(as.matrix(blnetevn$adj)),cmode="outdegree")
    hist(outdegree)
  } else gmessage("Sorry! Network file is not loaded.", parent = window)
}

showhindegree <- function(h,...) {
  if ("adj" %in% ls(envir=blnetevn)) {
    indegree <- degree(network(as.matrix(blnetevn$adj)),cmode="indegree")
    hist(indegree)
  } else gmessage("Sorry! Network file is not loaded.", parent = window)
}

