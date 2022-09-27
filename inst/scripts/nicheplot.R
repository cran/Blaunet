nicheplot <- function(h,...) {
  if ("cov" %in% ls(envir=blnetevn)==FALSE) {gmessage("Sorry! Attribute file is not loaded.", parent = window)} else {
    assign("d1",character(0),envir=blnetevn)
    assign("d2",character(0),envir=blnetevn)
    assign("d3","",envir=blnetevn)
    assign("d4",character(0),envir=blnetevn)
    assign("d5",character(0),envir=blnetevn)
    assign("d7",character(0),envir=blnetevn)
    assign("d8","FALSE",envir=blnetevn)
    assign("d9",character(0),envir=blnetevn)
    assign("m1",names(blnetevn$cov),envir=blnetevn)
    assign("m3",names(blnetevn$cov),envir=blnetevn)
    toplevel <- gwindow("Niche Plot", width=800, height=800, parent = window, visible=FALSE)
	cg <- ggroup(horizontal = TRUE,cont = toplevel)
	tbl0 <- gtable(blnetevn$m1,expand=TRUE,multiple=TRUE,cont=cg)
	cg1 <- ggroup(horizontal = FALSE, cont = cg)
	addSpring(cg1)
    gbplus1 <- gbutton("+", cont=cg1)
    gbminus1 <- gbutton("-", cont=cg1)
	addSpring(cg1)
	addSpring(cg1)
    gbplus2 <- gbutton("+", cont=cg1)
    gbminus2 <- gbutton("-", cont=cg1)
	addSpring(cg1)
    addSpring(cg1)
    addSpring(cg1)
	addSpring(cg1)
    addSpring(cg1)
	addSpring(cg1)
    addSpring(cg1)
    addSpring(cg1)
    gbplus4 <- gbutton("+", cont=cg1)
    gbminus4 <- gbutton("-", cont=cg1)
    addSpring(cg1)
    addSpring(cg1)
    addSpring(cg1)
    addSpring(cg1)
    addSpring(cg1)
    gbplus5 <- gbutton("+", cont=cg1)
    gbminus5 <- gbutton("-", cont=cg1)
    addSpring(cg1)
    addSpring(cg1)
    addSpring(cg1)
    addSpring(cg1) 
	gbplus7 <- gbutton("+", cont=cg1)
    gbminus7 <- gbutton("-", cont=cg1)
    addSpring(cg1)
    addSpring(cg1)
    addSpring(cg1)
    addSpring(cg1)
	addSpring(cg1)
    addSpring(cg1)
	addSpring(cg1)
	cg2 <- ggroup(horizontal = FALSE, cont = cg)
    d1temp <- data.frame(Node.ids="",stringsAsFactors=FALSE)
    d2temp <- data.frame(Ecology.ids="",stringsAsFactors=FALSE)
    d4temp <- data.frame(Dimensions=rep("",length(blnetevn$m3)),stringsAsFactors=FALSE)
    d5temp <- data.frame(Groups=rep("",length(blnetevn$m3)),stringsAsFactors=FALSE)
    d7temp <- data.frame(Weights="",stringsAsFactors=FALSE)
    tbl1 <- gtable(d1temp,expand=TRUE,multiple=FALSE,cont=cg2)
	size(tbl1)[2] <- 50
    tbl2 <- gtable(d2temp,expand=TRUE,multiple=FALSE,cont=cg2)
	size(tbl2)[2] <- 50
    if (('el' %in% ls(envir=blnetevn))==FALSE) {
      gcheckboxgroup("Show nodes",cont=cg2,handler = function(h,...) assign("d3",svalue(h$obj),envir=blnetevn))
    }
    if ('el' %in% ls(envir=blnetevn)) {
      gradio(c("Do not show nodes and network ties","Show nodes","Show nodes and network ties"),selected=1,cont=cg2,handler = function(h,...) assign("d3",svalue(h$obj),envir=blnetevn))
    }
    tbl4 <- gtable(d4temp,expand=TRUE,multiple=TRUE,cont=cg2)
	size(tbl4)[2] <- 120
    tbl5 <- gtable(d5temp,expand=TRUE,multiple=TRUE,cont=cg2)
	size(tbl5)[2] <- 120
    tbl7 <- gtable(d7temp,expand=TRUE,multiple=FALSE,cont=cg2)
	size(tbl7)[2] <- 50
    glabel("Complete.cases",cont=cg2)
    gradio(c("TRUE","FALSE"), selected = 2, cont = cg2,  handler = function(h,...) assign("d8",svalue(h$obj),envir=blnetevn))
    addHandlerClicked(gbplus1, handler = function(h,...) {
      temp <- svalue(tbl0)
      if ("" %in% temp==FALSE & tbl1[1]=="" & length(temp)==1) {
        assign("m1",blnetevn$m3[which(blnetevn$m3 %in% setdiff(blnetevn$m1,temp))],envir=blnetevn) 
        km1 <- c(blnetevn$m1,rep("",length(blnetevn$m3)-length(blnetevn$m1)))
        for (i in 1:length(blnetevn$m3)) tbl0[i] <- km1[i]
        assign("d1",blnetevn$m3[which(blnetevn$m3 %in% union(blnetevn$d1,temp))],envir=blnetevn) 
        tbl1[1] <- blnetevn$d1
      }
    })
    addHandlerClicked(gbminus1, handler = function(h,...) {
      temp <- svalue(tbl1)
      if ("" %in% temp==FALSE & length(temp)==1) {
        assign("m1",blnetevn$m3[which(blnetevn$m3 %in% union(blnetevn$m1,temp))],envir=blnetevn) 
        km1 <- c(blnetevn$m1,rep("",length(blnetevn$m3)-length(blnetevn$m1)))
        for (i in 1:length(blnetevn$m3)) tbl0[i] <- km1[i]
        assign("d1",character(0),envir=blnetevn) 
        tbl1[1] <- ""
      }
    })
    addHandlerClicked(gbplus2, handler = function(h,...) {
      temp <- svalue(tbl0)
      if ("" %in% temp==FALSE & tbl2[1]=="" & length(temp)==1) {
        assign("m1",blnetevn$m3[which(blnetevn$m3 %in% setdiff(blnetevn$m1,temp))],envir=blnetevn) 
        km1 <- c(blnetevn$m1,rep("",length(blnetevn$m3)-length(blnetevn$m1)))
        for (i in 1:length(blnetevn$m3)) tbl0[i] <- km1[i]
        assign("d2",blnetevn$m3[which(blnetevn$m3 %in% union(blnetevn$d2,temp))],envir=blnetevn) 
        tbl2[1] <- blnetevn$d2
      }
    })
    addHandlerClicked(gbminus2, handler = function(h,...) {
      temp <- svalue(tbl2)
      if ("" %in% temp==FALSE & length(temp)==1) {
        assign("m1",blnetevn$m3[which(blnetevn$m3 %in% union(blnetevn$m1,temp))],envir=blnetevn) 
        km1 <- c(blnetevn$m1,rep("",length(blnetevn$m3)-length(blnetevn$m1)))
        for (i in 1:length(blnetevn$m3)) tbl0[i] <- km1[i]
        assign("d2",character(0),envir=blnetevn) 
        tbl2[1] <- ""
      }
    })
    addHandlerClicked(gbplus4, handler = function(h,...) {
      temp <- svalue(tbl0)
      if ("" %in% temp==FALSE & length(temp)>0) {
        assign("m1",blnetevn$m3[which(blnetevn$m3 %in% setdiff(blnetevn$m1,temp))],envir=blnetevn) 
        km1 <- c(blnetevn$m1,rep("",length(blnetevn$m3)-length(blnetevn$m1)))
        for (i in 1:length(blnetevn$m3)) tbl0[i] <- km1[i]
        assign("d4",blnetevn$m3[which(blnetevn$m3 %in% union(blnetevn$d4,temp))],envir=blnetevn) 
        kd4 <- c(blnetevn$d4,rep("",length(blnetevn$m3)-length(blnetevn$d4)))
        for (j in 1:length(blnetevn$m3)) tbl4[j] <- kd4[j]
      }
    })
    addHandlerClicked(gbminus4, handler = function(h,...) {
      temp <- svalue(tbl4)
      if ("" %in% temp==FALSE & length(temp)>0) {
        assign("m1",blnetevn$m3[which(blnetevn$m3 %in% union(blnetevn$m1,temp))],envir=blnetevn) 
        km1 <- c(blnetevn$m1,rep("",length(blnetevn$m3)-length(blnetevn$m1)+1))
        for (i in 1:length(blnetevn$m3)) tbl0[i] <- km1[i]
        assign("d4",blnetevn$m3[which(blnetevn$m3 %in% setdiff(blnetevn$d4,temp))],envir=blnetevn) 
        kd4 <- c(blnetevn$d4,rep("",length(blnetevn$m3)-length(blnetevn$d4)+1))
        for (j in 1:length(blnetevn$m3)) tbl4[j] <- kd4[j]
      }
    })
    addHandlerClicked(gbplus5, handler = function(h,...) {
      temp <- svalue(tbl0)
      if ("" %in% temp==FALSE & length(temp)>0) {
        assign("m1",blnetevn$m3[which(blnetevn$m3 %in% setdiff(blnetevn$m1,temp))],envir=blnetevn) 
        km1 <- c(blnetevn$m1,rep("",length(blnetevn$m3)-length(blnetevn$m1)))
        for (i in 1:length(blnetevn$m3)) tbl0[i] <- km1[i]
        assign("d5",blnetevn$m3[which(blnetevn$m3 %in% union(blnetevn$d5,temp))],envir=blnetevn) 
        kd5 <- c(blnetevn$d5,rep("",length(blnetevn$m3)-length(blnetevn$d5)))
        for (j in 1:length(blnetevn$m3)) tbl5[j] <- kd5[j]
      }
    })
    addHandlerClicked(gbminus5, handler = function(h,...) {
      temp <- svalue(tbl5)
      if ("" %in% temp==FALSE & length(temp)>0) {
        assign("m1",blnetevn$m3[which(blnetevn$m3 %in% union(blnetevn$m1,temp))],envir=blnetevn) 
        km1 <- c(blnetevn$m1,rep("",length(blnetevn$m3)-length(blnetevn$m1)+1))
        for (i in 1:length(blnetevn$m3)) tbl0[i] <- km1[i]
        assign("d5",blnetevn$m3[which(blnetevn$m3 %in% setdiff(blnetevn$d5,temp))],envir=blnetevn) 
        kd5 <- c(blnetevn$d5,rep("",length(blnetevn$m3)-length(blnetevn$d5)+1))
        for (j in 1:length(blnetevn$m3)) tbl5[j] <- kd5[j]
      }
    })
    addHandlerClicked(gbplus7, handler = function(h,...) {
      temp <- svalue(tbl0)
      if ("" %in% temp==FALSE & tbl7[1]=="" & length(temp)==1) {
        assign("m1",blnetevn$m3[which(blnetevn$m3 %in% setdiff(blnetevn$m1,temp))],envir=blnetevn) 
        km1 <- c(blnetevn$m1,rep("",length(blnetevn$m3)-length(blnetevn$m1)))
        for (i in 1:length(blnetevn$m3)) tbl0[i] <- km1[i]
        assign("d7",blnetevn$m3[which(blnetevn$m3 %in% union(blnetevn$d7,temp))],envir=blnetevn) 
        tbl7[1] <- blnetevn$d7
      }
    })
    addHandlerClicked(gbminus7, handler = function(h,...) {
      temp <- svalue(tbl7)
      if ("" %in% temp==FALSE & length(temp)==1) {
        assign("m1",blnetevn$m3[which(blnetevn$m3 %in% union(blnetevn$m1,temp))],envir=blnetevn) 
        km1 <- c(blnetevn$m1,rep("",length(blnetevn$m3)-length(blnetevn$m1)))
        for (i in 1:length(blnetevn$m3)) tbl0[i] <- km1[i]
        assign("d7",character(0),envir=blnetevn) 
        tbl7[1] <- ""
      }
    })
	visible(toplevel) <- TRUE
    gbutton("Continue", expand = FALSE, cont = cg2, handler = function(h, ...) {
      if (length(blnetevn$d1)==0 | length(blnetevn$d4)==0 | length(blnetevn$d5)==0) {gmessage("Missing required information.", parent = toplevel)} else {
        dispose(toplevel)
        if (length(blnetevn$d7)>0) tmpweight <- blnetevn$d7 else tmpweight <- NULL  
        bcolors <- rep(c(26,116,142,47,8,12,31,32,33,41,51,53,62,139,151,175,153,85,450,477),5)
        if (length(blnetevn$d2)>0) {
          extralevel <- gwindow("Niche Plot Options", width=600, height=600, parent = window)
          extrag <- ggroup(cont = extralevel, use.scrollwindow=T, horizontal = FALSE)
		  extragv <- gvbox(cont = extrag)
          tbl <- gformlayout(cont = extragv)
		  gcheckboxgroup(c("all",as.matrix(unique(blnetevn$cov[which(colnames(blnetevn$cov)==blnetevn$d2)]))), cont = tbl, label="Please select which ecology you want to make niche plot:", handler = function(h,...){
            assign("d9",svalue(h$obj),envir=blnetevn)
          })
          assign("d6",rep(1.5,length(blnetevn$d4)),envir=blnetevn)
          gseparator(cont = extrag)
          glabel("Dev.range",cont=extrag)
          for (da in 1:length(blnetevn$d4)) {
            glabel(blnetevn$d4[da],cont=extrag)
            assign(paste0("d6da", da),gslider(from = 0, to = 5, by = .05, value = 1.5, cont=extrag, handler = function(h,...) svalue(get(paste0("d6da", da))) ))
          } 
          addSpring(extrag)
          addSpring(extrag)
          addSpring(extrag)
          button <- gbutton("Continue", expand = FALSE, cont = extrag, handler = function(h, ...) {
            d6tmp <- c()
            for (da in 1:length(blnetevn$d4)) {
              d6tmp <- c(d6tmp,svalue(get(paste0("d6da", da))))
            }
            assign("d6",d6tmp,envir=blnetevn)
			dispose(extralevel)
            if ("all" %in% blnetevn$d9) {
              cov1 <- blnetevn$cov
              if ('el' %in% ls(envir=blnetevn)) el1 <- blnetevn$el
            } else {
              cov1 <- blnetevn$cov[which(blnetevn$cov[,which(colnames(blnetevn$cov)==blnetevn$d2)] %in% blnetevn$d9),] 
              if ('el' %in% ls(envir=blnetevn)) {
                el1 <- blnetevn$el[which(blnetevn$el[,1] %in% unique(cov1[,which(colnames(cov1)==blnetevn$d1)]) & blnetevn$el[,2] %in% unique(cov1[,which(colnames(cov1)==blnetevn$d1)])),]
              }
            }
            if (('el' %in% ls(envir=blnetevn))==FALSE) {
              b <- blau(cov1, node.ids=blnetevn$d1, ecology.ids=blnetevn$d2, dimension=blnetevn$d4, memberships=blnetevn$d5,weights=tmpweight,complete.cases=d8)
              b <- niches(b, dev.range = blnetevn$d6)
            }
            if ('el' %in% ls(envir=blnetevn)) {
              b <- blau(cov1, node.ids=blnetevn$d1, ecology.ids=blnetevn$d2, graph = el1, dimension=blnetevn$d4, memberships=blnetevn$d5,weights=tmpweight,complete.cases=blnetevn$d8 )
              b <- niches(b, dev.range = blnetevn$d6)
            }
            assign("bobj",b,envir=blnetevn)
            if (length(blnetevn$d4)==2) {
			  oldpar <- par(no.readonly = TRUE) 
              on.exit(par(oldpar)) 
              lowb <- b$lowbounds
              topb <- b$topbounds
              k1 <- unique(b$ids$ecologyId)
              k2 <- c(which(names(cov1)==blnetevn$d4[1]),which(names(cov1)==blnetevn$d4[2]))
              niche2d1 <- function(xa,xb,ya,yb) {
                add_legend <- function(...) {
                  par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), mar=c(0, 0, 0, 0), new=TRUE)
                  plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
                  legend(...)
                }
                plot(-10000,-10000,xlim=c(xa,xb),ylim=c(ya,yb), xlab=blnetevn$d4[1], ylab=blnetevn$d4[2])
                if (length(grep('Show nodes',blnetevn$d3))==1) {
                  plot(cov1[,k2[1]],cov1[,k2[2]],xlim=c(xa,xb),ylim=c(ya,yb), xlab=blnetevn$d4[1], ylab=blnetevn$d4[2], col="red")
                }
                bcol <- 1
                for (i in 1:length(k1)) {
                  for (j in 1:length(blnetevn$d5)) {
                    k <- (i-1)*length(blnetevn$d5)+j
                    rect(lowb[k,1], lowb[k,2], topb[k,1], topb[k,2], border=colors()[bcolors[bcol]])
                    bcol <- bcol+1
                  }
                }        
                if (blnetevn$d3=="Show nodes and network ties") {
                  x1 <- y1 <- x2 <- y2 <- rep(0,nrow(blnetevn$el))
                  for (i in 1:nrow(el1)) {
                    s <- which(as.character(el1[i,1])==as.character(blnetevn$cov[,blnetevn$d1]))
                    x1[i] <- blnetevn$cov[s,k2[1]]
                    y1[i] <- blnetevn$cov[s,k2[2]]
                    t <- which(as.character(el1[i,2])==as.character(blnetevn$cov[,blnetevn$d1]))
                    x2[i] <- blnetevn$cov[t,k2[1]]
                    y2[i] <- blnetevn$cov[t,k2[2]]
                  }
                  segments(x1,y1,x2,y2,col="red")
                }
                add_legend("topleft",paste(rep(k1,each=length(blnetevn$d5)),"_",rownames(lowb),sep=""),text.col=colors()[bcolors[1:nrow(lowb)]],bty='n')
              }
              xa <- min(lowb[,1],cov1[,k2[1]],na.rm = TRUE)
              xb <- max(topb[,1],cov1[,k2[1]],na.rm = TRUE)
              ya <- min(lowb[,2],cov1[,k2[2]],na.rm = TRUE)
              yb <- max(topb[,2],cov1[,k2[2]],na.rm = TRUE)
              niche2d1(xa,xb,ya,yb)
            } else if (length(blnetevn$d4) == 3) {
              lowb <- b$lowbounds
              topb <- b$topbounds
              k1 <- unique(b$ids$ecologyId)
              k2 <- c(which(names(blnetevn$cov)==blnetevn$d4[1]),which(names(blnetevn$cov)==blnetevn$d4[2]),which(names(blnetevn$cov)==blnetevn$d4[3]))
              niche3drgl1 <- function() {
                scatter3Drgl(-10000,-10000,-10000, 
                       xlim=c(min(lowb[,1],blnetevn$cov[,k2[1]],na.rm = TRUE),max(topb[,1],blnetevn$cov[,k2[1]],na.rm = TRUE)),
                       ylim=c(min(lowb[,2],blnetevn$cov[,k2[2]],na.rm = TRUE),max(topb[,2],blnetevn$cov[,k2[2]],na.rm = TRUE)),
                       zlim=c(min(lowb[,3],blnetevn$cov[,k2[3]],na.rm = TRUE),max(topb[,3],blnetevn$cov[,k2[3]],na.rm = TRUE)),
                       xlab=blnetevn$d4[1],ylab=blnetevn$d4[2],zlab=blnetevn$d4[3],
                       colkey = FALSE, col="red")
                par3d(windowRect = c(900, 50, 1500, 650))
                if (length(grep('Show nodes',blnetevn$d3))==1) {
                  rgl.close()
                  scatter3Drgl(blnetevn$cov[,k2[1]],blnetevn$cov[,k2[2]],blnetevn$cov[,k2[3]], 
                        xlim=c(min(lowb[,1],blnetevn$cov[,k2[1]],na.rm = TRUE),max(topb[,1],blnetevn$cov[,k2[1]],na.rm = TRUE)),
                        ylim=c(min(lowb[,2],blnetevn$cov[,k2[2]],na.rm = TRUE),max(topb[,2],blnetevn$cov[,k2[2]],na.rm = TRUE)),
                        zlim=c(min(lowb[,3],blnetevn$cov[,k2[3]],na.rm = TRUE),max(topb[,3],blnetevn$cov[,k2[3]],na.rm = TRUE)),
                        xlab=blnetevn$d4[1],ylab=blnetevn$d4[2],zlab=blnetevn$d4[3],
                        colkey = FALSE, col="red")
                  par3d(windowRect = c(900, 50, 1500, 650))
                }
                axes3d()
                box3Drgl(lowb[,1], lowb[,2], lowb[,3],
                      topb[,1], topb[,2], topb[,3], 
                      add = TRUE,
                      col = colors()[bcolors[1:nrow(lowb)]], alpha = 0.5,
                      border = "black", lwd = 2)
                if (blnetevn$d3=="Show nodes and network ties") {
                  x1 <- y1 <- z1 <- x2 <- y2 <- z2 <- rep(0,nrow(el1))
                  for (i in 1:nrow(el1)) {
                    s <- which(as.character(el1[i,1])==as.character(cov1[,blnetevn$d1]))
                    x1[i] <- cov1[s,k2[1]]
                    y1[i] <- cov1[s,k2[2]]
                    z1[i] <- cov1[s,k2[3]]
                    t <- which(as.character(el1[i,2])==as.character(cov1[,blnetevn$d1]))
                    x2[i] <- cov1[t,k2[1]]
                    y2[i] <- cov1[t,k2[2]]
                    z2[i] <- cov1[t,k2[3]]
                  }
                  segments3Drgl(x1,y1,z1,x2,y2,z2,add = TRUE,col="red")
                }
                legend3d("topleft",paste(rep(k1,each=length(blnetevn$d5)),"_",rownames(lowb),sep=""),text.col=colors()[bcolors[1:nrow(lowb)]],bty='n')
              } 
              niche3drgl1()
            } else {
              dispose(toplevel)
              gmessage("You must select 2  or 3 dimensions.")
            }
          })  
        } else {
          extralevel <- gwindow("Dev.range", width=600, height=600, parent = window)
          extrag <- ggroup(cont = extralevel, use.scrollwindow=T, horizontal = FALSE)
          assign("d6",rep(1.5,length(blnetevn$d4)),envir=blnetevn)
          glabel("Dev.range",cont=extrag)
          for (da in 1:length(blnetevn$d4)) {
            glabel(blnetevn$d4[da],cont=extrag)
            assign(paste0("d6da", da),gslider(from = 0, to = 5, by = .05, value = 1.5, cont=extrag, handler = function(h,...) svalue(get(paste0("d6da", da))) ))
          } 
          addSpring(extrag)
          addSpring(extrag)
          addSpring(extrag)
          button <- gbutton("Continue", expand = FALSE, cont = extrag, handler = function(h, ...) {
            dispose(extralevel)
            if (('el' %in% ls(envir=blnetevn))==FALSE) {
              b <- blau(blnetevn$cov, node.ids=blnetevn$d1, dimension=blnetevn$d4, memberships=blnetevn$d5,weights=tmpweight,complete.cases=blnetevn$d8)
              b <- niches(b, dev.range = blnetevn$d6)
            }
             if ('el' %in% ls(envir=blnetevn)) {
              b <- blau(blnetevn$cov, node.ids=blnetevn$d1, graph = blnetevn$el, dimension=blnetevn$d4, memberships=blnetevn$d5,weights=tmpweight,complete.cases=blnetevn$d8)
              b <- niches(b, dev.range = blnetevn$d6)
            }
            assign("bobj",b,envir=blnetevn)
            if (length(blnetevn$d4)==2) {
			  oldpar <- par(no.readonly = TRUE) 
              on.exit(par(oldpar)) 
              lowb <- b$lowbounds
              topb <- b$topbounds
              k2 <- c(which(names(blnetevn$cov)==blnetevn$d4[1]),which(names(blnetevn$cov)==blnetevn$d4[2]))
              niche2d2 <- function(xa,xb,ya,yb) {
                add_legend <- function(...) {
                  par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0),mar=c(0, 0, 0, 0), new=TRUE)
                  plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
                  legend(...)
                }
                plot(-10000,-10000,xlim=c(xa,xb),ylim=c(ya,yb), xlab=blnetevn$d4[1], ylab=blnetevn$d4[2])
                if (length(grep('Show nodes',blnetevn$d3))==1) {
                  plot(blnetevn$cov[,k2[1]],blnetevn$cov[,k2[2]],xlim=c(xa,xb),ylim=c(ya,yb), xlab=blnetevn$d4[1], ylab=blnetevn$d4[2],col="red")
                }
                for (i in 1:length(blnetevn$d5)) {
                  rect(lowb[i,1], lowb[i,2], topb[i,1], topb[i,2], border=colors()[bcolors[i]])
                }
                if (blnetevn$d3=="Show nodes and network ties") {
                  r <- which(blnetevn$d1==names(blnetevn$cov))
                  x1 <- y1 <- x2 <- y2 <- rep(0,nrow(blnetevn$el))
                  for (i in 1:nrow(blnetevn$el)) {
                    s <- which(as.character(el[i,1])==as.character(blnetevn$cov[,r]))
                    x1[i] <- blnetevn$cov[s,k2[1]]
                    y1[i] <- blnetevn$cov[s,k2[2]]
                    t <- which(as.character(el[i,2])==as.character(blnetevn$cov[,r]))
                    x2[i] <- blnetevn$cov[t,k2[1]]
                    y2[i] <- blnetevn$cov[t,k2[2]]
                  }
                  segments(x1,y1,x2,y2,col="red")
                }  
                add_legend("topleft",blnetevn$d5,text.col=colors()[bcolors[1:nrow(lowb)]],bty='n')
              }
              xa <- min(lowb[,1],blnetevn$cov[,k2[1]],na.rm = TRUE)
              xb <- max(topb[,1],blnetevn$cov[,k2[1]],na.rm = TRUE)
              ya <- min(lowb[,2],blnetevn$cov[,k2[2]],na.rm = TRUE)
              yb <- max(topb[,2],blnetevn$cov[,k2[2]],na.rm = TRUE)
              niche2d2(xa,xb,ya,yb)       
            } else if (length(blnetevn$d4) == 3) {
              lowb <- b$lowbounds
              topb <- b$topbounds
              k2 <- c(which(names(blnetevn$cov)==blnetevn$d4[1]),which(names(blnetevn$cov)==blnetevn$d4[2]),which(names(blnetevn$cov)==blnetevn$d4[3]))
              niche3drgl2 <- function() {
                scatter3Drgl(-10000,-10000,-10000, 
                       xlim=c(min(lowb[,1],blnetevn$cov[,k2[1]],na.rm = TRUE),max(topb[,1],blnetevn$cov[,k2[1]],na.rm = TRUE)),
                       ylim=c(min(lowb[,2],blnetevn$cov[,k2[2]],na.rm = TRUE),max(topb[,2],blnetevn$cov[,k2[2]],na.rm = TRUE)),
                       zlim=c(min(lowb[,3],blnetevn$cov[,k2[3]],na.rm = TRUE),max(topb[,3],blnetevn$cov[,k2[3]],na.rm = TRUE)),
                       xlab=blnetevn$d4[1],ylab=blnetevn$d4[2],zlab=blnetevn$d4[3],
                       colkey = FALSE, col="red")
                par3d(windowRect = c(900, 50, 1500, 650))
                if (length(grep('Show nodes',blnetevn$d3))==1) {
                  rgl.close()
                  scatter3Drgl(blnetevn$cov[,k2[1]],blnetevn$cov[,k2[2]],blnetevn$cov[,k2[3]], 
                        xlim=c(min(lowb[,1],blnetevn$cov[,k2[1]],na.rm = TRUE),max(topb[,1],blnetevn$cov[,k2[1]],na.rm = TRUE)),
                        ylim=c(min(lowb[,2],blnetevn$cov[,k2[2]],na.rm = TRUE),max(topb[,2],blnetevn$cov[,k2[2]],na.rm = TRUE)),
                        zlim=c(min(lowb[,3],blnetevn$cov[,k2[3]],na.rm = TRUE),max(topb[,3],blnetevn$cov[,k2[3]],na.rm = TRUE)),
                        xlab=blnetevn$d4[1],ylab=blnetevn$d4[2],zlab=blnetevn$d4[3],
                        colkey = FALSE, col="red")
                  par3d(windowRect = c(900, 50, 1500, 650))
                }
                axes3d()
                box3Drgl(lowb[,1], lowb[,2], lowb[,3],
                      topb[,1], topb[,2], topb[,3], 
                      add = TRUE,
                      col = colors()[bcolors[1:nrow(lowb)]], alpha = 0.5,
                      border = "black", lwd = 2)
                if (blnetevn$d3=="Show nodes and network ties") {
                  r <- which(blnetevn$d1==names(blnetevn$cov))
                  x1 <- y1 <- z1 <- x2 <- y2 <- z2 <- rep(0,nrow(blnetevn$el))
                  for (i in 1:nrow(blnetevn$el)) {
                    s <- which(as.character(blnetevn$el[i,1])==as.character(blnetevn$cov[,r]))
                    x1[i] <- blnetevn$cov[s,k2[1]]
                    y1[i] <- blnetevn$cov[s,k2[2]]
                    z1[i] <- blnetevn$cov[s,k2[3]]
                    t <- which(as.character(blnetevn$el[i,2])==as.character(blnetevn$cov[,r]))
                    x2[i] <- blnetevn$cov[t,k2[1]]
                    y2[i] <- blnetevn$cov[t,k2[2]]
                    z2[i] <- blnetevn$cov[t,k2[3]]
                  }
                  segments3Drgl(x1,y1,z1,x2,y2,z2,add = TRUE,col="red")
                }
                legend3d("topleft",blnetevn$d5,text.col=colors()[bcolors[1:nrow(lowb)]],bty='n')
              }
              niche3drgl2()
            } else {
              gmessage("You must select 2  or 3 dimensions.")
            }
          })
        } 
      }
    })
  }
}