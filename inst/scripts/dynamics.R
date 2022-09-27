showdynamics <- function(h,...) {
  if ("cov" %in% ls(envir=blnetevn)==FALSE) {gmessage("Sorry! Attribute file is not loaded.", parent = window)} else {
    assign("dy1",character(0),envir=blnetevn)
    assign("dy2",character(0),envir=blnetevn)
    assign("dy3","",envir=blnetevn)
    assign("dy4",character(0),envir=blnetevn)
    assign("dy5",character(0),envir=blnetevn)
    assign("dy7",character(0),envir=blnetevn)
    assign("dy8","FALSE",envir=blnetevn)
    assign("dy9","all",envir=blnetevn)
    assign("m2",names(blnetevn$cov),envir=blnetevn)
    assign("m3",names(blnetevn$cov),envir=blnetevn)
    toplevel <- gwindow("Niche Dynamics", width=800, height=800, parent = window, visible=FALSE)
	cg <- ggroup(horizontal = TRUE,cont = toplevel)
	tbl0 <- gtable(blnetevn$m2,expand=TRUE,multiple=TRUE,cont=cg)
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
    cg2 <- gframe("Options", horizontal=FALSE, cont=cg)
    dy1temp <- data.frame(Node.ids="",stringsAsFactors=FALSE)
    dy2temp <- data.frame(Ecology.ids="",stringsAsFactors=FALSE)
    dy4temp <- data.frame(Dimensions=rep("",length(blnetevn$m3)),stringsAsFactors=FALSE)
    dy5temp <- data.frame(Groups=rep("",length(blnetevn$m3)),stringsAsFactors=FALSE)
    dy7temp <- data.frame(Weights="",stringsAsFactors=FALSE)
    tbl1 <- gtable(dy1temp,expand=TRUE,multiple=FALSE,cont=cg2)
	size(tbl1)[2] <- 50
    tbl2 <- gtable(dy2temp,expand=TRUE,multiple=FALSE,cont=cg2)
	size(tbl2)[2] <- 50
    if ('el' %in% ls(envir=blnetevn)) {
      gcheckboxgroup("Network included",cont=cg2,handler = function(h,...) assign("dy3",svalue(h$obj),envir=blnetevn))
    }
    tbl4 <- gtable(dy4temp,expand=TRUE,multiple=TRUE,cont=cg2)
	size(tbl4)[2] <- 120
    tbl5 <- gtable(dy5temp,expand=TRUE,multiple=TRUE,cont=cg2)
	size(tbl5)[2] <- 120
    tbl7 <- gtable(dy7temp,expand=TRUE,multiple=FALSE,cont=cg2)
	size(tbl7)[2] <- 50
    glabel("Complete.cases",cont=cg2)
    gradio(c("TRUE","FALSE"), selected = 2, cont = cg2,  handler = function(h,...) assign("dy8",svalue(h$obj),envir=blnetevn))
    addHandlerClicked(gbplus1, handler = function(h,...) {
      temp <- svalue(tbl0)
      if ("" %in% temp==FALSE & tbl1[1]=="" & length(temp)==1) {
        assign("m2",blnetevn$m3[which(blnetevn$m3 %in% setdiff(blnetevn$m2,temp))],envir=blnetevn) 
        km2 <- c(blnetevn$m2,rep("",length(blnetevn$m3)-length(blnetevn$m2)))
        for (i in 1:length(blnetevn$m3)) tbl0[i] <- km2[i]
        assign("dy1",blnetevn$m3[which(blnetevn$m3 %in% union(blnetevn$dy1,temp))],envir=blnetevn) 
        tbl1[1] <- blnetevn$dy1
      }
    })
    addHandlerClicked(gbminus1, handler = function(h,...) {
      temp <- svalue(tbl1)
      if ("" %in% temp==FALSE & length(temp)==1) {
        assign("m2",blnetevn$m3[which(blnetevn$m3 %in% union(blnetevn$m2,temp))],envir=blnetevn) 
        km2 <- c(blnetevn$m2,rep("",length(blnetevn$m3)-length(blnetevn$m2)))
        for (i in 1:length(blnetevn$m3)) tbl0[i] <- km2[i]
        assign("dy1",character(0),envir=blnetevn) 
        tbl1[1] <- ""
      }
    })
    addHandlerClicked(gbplus2, handler = function(h,...) {
      temp <- svalue(tbl0)
      if ("" %in% temp==FALSE & tbl2[1]=="" & length(temp)==1) {
        assign("m2",blnetevn$m3[which(blnetevn$m3 %in% setdiff(blnetevn$m2,temp))],envir=blnetevn) 
        km2 <- c(blnetevn$m2,rep("",length(blnetevn$m3)-length(blnetevn$m2)))
        for (i in 1:length(blnetevn$m3)) tbl0[i] <- km2[i]
        assign("dy2",blnetevn$m3[which(blnetevn$m3 %in% union(blnetevn$dy2,temp))],envir=blnetevn) 
        tbl2[1] <- blnetevn$dy2
      }
    })
    addHandlerClicked(gbminus2, handler = function(h,...) {
      temp <- svalue(tbl2)
      if ("" %in% temp==FALSE & length(temp)==1) {
        assign("m2",blnetevn$m3[which(blnetevn$m3 %in% union(blnetevn$m2,temp))],envir=blnetevn) 
        km2 <- c(blnetevn$m2,rep("",length(blnetevn$m3)-length(blnetevn$m2)))
        for (i in 1:length(blnetevn$m3)) tbl0[i] <- km2[i]
        assign("dy2",character(0),envir=blnetevn) 
        tbl2[1] <- ""
      }
    })
    addHandlerClicked(gbplus4, handler = function(h,...) {
      temp <- svalue(tbl0)
      if ("" %in% temp==FALSE & length(temp)>0) {
        assign("m2",blnetevn$m3[which(blnetevn$m3 %in% setdiff(blnetevn$m2,temp))],envir=blnetevn) 
        km2 <- c(blnetevn$m2,rep("",length(blnetevn$m3)-length(blnetevn$m2)))
        for (i in 1:length(blnetevn$m3)) tbl0[i] <- km2[i]
        assign("dy4",blnetevn$m3[which(blnetevn$m3 %in% union(blnetevn$dy4,temp))],envir=blnetevn) 
        kd4 <- c(blnetevn$dy4,rep("",length(blnetevn$m3)-length(blnetevn$dy4)))
        for (j in 1:length(blnetevn$m3)) tbl4[j] <- kd4[j]
      }
    })
    addHandlerClicked(gbminus4, handler = function(h,...) {
      temp <- svalue(tbl4)
      if ("" %in% temp==FALSE & length(temp)>0) {
        assign("m2",blnetevn$m3[which(blnetevn$m3 %in% union(blnetevn$m2,temp))],envir=blnetevn) 
        km2 <- c(blnetevn$m2,rep("",length(blnetevn$m3)-length(blnetevn$m2)+1))
        for (i in 1:length(blnetevn$m3)) tbl0[i] <- km2[i]
        assign("dy4",blnetevn$m3[which(blnetevn$m3 %in% setdiff(blnetevn$dy4,temp))],envir=blnetevn) 
        kd4 <- c(blnetevn$dy4,rep("",length(blnetevn$m3)-length(blnetevn$dy4)+1))
        for (j in 1:length(blnetevn$m3)) tbl4[j] <- kd4[j]
      }
    })
    addHandlerClicked(gbplus5, handler = function(h,...) {
      temp <- svalue(tbl0)
      if ("" %in% temp==FALSE & length(temp)>0) {
        assign("m2",blnetevn$m3[which(blnetevn$m3 %in% setdiff(blnetevn$m2,temp))],envir=blnetevn) 
        km2 <- c(blnetevn$m2,rep("",length(blnetevn$m3)-length(blnetevn$m2)))
        for (i in 1:length(blnetevn$m3)) tbl0[i] <- km2[i]
        assign("dy5",blnetevn$m3[which(blnetevn$m3 %in% union(blnetevn$dy5,temp))],envir=blnetevn) 
        kd5 <- c(blnetevn$dy5,rep("",length(blnetevn$m3)-length(blnetevn$dy5)))
        for (j in 1:length(blnetevn$m3)) tbl5[j] <- kd5[j]
      }
    })
    addHandlerClicked(gbminus5, handler = function(h,...) {
      temp <- svalue(tbl5)
      if ("" %in% temp==FALSE & length(temp)>0) {
        assign("m2",blnetevn$m3[which(blnetevn$m3 %in% union(blnetevn$m2,temp))],envir=blnetevn) 
        km2 <- c(blnetevn$m2,rep("",length(blnetevn$m3)-length(blnetevn$m2)+1))
        for (i in 1:length(blnetevn$m3)) tbl0[i] <- km2[i]
        assign("dy5",blnetevn$m3[which(blnetevn$m3 %in% setdiff(blnetevn$dy5,temp))],envir=blnetevn) 
        kd5 <- c(blnetevn$dy5,rep("",length(blnetevn$m3)-length(blnetevn$dy5)+1))
        for (j in 1:length(blnetevn$m3)) tbl5[j] <- kd5[j]
      }
    })
    addHandlerClicked(gbplus7, handler = function(h,...) {
      temp <- svalue(tbl0)
      if ("" %in% temp==FALSE & tbl7[1]=="" & length(temp)==1) {
        assign("m2",blnetevn$m3[which(blnetevn$m3 %in% setdiff(blnetevn$m2,temp))],envir=blnetevn) 
        km2 <- c(blnetevn$m2,rep("",length(blnetevn$m3)-length(blnetevn$m2)))
        for (i in 1:length(blnetevn$m3)) tbl0[i] <- km2[i]
        assign("dy7",blnetevn$m3[which(blnetevn$m3 %in% union(blnetevn$dy7,temp))],envir=blnetevn) 
        tbl7[1] <- blnetevn$dy7
      }
    })
    addHandlerClicked(gbminus7, handler = function(h,...) {
      temp <- svalue(tbl7)
      if ("" %in% temp==FALSE & length(temp)==1) {
        assign("m2",blnetevn$m3[which(blnetevn$m3 %in% union(blnetevn$m2,temp))],envir=blnetevn) 
        km2 <- c(blnetevn$m2,rep("",length(blnetevn$m3)-length(blnetevn$m2)))
        for (i in 1:length(blnetevn$m3)) tbl0[i] <- km2[i]
        assign("dy7",character(0),envir=blnetevn) 
        tbl7[1] <- ""
      }
    })
	visible(toplevel) <- TRUE	
    gbutton("Continue", cont = cg2, width=20, handler = function(h, ...) {
      if (length(blnetevn$dy1)==0 | length(blnetevn$dy4)==0 | length(blnetevn$dy5)==0) {gmessage("Missing required information.", parent = toplevel)} else {
        if (length(blnetevn$dy7)>0) tmpweight <- blnetevn$dy7 else tmpweight <- NULL  
        cc_calc <- function(x,y) {
          cc <- data.frame(matrix(rep(0,blnetevn$m1*blnetevn$n1*(length(y)+3)),nrow=blnetevn$m1*blnetevn$n1))
          names(cc)[1:2] <- blnetevn$dy4
          names(cc)[3:(length(y)+2)] <- paste("cc",y,"_niche",sep="") 
          names(cc)[length(y)+3] <- "meancc"
          cc[,1] <- rep(1:blnetevn$m1,each=blnetevn$n1)
          cc[,2] <- rep(1:blnetevn$n1,blnetevn$m1)
          for (i in 1:length(y)) {
            k1 <- x[which(x[,i+3]==1),c(1:3,i+3)]
            k2 <- table(k1[,2],k1[,3])
            for (f in 1:(nrow(k2))) {
              for (g in 1:(ncol(k2))) {
                o <- which(cc[,1]==as.numeric(rownames(k2)[f]) & cc[,2]==as.numeric(colnames(k2)[g]))
                cc[o,(i+2)] <- k2[f,g]/sum(k2)
              }
            }
          }
          cc[,(length(y)+3)] <- rowMeans(cc[,3:(length(y)+2)])
          return(cc)
        }
        mr_calc <- function(x,y) {
          mr <- data.frame(matrix(rep(0,blnetevn$m1*blnetevn$n1*(length(y)+3)),nrow=blnetevn$m1*blnetevn$n1))
          names(mr)[1:2] <- blnetevn$dy4
          names(mr)[3:(length(y)+2)] <- paste("mr",y,sep="") 
          names(mr)[length(y)+3] <- "meanmr"
          mr[,1] <- rep(1:blnetevn$m1,each=blnetevn$n1)
          mr[,2] <- rep(1:blnetevn$n1,blnetevn$m1)
          for (i in 1:length(y)) {
            k1 <- x[which(x[,i+length(y)+3]==1),c(1:3,i+length(y)+3)]
            k2 <- table(k1[,2],k1[,3])
            for (f in 1:(nrow(k2))) {
              for (g in 1:(ncol(k2))) {
                o <- which(mr[,1]==as.numeric(rownames(k2)[f]) & mr[,2]==as.numeric(colnames(k2)[g]))
                mr[o,(i+2)] <- k2[f,g]/sum(k2)
              }
            }
          }
          mr[,(length(y)+3)] <- rowMeans(mr[,3:(length(y)+2)])
          return(mr)
        }
        dispose(toplevel)
        assign("dy6",rep(1.5,length(blnetevn$dy4)),envir=blnetevn)
        extralevel <- gwindow("Dev.range", width=800, height=300)
        ge <- gpanedgroup(cont = extralevel, horizontal = FALSE)
        cge <- ggroup(cont = ge, horizontal = FALSE)
        for (da in 1:length(blnetevn$dy4)) {
          glabel(blnetevn$dy4[da],cont=cge)
          assign(paste0("dy6da", da),gslider(from = 0, to = 5, by = .05, value = 1.5, cont=cge, handler = function(h,...) svalue(get(paste0("dy6da", da))) ))
        } 
        addSpring(cge)
        addSpring(cge)
        addSpring(cge)
        button <- gbutton("Continue", cont = cge, handler = function(h, ...) {
          dy6tmp <- c()
          for (da in 1:length(blnetevn$dy4)) {
            dy6tmp <- c(dy6tmp,svalue(get(paste0("dy6da", da))))
          }
          assign("dy6",dy6tmp,envir=blnetevn)
          dispose(extralevel)
          if (length(blnetevn$dy4)==2) {
            single <- function(attr) {
              b <- blau(attr, node.ids=blnetevn$dy1, dimension=blnetevn$dy4, memberships=blnetevn$dy5,weights=tmpweight,complete.cases=blnetevn$dy8)
              b <- niches(b, dev.range = blnetevn$dy6)
              assign("bobj",b,envir=blnetevn)
              k <- data.frame(cbind(b$ids$nodeId,b$dimensions,b$isInNiche,b$memberships))
              names(k)[1] <- "nodeId"
			  k[,2] <- as.numeric(k[,2])
			  k[,3] <- as.numeric(k[,3])
              ccw <- gwindow("Dimension Category Selection", width=600, height=700, parent = window)
              cg1 <- ggroup(cont = ccw, use.scrollwindow=T, horizontal = FALSE)
              dd1 <- data.frame(table(k[,2]))
              colnames(dd1)[1] <- blnetevn$dy4[1]
			  #if (class(dd1[,1])!="numeric") dd1 <- dd1[order(grepl("^\\d+$", dd1[,1]), sprintf("%10s", dd1[,1])),]
              dd2 <- data.frame(table(k[,3]))
              colnames(dd2)[1] <- blnetevn$dy4[2]
			  #if (class(dd2[,1])!="numeric") dd2 <- dd2[order(grepl("^\\d+$", dd2[,1]), sprintf("%10s", dd2[,1])),]
              glabel(paste("This is the frequency table for ",blnetevn$dy4[1],".",sep=""),cont=cg1)
              dim1 <- gdf(dd1, expand = TRUE, fill=TRUE, cont = cg1)
              glabel(paste("Please slide the bar below to set the categories for dimension ",blnetevn$dy4[1],"?",sep=""),cont=cg1)
              assign("m1",1,envir=blnetevn)
              assign("n1",1,envir=blnetevn)
              gslider(from = 1, to = max(k[,2],na.rm = TRUE)-min(k[,2],na.rm = TRUE)+1, by = 1, value = 1, cont=cg1, handler = function(h,...){
                assign("m1",svalue(h$obj),envir=blnetevn)
              })
              gseparator(cont = cg1)
              gdim2 <- glabel(paste("This is the frequency table for ",blnetevn$dy4[2],".",sep=""),cont=cg1)
              dim2 <- gdf(dd2, expand = TRUE, fill=TRUE, cont = cg1)
              glabel(paste("Please slide the bar below to set the categories for dimension ",blnetevn$dy4[2],"?",sep=""),cont=cg1)
              gslider(from = 1, to = max(k[,3],na.rm = TRUE)-min(k[,3],na.rm = TRUE)+1, by = 1, value = 1, cont=cg1, handler = function(h,...){
                assign("n1",svalue(h$obj),envir=blnetevn)
              })
              gseparator(cont = cg1)
              gbutton("Continue", cont = cg1, width=20, handler = function(h, ...){
                dispose(ccw)  
                x <- seq(min(k[,2],na.rm = TRUE),max(k[,2],na.rm = TRUE),by = (max(k[,2],na.rm = TRUE)-min(k[,2],na.rm = TRUE))/(blnetevn$m1-1))
                y <- seq(min(k[,3],na.rm = TRUE),max(k[,3],na.rm = TRUE),by = (max(k[,3],na.rm = TRUE)-min(k[,3],na.rm = TRUE))/(blnetevn$n1-1)) 
                k[,2] <- cut(k[,2],b=blnetevn$m1,label=c(1:blnetevn$m1)) 
                k[,3] <- cut(k[,3],b=blnetevn$n1,label=c(1:blnetevn$n1)) 
                cc <- cc_calc(k,blnetevn$dy5)
                mr <- mr_calc(k,blnetevn$dy5)
                ie <- cbind(cc[,1],cc[,2],mr[,(length(blnetevn$dy5)+3)]-cc[,(length(blnetevn$dy5)+3)])
                ie_poly <- lm(ie[,3]~ie[,1]+I(ie[,1]^2)+I(ie[,1]^3)+ie[,2]+I(ie[,2]^2)+I(ie[,2]^3)+ie[,1]*ie[,2])
                ie <- cbind(ie,predict(ie_poly))
                final <- data.frame(cbind(cc,mr[,3:ncol(mr)],ie[,3:4]))
                colnames(final)[ncol(final)-1] <- "ie"
                colnames(final)[ncol(final)] <- "ie_poly"
                trdlevel <- gwindow("Plot...", width=580, height=30)
                g2 <- gpanedgroup(cont = trdlevel, horizontal = TRUE)
                cg2 <- ggroup(cont = g2, horizontal = TRUE)
                button1 <- gbutton("Plot Carrying Capacity", cont = cg2, width=20, handler = function(h, ...) {
                  x1 <- x2 <- y1 <- y2 <- z1 <- z2 <- rep(0,length(blnetevn$dy5))
                  for (i in 1:length(blnetevn$dy5)) {
                    x1[i] <- x2[i] <- mean(attr[which(attr[,blnetevn$dy5[i]]==1),blnetevn$dy4[1]],na.rm = TRUE)
                    y1[i] <- y2[i] <- mean(attr[which(attr[,blnetevn$dy5[i]]==1),blnetevn$dy4[2]],na.rm = TRUE)
                    z1[i] <- min(cc[,(3+length(blnetevn$dy5))])
                    z2[i] <- max(cc[,(3+length(blnetevn$dy5))])
                  }
                  z <- matrix(cc[,(3+length(blnetevn$dy5))],nrow=length(x))
                  cc3Drgl <- function() {
                    nbcol = blnetevn$m1*blnetevn$n1
                    color = rev(rainbow(nbcol, start = 0/6, end = 4/6))
                    zcol  = cut(cc[,(3+length(blnetevn$dy5))], nbcol)
                    persp3d(x,y,z, 
                               xlab = blnetevn$dy4[1], ylab = blnetevn$dy4[2], zlab = "Carrying capacity",
                               col=color[zcol])
                    par3d(windowRect = c(900, 50, 1500, 650))
                    scatter3Drgl(x2,y2,z2,dev = rgl.cur(), col = "red")
                    rgl.close()
                    scatter3Drgl(x2,y2,z2,add = TRUE, col = "red")
                    scatter3Drgl(x1,y1,z1,add = TRUE, col = "red")
                    segments3Drgl(x1,y1,z1,x2,y2,z2,add = TRUE,col="red")
                    text3Drgl(x2, y2, z2, labels=blnetevn$dy5,add = TRUE, col = "red", cex=0.9)
                  }
                  cc3Drgl()
                })
                addSpace(cg2, 5) 
                button2 <- gbutton("Plot Membership Rate", cont = cg2, width=20, handler = function(h, ...) {
                  x1 <- x2 <- y1 <- y2 <- z1 <- z2 <- rep(0,length(blnetevn$dy5))
                  for (i in 1:length(blnetevn$dy5)) {
                    x1[i] <- x2[i] <- mean(attr[which(attr[,blnetevn$dy5[i]]==1),blnetevn$dy4[1]],na.rm = TRUE)
                    y1[i] <- y2[i] <- mean(attr[which(attr[,blnetevn$dy5[i]]==1),blnetevn$dy4[2]],na.rm = TRUE)
                    z1[i] <- min(mr[,(3+length(blnetevn$dy5))])
                    z2[i] <- max(mr[,(3+length(blnetevn$dy5))])
                  }
                  z <- matrix(mr[,(3+length(blnetevn$dy5))],nrow=length(x))
                  mr3Drgl <- function() {
                    nbcol = blnetevn$m1*blnetevn$n1
                    color = rev(rainbow(nbcol, start = 0/6, end = 4/6))
                    zcol  = cut(mr[,(3+length(blnetevn$dy5))], nbcol)
                    persp3d(x,y,z, 
                            col=color[zcol], 
                            xlab = blnetevn$dy4[1], ylab = blnetevn$dy4[2], zlab = "Membership rate")
                    par3d(windowRect = c(900, 50, 1500, 650))
                    scatter3Drgl(x2,y2,z2,dev = rgl.cur(), col = "red")
                    rgl.close()
                    scatter3Drgl(x2,y2,z2,add = TRUE, col = "red")
                    scatter3Drgl(x1,y1,z1,add = TRUE, col = "red")
                    segments3Drgl(x1,y1,z1,x2,y2,z2,add = TRUE,col="red")
                    text3Drgl(x2, y2, z2, labels=blnetevn$dy5,add = TRUE, col = "red", cex=0.9)
                  }
                  mr3Drgl()
                })
                addSpace(cg2, 5) 
                button3 <- gbutton("Plot Intensity of Exploitation", cont = cg2, width=20, handler = function(h, ...) {
                  x1 <- x2 <- y1 <- y2 <- z1 <- z2 <- rep(0,length(blnetevn$dy5))
                  for (i in 1:length(blnetevn$dy5)) {
                    x1[i] <- x2[i] <- mean(attr[which(attr[,blnetevn$dy5[i]]==1),blnetevn$dy4[1]],na.rm = TRUE)
                    y1[i] <- y2[i] <- mean(attr[which(attr[,blnetevn$dy5[i]]==1),blnetevn$dy4[2]],na.rm = TRUE)
                    z1[i] <- min(ie[,4])
                    z2[i] <- max(ie[,4])
                  }
                  z <- matrix(ie[,4],nrow=length(x))
                  ie3Drgl <- function() {
                    nbcol = blnetevn$m1*blnetevn$n1
                    color = rev(rainbow(nbcol, start = 0/6, end = 4/6))
                    zcol  = cut(ie[,3], nbcol)
                    persp3d(x,y,z,
                          col=color[zcol], 
                          xlab = blnetevn$dy4[1], ylab = blnetevn$dy4[2], zlab = "Intensity of exploitation")
                    par3d(windowRect = c(900, 50, 1500, 650))
                    scatter3Drgl(x2,y2,z2,dev = rgl.cur(), col = "red")
                    rgl.close()
                    scatter3Drgl(x2,y2,z2,add = TRUE, col = "red")
                    scatter3Drgl(x1,y1,z1,add = TRUE, col = "red")
                    segments3Drgl(x1,y1,z1,x2,y2,z2,add = TRUE,col="red")
                    text3Drgl(x2, y2, z2, labels=blnetevn$dy5,add = TRUE, col = "red", cex=0.9)
                  }
                  ie3Drgl()
                  ithlevel <- gwindow("Intensity of Exploitation Equation", width = 1100, height = 100)
                  ig <- ggroup(cont = ithlevel, horizontal = F, expand=T)
                  polyf <- data.frame(matrix(ie_poly$coefficients,nrow=1))
                  names(polyf) <- c("Intercept",blnetevn$dy4[1],paste(blnetevn$dy4[1],"^2",sep=""),paste(blnetevn$dy4[1],"^3",sep=""),
                    blnetevn$dy4[2],paste(blnetevn$dy4[2],"^2",sep=""),paste(blnetevn$dy4[2],"^3",sep=""),paste(blnetevn$dy4[1],"*",blnetevn$dy4[2],sep=""))
                  gdf(polyf,expand=TRUE,fill=TRUE,cont=ig)
                })
                addSpace(cg2, 5) 
                button4 <- gbutton("Show Table", cont = cg2, width=20, handler = function(h, ...) {
                  fourthlevel <- gwindow("Table for Carrying Capacity, Membership Rate, & Intensity of Exploitation",width = 800, height = 600)
                  ng4 <- ggroup(horizontal = FALSE, cont = fourthlevel)
                  button1 <- gbutton("Save as csv file: cc&mr&ie.csv", expand = FALSE, cont = ng4, handler = function(h, ...) {
                    write.table(final, "cc&mr&ie.csv", row.names=F, col.names=T, sep=",")
                  })
                  button2 <- gbutton("Save as R file: cc&mr&ie.Rdata", expand = FALSE, cont = ng4, handler = function(h, ...) {
                    save(final, file="cc&mr&ie.Rdata")
                  })
                  button3 <- gbutton("Save as SAS file: cc&mr&ie.txt & cc&mr&ie.sas", expand = FALSE, cont = ng4, handler = function(h, ...) {
                    write.foreign(final, "cc&mr&ie.txt", "cc&mr&ie.sas",   package="SAS")
                  })
                  button4 <- gbutton("Save as Stata file: cc&mr&ie.dta", expand = FALSE, cont = ng4, handler = function(h, ...) {
                    write.dta(final, ("cc&mr&ie.dta"))
                  })
                  button5 <- gbutton("Save as SPSS file: cc&mr&ie.txt & cc&mr&ie.sps", expand = FALSE, cont = ng4, handler = function(h, ...) {
                    write.foreign(final, "cc&mr&ie.txt", "cc&mr&ie.sps",   package="SPSS")
                  })
                  gseparator(cont = ng4)
                  vars <- gdf(final, expand = TRUE, fill=TRUE, cont = ng4)
                })
              })
            }
			if (length(blnetevn$dy2)==0) {
              single(blnetevn$cov)
            }
            if (length(blnetevn$dy2)>0) {
              if (nrow(unique(blnetevn$cov[which(colnames(blnetevn$cov)==blnetevn$dy2)]))==1) {
                single(blnetevn$cov)
              } else if (nrow(unique(blnetevn$cov[which(colnames(blnetevn$cov)==blnetevn$dy2)]))>1) {
                dylevel <- gwindow("Niche Dynamics Options", width=600, height=600, parent = window)
                dyg <- ggroup(cont = dylevel, use.scrollwindow=T, horizontal = FALSE)
                tbl <- glayout(cont = dyg)
                glabel("Please select which ecology you want to test niche dynamics:", cont=dyg)
                gradio(c("all",as.matrix(unique(blnetevn$cov[which(colnames(blnetevn$cov)==blnetevn$dy2)]))), check=1, cont=dyg, handler = function(h,...){
                  assign("dy9",svalue(h$obj),envir=blnetevn)
                })
                buttondy <- gbutton("Continue", expand = FALSE, cont = dyg, handler = function(h, ...) {
                  dispose(dylevel)
                  if (blnetevn$dy9!="all") {
                    cov1 <- blnetevn$cov[which(blnetevn$cov[,which(names(blnetevn$cov)==blnetevn$dy2)]==blnetevn$dy9),]
                    single(cov1)
                  } else {
                    z1 <- which(names(blnetevn$cov)==blnetevn$dy1)
                    z2 <- which(names(blnetevn$cov)==blnetevn$dy2)
                    z3 <- which(names(blnetevn$cov)==blnetevn$dy4[1])
                    z4 <- which(names(blnetevn$cov)==blnetevn$dy4[2])
                    t <- table(blnetevn$cov[z2])
                    k <- data.frame(cbind(blnetevn$cov[z1],blnetevn$cov[z3],blnetevn$cov[z4]))
                    ccw <- gwindow("Dimension Category Selection", width=600, height=700, parent = window)
                    cg1 <- ggroup(cont = ccw, use.scrollwindow=T, horizontal = FALSE)
                    dd1 <- data.frame(t(table(k[,2])))[,2:3]
                    colnames(dd1)[1] <- blnetevn$dy4[1]
                    dd2 <- data.frame(t(table(k[,3])))[,2:3]
                    colnames(dd2)[1] <- blnetevn$dy4[2]
                    glabel(paste("This is the frequency table for ",blnetevn$dy4[1],".",sep=""),cont=cg1)
                    dim1 <- gdf(dd1, expand = TRUE, fill=TRUE, cont = cg1)
                    glabel(paste("Please slide the bar below to set the categories for dimension ",blnetevn$dy4[1],"?",sep=""),cont=cg1)
                    assign("m1",1,envir=blnetevn)
                    assign("n1",1,envir=blnetevn)
                    gslider(from = 1, to = max(k[,2],na.rm = TRUE)-min(k[,2],na.rm = TRUE)+1, by = 1, value = 1, cont=cg1, handler = function(h,...){
                      assign("m1",svalue(h$obj),envir=blnetevn)
                    })
                    gseparator(cont = cg1)
                    gdim2 <- glabel(paste("This is the frequency table for ",blnetevn$dy4[2],".",sep=""),cont=cg1)
                    dim2 <- gdf(dd2, expand = TRUE, fill=TRUE, cont = cg1)
                    glabel(paste("Please slide the bar below to set the categories for dimension ",blnetevn$dy4[2],"?",sep=""),cont=cg1)
                    gslider(from = 1, to = max(k[,3],na.rm = TRUE)-min(k[,3],na.rm = TRUE)+1, by = 1, value = 1, cont=cg1, handler = function(h,...){
                      assign("n1",svalue(h$obj),envir=blnetevn)
                    })
                    gseparator(cont = cg1)
                    gbutton("Continue", cont = cg1, width=20, handler = function(h, ...){
                      dispose(ccw) 
                      nichem1 <- nichem2 <- matrix(rep(0,(length(t)-1)*length(blnetevn$dy5)),nrow=length(t)-1)
                      netop1 <- netop2 <- rightc1 <- rightc2 <- leftc1 <- leftc2 <- matrix(rep(0,length(t)*length(blnetevn$dy5)),nrow=length(t))
                      for (o in 1:(length(t)-1)) {
                        a1 <- blnetevn$cov[which(blnetevn$cov[,which(names(blnetevn$cov)==blnetevn$dy2)]==names(t)[o]),]
                        a1[,z3] <- as.numeric(cut(a1[,z3],b=blnetevn$m1,label=c(1:blnetevn$m1))) 
                        a1[,z4] <- as.numeric(cut(a1[,z4],b=blnetevn$n1,label=c(1:blnetevn$n1))) 
                        a2 <- blnetevn$cov[which(blnetevn$cov[,which(names(blnetevn$cov)==blnetevn$dy2)]==names(t)[o+1]),]
                        a2[,z3] <- as.numeric(cut(a2[,z3],b=blnetevn$m1,label=c(1:blnetevn$m1))) 
                        a2[,z4] <- as.numeric(cut(a2[,z4],b=blnetevn$n1,label=c(1:blnetevn$n1))) 
                        for(p in 1:length(blnetevn$dy5)) {
                          nichem1[o,p] <- mean(a2[which(a2[,p+3]==1),z3],na.rm=T)-mean(a1[which(a1[,p+3]==1),z3],na.rm=T)
                          nichem2[o,p] <- mean(a2[which(a2[,p+3]==1),z4],na.rm=T)-mean(a1[which(a1[,p+3]==1),z4],na.rm=T) 
                        }
                      }
                      for (o in 1:length(t)) {
                         a <- blnetevn$cov[which(blnetevn$cov[,which(names(blnetevn$cov)==blnetevn$dy2)]==names(t)[o]),]
                         bo <- blau(a, node.ids=blnetevn$dy1, dimension=blnetevn$dy4, memberships=blnetevn$dy5,weights=tmpweight,complete.cases=blnetevn$dy8)
                         bo <- niches(bo, dev.range = blnetevn$dy6)
                         ko <- data.frame(cbind(bo$ids$nodeId,bo$dimensions,bo$isInNiche,bo$memberships))
                         cco <- cc_calc(ko,blnetevn$dy5)
                         mro <- mr_calc(ko,blnetevn$dy5)
                         ieo <- data.frame(cbind(cco[,1],cco[,2],mro[,(length(blnetevn$dy5)+3)]-cco[,(length(blnetevn$dy5)+3)]))
                         names(ieo) <- c("x1","x2","y")
                         ie_polyo <- lm(y~x1+I(x1^2)+I(x1^3)+x2+I(x2^2)+I(x2^3)+x1*x2,data=ieo)
                         cuto1 <- seq(from = blnetevn$dy6[1]/10, to = blnetevn$dy6[1], by = blnetevn$dy6[1]/10)
                         cuto2 <- seq(from = blnetevn$dy6[2]/10, to = blnetevn$dy6[2], by = blnetevn$dy6[2]/10)
                         for(p in 1:length(blnetevn$dy5)) {
                           mean1 <- mean(a[which(a[,which(names(a)==blnetevn$dy5[p])]==1),z3],na.rm=T)
                           sd1 <- sd(a[which(a[,which(names(a)==blnetevn$dy5[p])]==1),z3],na.rm=T)
                           mean2 <- mean(a[which(a[,which(names(a)==blnetevn$dy5[p])]==1),z4],na.rm=T)
                           sd2 <- sd(a[which(a[,which(names(a)==blnetevn$dy5[p])]==1),z4],na.rm=T)
                           r1 <- mean1+cuto1*sd1
                           iep <- data.frame(r1,rep(mean2,length(r1)))
                           names(iep) <- c("x1","x2")
                           rightc1[o,p] <- sum(predict(ie_polyo,iep))
                           l1 <- mean1-cuto1*sd1
                           iep <- data.frame(l1,rep(mean2,length(l1)))
                           names(iep) <- c("x1","x2")
                           leftc1[o,p] <- sum(predict(ie_polyo,iep))     
                           netop1[o,p] <- leftc1[o,p]-rightc1[o,p]
                           r2 <- mean2+cuto2*sd2
                           iep <- data.frame(rep(mean1,length(r2)),r2)
                           names(iep) <- c("x1","x2")
                           rightc2[o,p] <- sum(predict(ie_polyo,iep))
                           l2 <- mean2-cuto2*sd2
                           iep <- data.frame(rep(mean1,length(l2)),l2)
                           names(iep) <- c("x1","x2")
                           leftc2[o,p] <- sum(predict(ie_polyo,iep))     
                           netop2[o,p] <- leftc2[o,p]-rightc2[o,p]
                         }
                      }
                      niche_movement <- data.frame(cbind(c(nichem1),c(netop1[1:(length(t)-1),]),c(nichem2),c(netop2[1:(length(t)-1),])))
                      names(niche_movement)[1] <- "Niche_movement_dim1"
                      names(niche_movement)[2] <- "Niche_opportunity_dim1"
                      names(niche_movement)[3] <- "Niche_movement_dim2"
                      names(niche_movement)[4] <- "Niche_opportunity_dim2"
                      assign("niche_movement",niche_movement,envir=blnetevn)
                      eq3 <- lm(Niche_movement_dim1~Niche_opportunity_dim1,data=niche_movement)
                      eq4 <- lm(Niche_movement_dim2~Niche_opportunity_dim2,data=niche_movement)
                      outeq3 <- paste(capture.output(summary(eq3)), collapse="\n")
                      outeq4 <- paste(capture.output(summary(eq4)), collapse="\n")
                      eqw <- gwindow("Predicted Niche Movement", width=800, height=750, parent = window)
                      eqg1 <- ggroup(cont = eqw, use.scrollwindow=T, horizontal = FALSE)
                      button1 <- gbutton("Save as csv file: niche_movement.csv", expand = FALSE, cont = eqg1, handler = function(h, ...) {
                        write.table(niche_movement, "niche_movement.csv", row.names=F, col.names=T, sep=",")
                      })
                      button2 <- gbutton("Save as R file: niche_movement.Rdata", expand = FALSE, cont = eqg1, handler = function(h, ...) {
                        save(niche_movement, file="niche_movements.Rdata")
                      })
                      button3 <- gbutton("Save as SAS file: niche_movement.txt & niche_movement.sas", expand = FALSE, cont = eqg1, handler = function(h, ...) {
                        write.foreign(niche_movement, "niche_movements.txt", "niche_movements.sas",   package="SAS")
                      })
                      button4 <- gbutton("Save as Stata file: niche_movement.dta", expand = FALSE, cont = eqg1, handler = function(h, ...) {
                        write.dta(niche_movement, ("niche_movements.dta"))
                      })
                      button5 <- gbutton("Save as SPSS file: niche_movement.txt & niche_movement.sps", expand = FALSE, cont = eqg1, handler = function(h, ...) {
                        write.foreign(niche_movement, "niche_movement.txt", "niche_movements.sps",   package="SPSS")
                      })
                      gseparator(cont = eqg1)
                      glabel(paste("dim1 = ",blnetevn$dy4[1],sep=""),cont=eqg1)
                      gtext(outeq3, cont=eqg1, expand=TRUE, font.attr=list(family="monospace"), height=200)
                      glabel(paste("dim2 = ",blnetevn$dy4[2],sep=""),cont=eqg1)
                      gtext(outeq4, cont=eqg1, expand=TRUE, font.attr=list(family="monospace"), height=200)
                    })
                  }
                })
              }
            }
          }
          else {
            gmessage("You must select 2 dimensions.")
          }
        })
      }    
    })
  }
}
