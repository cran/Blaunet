showblaububble <- function(h,...) {
  if ("cov" %in% ls(envir=blnetevn)==FALSE) {gmessage("Sorry! Attribute file is not loaded.", parent = window)} else {
  assign("u1",character(0),envir=blnetevn)
  assign("u2",character(0),envir=blnetevn)
  assign("u3","all",envir=blnetevn)
  assign("u4",character(0),envir=blnetevn)
  assign("u5",character(0),envir=blnetevn)
  assign("u6",0.33,envir=blnetevn)
  assign("m1",names(blnetevn$cov),envir=blnetevn)
  assign("m3",names(blnetevn$cov),envir=blnetevn)
  toplevel <- gwindow("Blau Bubbles", width=800, height=600, parent = window, visible=FALSE)
  cg <- ggroup(horizontal = TRUE,cont = toplevel)
  tbl0 <- gtable(blnetevn$m1,expand=TRUE,multiple=TRUE,cont=cg)
  cg1 <- ggroup(horizontal = FALSE, cont = cg)
  addSpring(cg1)
  addSpring(cg1)
  gbplus1 <- gbutton("+", cont=cg1)
  gbminus1 <- gbutton("-", cont=cg1)
  addSpring(cg1)
  addSpring(cg1)
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
  gbplus4 <- gbutton("+", cont=cg1)
  gbminus4 <- gbutton("-", cont=cg1)
  addSpring(cg1)
  addSpring(cg1)
  addSpring(cg1)
  addSpring(cg1)
  cg2 <- gframe("Options", horizontal=FALSE, cont=cg)
  u1temp <- data.frame(Node.ids="",stringsAsFactors=FALSE)
  u2temp <- data.frame(Ecology.ids="",stringsAsFactors=FALSE)
  u4temp <- data.frame(Dimensions=rep("",length(blnetevn$m3)),stringsAsFactors=FALSE)
  u5temp <- data.frame(Groups=rep("",length(blnetevn$m3)),stringsAsFactors=FALSE)
  tbl1 <- gtable(u1temp,expand=TRUE,multiple=FALSE,cont=cg2)
  size(tbl1)[2] <- 50
  tbl2 <- gtable(u2temp,expand=TRUE,multiple=FALSE,cont=cg2)
  size(tbl2)[2] <- 50
  tbl4 <- gtable(u4temp,expand=TRUE,multiple=TRUE,cont=cg2)
  size(tbl4)[2] <- 120
  addHandlerClicked(gbplus1, handler = function(h,...) {
    temp <- svalue(tbl0)
    if ("" %in% temp==FALSE & tbl1[1]=="" & length(temp)==1) {
      assign("m1",blnetevn$m3[which(blnetevn$m3 %in% setdiff(blnetevn$m1,temp))],envir=blnetevn) 
      km1 <- c(blnetevn$m1,rep("",length(blnetevn$m3)-length(blnetevn$m1)))
      for (i in 1:length(blnetevn$m3)) tbl0[i] <- km1[i]
      assign("u1",blnetevn$m3[which(blnetevn$m3 %in% union(blnetevn$u1,temp))],envir=blnetevn) 
      tbl1[1] <- blnetevn$u1
    }
  })
  addHandlerClicked(gbminus1, handler = function(h,...) {
    temp <- svalue(tbl1)
    if ("" %in% temp==FALSE& length(temp)==1) {
      assign("m1",blnetevn$m3[which(blnetevn$m3 %in% union(blnetevn$m1,temp))],envir=blnetevn) 
      km1 <- c(blnetevn$m1,rep("",length(blnetevn$m3)-length(blnetevn$m1)))
      for (i in 1:length(blnetevn$m3)) tbl0[i] <- km1[i]
      assign("u1",character(0),envir=blnetevn) 
      tbl1[1] <- ""
    }
  })
  addHandlerClicked(gbplus2, handler = function(h,...) {
    temp <- svalue(tbl0)
    if ("" %in% temp==FALSE & tbl2[1]=="" & length(temp)==1) {
      assign("m1",blnetevn$m3[which(blnetevn$m3 %in% setdiff(blnetevn$m1,temp))],envir=blnetevn) 
      km1 <- c(blnetevn$m1,rep("",length(blnetevn$m3)-length(blnetevn$m1)))
      for (i in 1:length(blnetevn$m3)) tbl0[i] <- km1[i]
      assign("u2",blnetevn$m3[which(blnetevn$m3 %in% union(blnetevn$u2,temp))],envir=blnetevn) 
      tbl2[1] <- blnetevn$u2
    }
  })
  addHandlerClicked(gbminus2, handler = function(h,...) {
    temp <- svalue(tbl2)
    if ("" %in% temp==FALSE & length(temp)==1) {
      assign("m1",blnetevn$m3[which(blnetevn$m3 %in% union(blnetevn$m1,temp))],envir=blnetevn) 
      km1 <- c(blnetevn$m1,rep("",length(blnetevn$m3)-length(blnetevn$m1)))
      for (i in 1:length(blnetevn$m3)) tbl0[i] <- km1[i]
      assign("u2",character(0),envir=blnetevn) 
      tbl2[1] <- ""
    }
  })
  addHandlerClicked(gbplus4, handler = function(h,...) {
    temp <- svalue(tbl0)
    if ("" %in% temp==FALSE & length(temp)>0) {
      assign("m1",blnetevn$m3[which(blnetevn$m3 %in% setdiff(blnetevn$m1,temp))],envir=blnetevn) 
      km1 <- c(blnetevn$m1,rep("",length(blnetevn$m3)-length(blnetevn$m1)))
      for (i in 1:length(blnetevn$m3)) tbl0[i] <- km1[i]
      assign("u4",blnetevn$m3[which(blnetevn$m3 %in% union(blnetevn$u4,temp))],envir=blnetevn) 
      ku4 <- c(blnetevn$u4,rep("",length(blnetevn$m3)-length(blnetevn$u4)))
      for (j in 1:length(blnetevn$m3)) tbl4[j] <- ku4[j]
    }
  })
  addHandlerClicked(gbminus4, handler = function(h,...) {
    temp <- svalue(tbl4)
    if ("" %in% temp==FALSE & length(temp)>0) {
      assign("m1",blnetevn$m3[which(blnetevn$m3 %in% union(blnetevn$m1,temp))],envir=blnetevn) 
      km1 <- c(blnetevn$m1,rep("",length(blnetevn$m3)-length(blnetevn$m1)+1))
      for (i in 1:length(blnetevn$m3)) tbl0[i] <- km1[i]
      assign("u4",blnetevn$m3[which(blnetevn$m3 %in% setdiff(blnetevn$u4,temp))],envir=blnetevn) 
      ku4 <- c(blnetevn$u4,rep("",length(blnetevn$m3)-length(blnetevn$u4)+1))
      for (j in 1:length(blnetevn$m3)) tbl4[j] <- ku4[j]
    }
  })
  visible(toplevel) <- TRUE
  button <- gbutton("Continue", expand = FALSE, cont = cg2, handler = function(h, ...) {
    if (length(blnetevn$u1)==0 | length(blnetevn$u4)==0 ) {gmessage("Missing required information.", parent = toplevel)} else {
    dispose(toplevel)
    seclevel <- gwindow("Blau Bubble Options", width=600, height=350, parent = window)
    secg <- ggroup(cont = seclevel, use.scrollwindow=T, horizontal = FALSE)
	secgv <- gvbox(cont = secg)
    tbl <- gformlayout(cont = secgv)
    if (length(blnetevn$u2)!=0) {
      glabel("Please select which ecology you want to generat Blau bubbles", cont=tbl)
      gradio(c("all",as.matrix(unique(blnetevn$cov[which(colnames(blnetevn$cov)==blnetevn$u2)]))), selected = 1, , cont=tbl, handler = function(h,...){
        assign("u3",svalue(h$obj),envir=blnetevn)
      })
      gseparator(cont = secgv)
    }
    gcheckboxgroup(blnetevn$u4, cont = tbl, label="Please identify the categorical variable(s)", handler = function(h,...){
            assign("u5",svalue(h$obj),envir=blnetevn)
          })
    gseparator(cont = secgv)
    glabel("radius (0.33 by default)",cont=secg)
    gslider(from = 0, to = 1, by = .01, value = 0.33, ,cont=secg, handler = function(h,...){
      assign("u6",svalue(h$obj),envir=blnetevn)
    })
    button <- gbutton("Continue", expand = FALSE, cont = secg, handler = function(h, ...) {
      dispose(seclevel)
      if (blnetevn$u3=="all") {
        cov1 <- blnetevn$cov
        if ("el" %in% ls(envir=blnetevn)) adj1 <- blnetevn$adj
      } else {
        cov1 <- blnetevn$cov[which(blnetevn$cov[which(colnames(blnetevn$cov)==blnetevn$u2)]==blnetevn$u3),]
        if ("el" %in% ls(envir=blnetevn)) adj1 <- blnetevn$adj[which(blnetevn$cov[which(colnames(blnetevn$cov)==blnetevn$u2)]==blnetevn$u3),which(blnetevn$cov[which(colnames(blnetevn$cov)==blnetevn$u2)]==blnetevn$u3)]
      }
      k <- nrow(cov1)
      distmat <- matrix(rep(0,k*k),nrow=k)
      data <- data.frame(matrix(rep(0,k*length(blnetevn$u4)),nrow=k))
      for (o in 1:length(blnetevn$u4)) {
        if (blnetevn$u4[o] %in% blnetevn$u5) {
          data[,o] <- cov1[which(colnames(cov1)==blnetevn$u4[o])]
        } else {
          min <- min(cov1[which(colnames(cov1)==blnetevn$u4[o])],na.rm=TRUE)
          max <- max(cov1[which(colnames(cov1)==blnetevn$u4[o])],na.rm=TRUE)
          data[,o] <- (cov1[which(colnames(cov1)==blnetevn$u4[o])]-min)/(max-min)
        }
      }
      colnames(data) <- blnetevn$u4
      for (p in 1:k) for (q in 1:k) if (p!=q) {
        distance <- valid_count <- 0
        for (r in 1:length(blnetevn$u4)) {
          i_ <- data[p,r]
          j_ <- data[q,r]
          if (is.na(i_)==FALSE && is.na(j_)==FALSE) {
            if (blnetevn$u4[r] %in% blnetevn$u5) {
              if (i_==j_) distance <- distance+0 else distance <- distance+1
            } else distance <- distance+(i_-j_)^2
            valid_count <- valid_count+1
          }
        }
        distmat[p,q] <- sqrt(distance)/sqrt(valid_count)
      }
      blaububble <- matrix(rep(0,k*k),nrow=k)
      blaububble[which(distmat<=blnetevn$u6)] <- 1
      blaububble[which(distmat>blnetevn$u6)] <- 0
      diag(blaububble) <- 0
      distmat <- data.frame(distmat)
      distmat <- data.frame(cbind(as.matrix(cov1[which(colnames(cov1)==blnetevn$u1)]),distmat))
      colnames(distmat) <- c("ego",as.matrix(cov1[which(colnames(cov1)==blnetevn$u1)]))
      blaububble <- data.frame(blaububble)
      rownames(blaububble) <- as.matrix(cov1[which(colnames(cov1)==blnetevn$u1)])
      colnames(blaububble) <- as.matrix(cov1[which(colnames(cov1)==blnetevn$u1)])
      bbubble <- data.frame(cbind(c(rownames(blaububble)),blaububble))
      colnames(bbubble) <- c("ego",as.matrix(cov1[which(colnames(cov1)==blnetevn$u1)]))
      thdlevel <- gwindow("Blau Bubble Results", width=515, height=30)
      thdg <- gpanedgroup(cont = thdlevel, horizontal = TRUE)
      tg <- ggroup(cont = thdg, horizontal = TRUE)
      button <- gbutton("Blau Distance Matrix", cont = tg, handler = function(h, ...) {
        fthlevel1 <- gwindow("Blau Distance Matrix",width = 800, height = 600)
        fg1 <- ggroup(horizontal = FALSE, cont = fthlevel1)
        button1 <- gbutton("Save as csv file: blaudistance.csv", expand = FALSE, cont = fg1, handler = function(h, ...) {
          write.table(distmat, "blaudistance.csv", row.names=F, col.names=T, sep=",")
        })
        button2 <- gbutton("Save as R file: blaudistance.Rdata", expand = FALSE, cont = fg1, handler = function(h, ...) {
          save(distmat, file="blaudistance.Rdata")
        })
        button3 <- gbutton("Save as SAS file: blaudistance.txt & blaudistance.sas", expand = FALSE, cont = fg1, handler = function(h, ...) {
          write.foreign(distmat, "blaudistance.txt", "blaudistance.sas",   package="SAS")
        })
        button4 <- gbutton("Save as Stata file: blaudistance.dta", expand = FALSE, cont = fg1, handler = function(h, ...) {
          write.dta(distmat, ("blaudistance.dta"))
        })
        button5 <- gbutton("Save as SPSS file: blaudistance.txt & blaudistance.sps", expand = FALSE, cont = fg1, handler = function(h, ...) {
          write.foreign(distmat, "blaudistance.txt", "blaudistance.sps",   package="SPSS")
        })
        gseparator(cont = fg1)
        vars <- gdf(distmat, expand = TRUE, fill=TRUE, cont = fg1)
      })
      addSpace(tg, 5) 
      button <- gbutton("Blau Bubble Matrix", cont = tg, handler = function(h, ...) {
        fthlevel1 <- gwindow("Blau Bubble Matrix",width = 800, height = 600)
        fg1 <- ggroup(horizontal = FALSE, cont = fthlevel1)
        button1 <- gbutton("Save as csv file: blaububble.csv", expand = FALSE, cont = fg1, handler = function(h, ...) {
          write.table(bbubble, "blaububble.csv", row.names=F, col.names=T, sep=",")
        })
        button2 <- gbutton("Save as R file: blaububble.Rdata", expand = FALSE, cont = fg1, handler = function(h, ...) {
          save(bbubble, file="blaububble.Rdata")
        })
        button3 <- gbutton("Save as SAS file: blaububble.txt & blaububble.sas", expand = FALSE, cont = fg1, handler = function(h, ...) {
          write.foreign(bbubble, "blaububble.txt", "blaububble.sas",   package="SAS")
        })
        button4 <- gbutton("Save as Stata file: blaububble.dta", expand = FALSE, cont = fg1, handler = function(h, ...) {
          write.dta(bbubble, ("blaububble.dta"))
        })
        button5 <- gbutton("Save as SPSS file: blaububble.txt & blaububble.sps", expand = FALSE, cont = fg1, handler = function(h, ...) {
          write.foreign(bbubble, "blaububble.txt", "blaububble.sps",   package="SPSS")
        })
        gseparator(cont = fg1)
        vars <- gdf(bbubble, expand = TRUE, fill=TRUE, cont = fg1)
      })
      addSpace(tg, 5) 
      button <- gbutton("Blau Bubble List", cont = tg, handler = function(h, ...) {
        blaublist <- data.frame(as.matrix(network(as.matrix(blaububble)),matrix.type="edgelist"))
        if (nrow(blaublist)>0) {
          dim.distance <- rep(0,nrow(blaublist))
          distmattmp <- distmat[,-1]
          for (x in 1:nrow(blaublist)){
            dim.distance[x] <- distmattmp[blaublist[x,1],blaublist[x,2]]
          }
          rm(x)
          blaublist <- cbind(blaublist,dim.distance)
          colnames(blaublist) <- c("i","j","dim.distance")
          if ("el" %in% ls(envir=blnetevn)) {
            geodesic.distance <- present.edges <- rep(0,nrow(blaublist))
            tempadj <- symmetrize(adj1,rule="weak")
            tempel <- data.frame(as.matrix(network(tempadj),matrix.type="edgelist"))
            colnames(tempel) <- c("i","j")
            gd <- geodist(network(symmetrize(adj1,rule="weak")))$gdist
            for (x in 1:nrow(blaublist)){
              if (nrow(merge(blaublist[x,1:2],tempel))>0) present.edges[x] <- 1
              geodesic.distance[x] <- gd[blaublist[x,1],blaublist[x,2]]
            }
            blaublist <- cbind(blaublist,present.edges,geodesic.distance)
            rm(x)
          }
          blaublist[,1] <- as.matrix(cov1[which(colnames(cov1)==blnetevn$u1)])[blaublist[,1]]
          blaublist[,2] <- as.matrix(cov1[which(colnames(cov1)==blnetevn$u1)])[blaublist[,2]]
        } else {
          if ("el" %in% ls(envir=blnetevn)) {
            blaublist <- data.frame(i=double(),j=double(),dim.distance=double(),present.edges=double(),geodesic.distance=double())
          } else {
            blaublist <- data.frame(i=double(),j=double(),dim.distance=double())
          }
        }
        fthlevel2 <- gwindow("Blau Bubble List",width = 800, height = 600)
        fg2 <- ggroup(horizontal = FALSE, cont = fthlevel2)
        button1 <- gbutton("Save as csv file: blaububblelist.csv", expand = FALSE, cont = fg2, handler = function(h, ...) {
          write.table(blaublist, "blaububblelist.csv", row.names=F, col.names=T, sep=",")
        })
        button2 <- gbutton("Save as R file: blaububblelist.Rdata", expand = FALSE, cont = fg2, handler = function(h, ...) {
          save(blaublist, file="blaububblelist.Rdata")
        })
        button3 <- gbutton("Save as SAS file: blaububble.txt & blaububble.sas", expand = FALSE, cont = fg2, handler = function(h, ...) {
          write.foreign(blaublist, "blaububblelist.txt", "blaububblelist.sas",   package="SAS")
        })
        button4 <- gbutton("Save as Stata file: blaububble.dta", expand = FALSE, cont = fg2, handler = function(h, ...) {
          write.dta(blaublist, ("blaububblelist.dta"))
        })
        button5 <- gbutton("Save as SPSS file: blaububble.txt & blaububble.sps", expand = FALSE, cont = fg2, handler = function(h, ...) {
          write.foreign(blaublist, "blaububblelist.txt", "blaububblelist.sps",   package="SPSS")
        })
        gseparator(cont = fg2)
        vars <- gdf(blaublist, expand = TRUE, fill=TRUE, cont = fg2)
      })
      addSpace(tg, 5) 
      button <- gbutton("Nodal Bubble List", cont = tg, handler = function(h, ...) {
        nodes <- cov1[which(colnames(cov1)==blnetevn$u1)]
        co_bubble <- degree <- coincidence <- rep(0,k)
        co_bubble_list <- rep("",k)
        for (x in 1:k) {
          co_bubble[x] <- sum(blaububble[x,])
          co_bubble_list[x] <- paste(colnames(blaububble)[which(blaububble[x,]==1)], collapse = " ")
        }
        rm(x)
        nodalbubble <- cbind(nodes,co_bubble,co_bubble_list)
        if ("el" %in% ls(envir=blnetevn)) {
          tempadj <- symmetrize(adj1,rule="weak")
          colnames(tempadj) <- as.matrix(cov1[which(colnames(cov1)==blnetevn$u1)])
          degree <- coincidence <- rep(0,k)
          alter_list <- coincidence_list <- rep("",k)
          for (x in 1:k) {
            degree[x] <- sum(tempadj[x,])
            alter_list[x] <- paste(colnames(tempadj)[which(tempadj[x,]==1)], collapse = " ")
            y <- intersect(colnames(blaububble)[which(blaububble[x,]==1)],colnames(tempadj)[which(tempadj[x,]==1)])
            coincidence[x] <- length(y)
            coincidence_list[x] <- paste(y, collapse = " ")
          }
          rm(x,y)
          nodalbubble <- cbind(nodalbubble,degree,alter_list,coincidence,coincidence_list)
        }
        fthlevel3 <- gwindow("Nodal Bubble List",width = 800, height = 600)
        fg3 <- ggroup(horizontal = FALSE, cont = fthlevel3)
        button1 <- gbutton("Save as csv file: nodalbubblelist.csv", expand = FALSE, cont = fg3, handler = function(h, ...) {
          write.table(nodalbubble, "nodalbubble.csv", row.names=F, col.names=T, sep=",")
        })
        button2 <- gbutton("Save as R file: nodalbubblelist.Rdata", expand = FALSE, cont = fg3, handler = function(h, ...) {
          save(nodalbubble, file="nodalbubblelist.Rdata")
        })
        button3 <- gbutton("Save as SAS file: nodalbubblelist.txt & nodalbubblelist.sas", expand = FALSE, cont = fg3, handler = function(h, ...) {
          write.foreign(nodalbubble, "nodalbubblelist.txt", "nodalbubblelist.sas",   package="SAS")
        })
        button4 <- gbutton("Save as Stata file: nodalbubblelist.dta", expand = FALSE, cont = fg3, handler = function(h, ...) {
          write.dta(nodalbubble, "nodalbubblelist.dta")
        })
        button5 <- gbutton("Save as SPSS file: nodalbubblelist.txt & nodalbubblelist.sps", expand = FALSE, cont = fg3, handler = function(h, ...) {
          write.foreign(nodalbubble, "nodalbubblelist.txt", "nodalbubblelist.sps",   package="SPSS")
        })
        gseparator(cont = fg3)
        vars <- gdf(nodalbubble, expand = TRUE, fill=TRUE, cont = fg3)
      })
    })
  }})}
}

