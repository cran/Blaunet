showdimensions <- function(h,...) {
  if ("cov" %in% ls(envir=blnetevn)==FALSE) {gmessage("Sorry! Attribute file is not loaded.", parent = window)} else {
  assign("c1",character(0),envir=blnetevn)
  assign("c2",character(0),envir=blnetevn)
  assign("c3","all",envir=blnetevn)
  assign("c4",character(0),envir=blnetevn)
  assign("c5",character(0),envir=blnetevn)
  assign("c6",0.05,envir=blnetevn)
  assign("c7",character(0),envir=blnetevn)
  assign("c8",character(0),envir=blnetevn)
  assign("c9",character(0),envir=blnetevn)
  assign("m1",names(blnetevn$cov),envir=blnetevn)
  assign("m3",names(blnetevn$cov),envir=blnetevn)
  toplevel <- gwindow("Salient Dimensions", width=600, height=800, parent = window, visible=FALSE)
  cg <- ggroup(horizontal = TRUE,cont = toplevel)
  tbl0 <- gtable(blnetevn$m1,expand=TRUE,multiple=TRUE,cont=cg)
  cg1 <- ggroup(horizontal = FALSE, cont = cg)
  gbplus1 <- gbutton("+", cont=cg1)
  gbminus1 <- gbutton("-", cont=cg1)
  gbplus2 <- gbutton("+", cont=cg1)
  gbminus2 <- gbutton("-", cont=cg1)
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
  addSpring(cg1) 
  cg2 <- ggroup(horizontal = FALSE, cont = cg)
  c1temp <- data.frame(Node.ids="",stringsAsFactors=FALSE)
  c2temp <- data.frame(Ecology.ids="",stringsAsFactors=FALSE)
  c4temp <- data.frame(Dimensions=rep("",length(blnetevn$m3)),stringsAsFactors=FALSE)
  c5temp <- data.frame(Groups=rep("",length(blnetevn$m3)),stringsAsFactors=FALSE)
  tbl1 <- gtable(c1temp,index=TRUE, cont=cg2)
  size(tbl1)[2] <- 50
  tbl2 <- gtable(c2temp,cont=cg2)
  size(tbl2)[2] <- 50
  tbl4 <- gtable(c4temp,expand=TRUE,multiple=TRUE,cont=cg2)
  tbl5 <- gtable(c5temp,expand=TRUE,multiple=TRUE,cont=cg2)
  glabel("Alpha (0.05 by default)",cont=cg2)
  gcombobox(c(0.05,0.001,0.01,0.1), cont=cg2, handler = function(h,...) assign("c6",svalue(h$obj),envir=blnetevn)) 
  addHandlerClicked(gbplus1, handler = function(h,...) {
    temp <- svalue(tbl0)
    if ("" %in% temp==FALSE & tbl1[1]=="" & length(temp)==1) {
      assign("m1",blnetevn$m3[which(blnetevn$m3 %in% setdiff(blnetevn$m1,temp))],envir=blnetevn) 
      km1 <- c(blnetevn$m1,rep("",length(blnetevn$m3)-length(blnetevn$m1)))
      for (i in 1:length(blnetevn$m3)) tbl0[i] <- km1[i]
      assign("c1",blnetevn$m3[which(blnetevn$m3 %in% union(blnetevn$c1,temp))],envir=blnetevn) 
      tbl1[1] <- blnetevn$c1
    }
  })
  addHandlerClicked(gbminus1, handler = function(h,...) {
    temp <- svalue(tbl1)
    if ("" %in% temp==FALSE & length(temp)==1) {
      assign("m1",blnetevn$m3[which(blnetevn$m3 %in% union(blnetevn$m1,temp))],envir=blnetevn) 
      km1 <- c(blnetevn$m1,rep("",length(blnetevn$m3)-length(blnetevn$m1)))
      for (i in 1:length(blnetevn$m3)) tbl0[i] <- km1[i]
      assign("c1",character(0),envir=blnetevn) 
      tbl1[1] <- ""
    }
  })
  addHandlerClicked(gbplus2, handler = function(h,...) {
    temp <- svalue(tbl0)
    if ("" %in% temp==FALSE & tbl2[1]=="" & length(temp)==1) {
      assign("m1",blnetevn$m3[which(blnetevn$m3 %in% setdiff(blnetevn$m1,temp))],envir=blnetevn) 
      km1 <- c(blnetevn$m1,rep("",length(blnetevn$m3)-length(blnetevn$m1)))
      for (i in 1:length(blnetevn$m3)) tbl0[i] <- km1[i]
      assign("c2",blnetevn$m3[which(blnetevn$m3 %in% union(blnetevn$c2,temp))],envir=blnetevn) 
      tbl2[1] <- blnetevn$c2
    }
  })
  addHandlerClicked(gbminus2, handler = function(h,...) {
    temp <- svalue(tbl2)
    if ("" %in% temp==FALSE & length(temp)==1) {
      assign("m1",blnetevn$m3[which(blnetevn$m3 %in% union(blnetevn$m1,temp))],envir=blnetevn) 
      km1 <- c(blnetevn$m1,rep("",length(blnetevn$m3)-length(blnetevn$m1)))
      for (i in 1:length(blnetevn$m3)) tbl0[i] <- km1[i]
      assign("c2",character(0),envir=blnetevn) 
      tbl2[1] <- ""
    }
  })
  addHandlerClicked(gbplus4, handler = function(h,...) {
    temp <- svalue(tbl0)
    if ("" %in% temp==FALSE & length(temp)>0) {
      assign("m1",blnetevn$m3[which(blnetevn$m3 %in% setdiff(blnetevn$m1,temp))],envir=blnetevn) 
      km1 <- c(blnetevn$m1,rep("",length(blnetevn$m3)-length(blnetevn$m1)))
      for (i in 1:length(blnetevn$m3)) tbl0[i] <- km1[i]
      assign("c4",blnetevn$m3[which(blnetevn$m3 %in% union(blnetevn$c4,temp))],envir=blnetevn) 
      kc4 <- c(blnetevn$c4,rep("",length(blnetevn$m3)-length(blnetevn$c4)))
      for (j in 1:length(blnetevn$m3)) tbl4[j] <- kc4[j]
    }
  })
  addHandlerClicked(gbminus4, handler = function(h,...) {
    temp <- svalue(tbl4)
    if ("" %in% temp==FALSE & length(temp)>0) {
      assign("m1",blnetevn$m3[which(blnetevn$m3 %in% union(blnetevn$m1,temp))],envir=blnetevn) 
      km1 <- c(blnetevn$m1,rep("",length(blnetevn$m3)-length(blnetevn$m1)+1))
      for (i in 1:length(blnetevn$m3)) tbl0[i] <- km1[i]
      assign("c4",blnetevn$m3[which(blnetevn$m3 %in% setdiff(blnetevn$c4,temp))],envir=blnetevn) 
      kc4 <- c(blnetevn$c4,rep("",length(blnetevn$m3)-length(blnetevn$c4)+1))
      for (j in 1:length(blnetevn$m3)) tbl4[j] <- kc4[j]
    }
  })
  addHandlerClicked(gbplus5, handler = function(h,...) {
    temp <- svalue(tbl0)
    if ("" %in% temp==FALSE & length(temp)>0) {
      assign("m1",blnetevn$m3[which(blnetevn$m3 %in% setdiff(blnetevn$m1,temp))],envir=blnetevn) 
      km1 <- c(blnetevn$m1,rep("",length(blnetevn$m3)-length(blnetevn$m1)))
      for (i in 1:length(blnetevn$m3)) tbl0[i] <- km1[i]
      assign("c5",blnetevn$m3[which(blnetevn$m3 %in% union(blnetevn$c5,temp))],envir=blnetevn) 
      kc5 <- c(blnetevn$c5,rep("",length(blnetevn$m3)-length(blnetevn$c5)))
      for (j in 1:length(blnetevn$m3)) tbl5[j] <- kc5[j]
    }
  })
  addHandlerClicked(gbminus5, handler = function(h,...) {
    temp <- svalue(tbl5)
    if ("" %in% temp==FALSE & length(temp)>0) {
      assign("m1",blnetevn$m3[which(blnetevn$m3 %in% union(blnetevn$m1,temp))],envir=blnetevn) 
      km1 <- c(blnetevn$m1,rep("",length(blnetevn$m3)-length(blnetevn$m1)+1))
      for (i in 1:length(blnetevn$m3)) tbl0[i] <- km1[i]
      assign("c5",blnetevn$m3[which(blnetevn$m3 %in% setdiff(blnetevn$c5,temp))],envir=blnetevn) 
      kc5 <- c(blnetevn$c5,rep("",length(blnetevn$m3)-length(blnetevn$c5)+1))
      for (j in 1:length(blnetevn$m3)) tbl5[j] <- kc5[j]
    }
  })
  visible(toplevel) <- TRUE
  button <- gbutton("Continue", expand = FALSE, cont = cg2, handler = function(h, ...) {
    if (length(blnetevn$c1)==0 | length(blnetevn$c4)==0) {gmessage("Missing required information.", parent = toplevel)} else {
    dispose(toplevel)
    if ('el' %in% ls(envir=blnetevn)==FALSE & length(blnetevn$c5)==0) {
      gmessage("You must select group options.")
    } else {
      if (length(blnetevn$c5)>0) {
        if (length(blnetevn$c2)!=0) {
          seclevel <- gwindow("Salient Dimension Options", width=600, height=600, parent = window)
          secg <- ggroup(cont = seclevel, use.scrollwindow=T, horizontal = FALSE)
          secgv <- gvbox(cont = secg)
          tbl <- gformlayout(cont = secgv)
		  gcheckboxgroup(c("all",as.matrix(unique(blnetevn$cov[which(colnames(blnetevn$cov)==blnetevn$c2)]))), cont = tbl, label="Please select which ecology you want to identify salient dimensions:", handler = function(h,...){
            assign("c3",svalue(h$obj),envir=blnetevn)
          })
          gcheckboxgroup(blnetevn$c4, cont = tbl, label="Please identify categorical variables:", handler = function(h,...){
            assign("c7",svalue(h$obj),envir=blnetevn)
          })
          button <- gbutton("Continue", expand = FALSE, cont = secg, handler = function(h, ...) {
            dispose(seclevel)
            if ("all" %in% blnetevn$c3) {
              cov1 <- blnetevn$cov
			  cov1[is.na(cov1)] <- 0
              if ('el' %in% ls(envir=blnetevn)) adj1 <- as.matrix(blnetevn$adj)
            } else {
              cov1 <- blnetevn$cov[which(blnetevn$cov[,which(colnames(blnetevn$cov)==blnetevn$c2)] %in% blnetevn$c3),]
			  cov1[is.na(cov1)] <- 0
              if ('el' %in% ls(envir=blnetevn)) {adj1 <- as.matrix(blnetevn$adj[which(blnetevn$cov[,which(colnames(blnetevn$cov)==blnetevn$c2)] %in% blnetevn$c3),which(blnetevn$cov[,which(colnames(blnetevn$cov)==blnetevn$c2)] %in% blnetevn$c3)])}
            }
            if ('el' %in% ls(envir=blnetevn)) {
              tempadj <- symmetrize(adj1,rule="weak")
              deg <- degree(network(tempadj), cmode="outdegree")
              bet <- betweenness(network(tempadj))
              eig <- evcent(network(tempadj))
              tempel <- as.matrix(network(tempadj),matrix.type="edgelist")
              lclu <- rep(0,nrow(cov1))
              for (i in 1:nrow(cov1)) {
                k <- tempel[which(tempel[,1]==i),2]
                if (length(k)>=2) {
                  m <- rbind(tempel, t(combn(k,2)))
                  lclu[i] <- nrow(m[duplicated(m), , drop = FALSE])/nrow(t(combn(k,2)))
                } else {lclu[i] <- NA}
              }
              rm(i,k,m)
              k <- c()
              for (i in 1:length(blnetevn$c5)) {
                alteringroup <- rep(0,nrow(cov1))
                for (j in 1:nrow(cov1)) {
                  if (length(tempel[which(tempel[,1]==j),2])>=1) alteringroup[j] <- sum(cov1[tempel[which(tempel[,1]==j),2],which(colnames(cov1)==blnetevn$c5[i])])
                  else alteringroup[j] <- 0
                }
                temp <- cbind(cov1[which(colnames(cov1)==blnetevn$c5[i])],cov1[which(colnames(cov1) %in% blnetevn$c5[-i])],cov1[which(colnames(cov1) %in% blnetevn$c4)],deg,bet,eig,lclu,alteringroup)
                fit1 <- glm(temp, family=binomial)
                k <- unique(c(k,names(which(summary(fit1)$coefficients[,4]<blnetevn$c6))))
              }
              assign("c8",blnetevn$c4[which(blnetevn$c4 %in% k)],envir=blnetevn) 
              if (length(blnetevn$c8)>0) {
                g <- c()
                gm <- list()
                for (i in 1:length(blnetevn$c5)) {
                  g <- c(g,which(names(blnetevn$cov)==blnetevn$c5[i]))
                  gm[[i]] <- matrix(rep(0,nrow(blnetevn$cov)*nrow(blnetevn$cov)),nrow=nrow(blnetevn$cov))
                }
                for (l in 1:length(blnetevn$c5)) {
                  tempgm <- cbind(c(1:nrow(blnetevn$cov)),cov1[,g[l]])
                  tempgm <- tempgm[which(tempgm[,2]==1),]
                  tempgm <- t(combn(tempgm[,1],2))
                  tempgm <- network(tempgm,directed=FALSE)
                  tempgm1 <- nrow(blnetevn$cov)-network.size(tempgm)
                  if (tempgm1>0) add.vertices(tempgm,tempgm1)
                  gm[[l]] <- as.matrix(tempgm)
                }
                rm(i,l)
                for (l in 1:length(blnetevn$c5)) {
                  n <- network(gm[[l]])
                  emf <- "n~"
                  for (m in 1:length(blnetevn$c8)) {
                    k1 <- cov1[,which(names(cov1)==blnetevn$c8[m])]
                    k1[which(is.na(k1)==TRUE)] <- round(mean(k1,na.rm=TRUE))
                    n %v% blnetevn$c8[m] <- k1
                    if (blnetevn$c8[m] %in% blnetevn$c7) {
                      emf <- paste(emf,"+nodematch(","\'",blnetevn$c8[m],"\'",")",sep="")                    
                    } else {
                      emf <- paste(emf,"+absdiff(","\'",blnetevn$c8[m],"\'",")",sep="")                    
                    }
                  }
                  o <- setdiff(union(l,1:length(blnetevn$c5)),intersect(l,1:length(blnetevn$c5)))
                  if (nrow(cov1)<=500) {
                    for (m in 1:length(o)) emf <- paste(emf,"+edgecov(gm[[",o[m],"]])",sep="")
                    emf <- paste(emf,"+edgecov(tempadj)",sep="")
                  }
                  emf <- paste(substr(emf, 1, 2), substr(emf, 4, nchar(emf)), sep='')
                  em <- ergm(as.formula(emf))
                  blnetevn$c9 <- union(blnetevn$c9,blnetevn$c8[which(summary(em)$coefs[1:length(blnetevn$c8),4]<blnetevn$c6)])
                }
                assign("c9",blnetevn$c9,envir=blnetevn) 
              } else {assign("c9","",envir=blnetevn)}
              k <- data.frame(blnetevn$c9)
              colnames(k) <- "Dimensions"  
              thdlevel <- gwindow("Salient Dimensions",width = 200, height = 400)
              tg <- ggroup(horizontal = FALSE, cont = thdlevel)
              dims <- gtable(k, expand = TRUE, cont = tg)  
            } else {
              k <- c()
              for (j in 1:length(blnetevn$c5)) {
                temp <- cbind(cov1[which(colnames(cov1)==blnetevn$c5[j])],cov1[which(colnames(cov1) %in% blnetevn$c5[-j])],cov1[which(colnames(cov1) %in% blnetevn$c4)])
                fit1 <- glm(temp, family=binomial)
                k <- unique(c(k,names(which(summary(fit1)$coefficients[,4]<blnetevn$c6))))
              }
              assign("c8",blnetevn$c4[which(blnetevn$c4 %in% k)],envir=blnetevn) 
              if (length(blnetevn$c8)>0) {
                g <- c()
                gm <- list()
                for (i in 1:length(blnetevn$c5)) {
                  g <- c(g,which(names(cov1)==blnetevn$c5[i]))
                  gm[[i]] <- matrix(rep(0,nrow(cov1)*nrow(cov1)),nrow=nrow(cov1))
                }
                for (l in 1:length(blnetevn$c5)) {
                  tempgm <- cbind(c(1:nrow(blnetevn$cov)),cov1[,g[l]])
                  tempgm <- tempgm[which(tempgm[,2]==1),]
                  tempgm <- t(combn(tempgm[,1],2))
                  tempgm <- network(tempgm,directed=FALSE)
                  tempgm1 <- nrow(blnetevn$cov)-network.size(tempgm)
                  if (tempgm1>0) add.vertices(tempgm,tempgm1)
                  gm[[l]] <- as.matrix(tempgm)
                }
                rm(i,l)
                for (l in 1:length(blnetevn$c5)) {
                  n <- network(gm[[l]])
                  emf <- "n~"
                  for (m in 1:length(blnetevn$c8)) {
                    k1 <- cov1[,which(names(cov1)==blnetevn$c8[m])]
                    k1[which(is.na(k1)==TRUE)] <- round(mean(k1,na.rm=TRUE))
                    n %v% blnetevn$c8[m] <- k1
                    if (blnetevn$c8[m] %in% blnetevn$c7) {
                      emf <- paste(emf,"+nodematch(","\'",blnetevn$c8[m],"\'",")",sep="")                    
                    } else {
                      emf <- paste(emf,"+absdiff(","\'",blnetevn$c8[m],"\'",")",sep="")                    
                    }
                  }
                  o <- setdiff(union(l,1:length(blnetevn$c5)),intersect(l,1:length(blnetevn$c5)))
                  if (nrow(cov1)<=500) {
                    for (m in 1:length(o)) emf <- paste(emf,"+edgecov(gm[[",o[m],"]])",sep="")
                  }
                  emf <- paste(substr(emf, 1, 2), substr(emf, 4, nchar(emf)), sep='')
                  em <- ergm(as.formula(emf))
                  blnetevn$c9 <- union(blnetevn$c9,blnetevn$c8[which(summary(em)$coefs[1:length(blnetevn$c8),4]<blnetevn$c6)])
                }
                assign("c9",blnetevn$c9,envir=blnetevn) 
              } else {assign("c9","",envir=blnetevn)}
              k <- data.frame(blnetevn$c9)
              colnames(k) <- "Dimensions"  
              thdlevel <- gwindow("Salient Dimensions",width = 200, height = 400)
              tg <- ggroup(horizontal = FALSE, cont = thdlevel)
              dims <- gtable(k, expand = TRUE, cont = tg)   
            }
          })
        } else {
          seclevel <- gwindow("Salient Dimension Options", width=600, height=600, parent = window)
          secg <- ggroup(cont = seclevel, use.scrollwindow=T, horizontal = FALSE)
		  secgv <- gvbox(cont = secg)
          tbl <- gformlayout(cont = secgv)
		  gcheckboxgroup(blnetevn$c4, cont = tbl, label="Please identify categorical variables:", handler = function(h,...){
            assign("c7",svalue(h$obj),envir=blnetevn)
          })
          button <- gbutton("Continue", expand = FALSE, cont = secg, handler = function(h, ...) {
            dispose(seclevel)
            cov1 <- blnetevn$cov
			cov1[is.na(cov1)] <- 0
            if ('el' %in% ls(envir=blnetevn)) {
              adj1 <- blnetevn$adj
              tempadj <- symmetrize(adj1,rule="weak")
              deg <- degree(network(tempadj), cmode="outdegree")
              bet <- betweenness(network(tempadj))
              eig <- evcent(network(tempadj))
              tempel <- as.matrix(network(tempadj),matrix.type="edgelist")
              lclu <- rep(0,nrow(cov1))
              for (i in 1:nrow(cov1)) {
                k <- tempel[which(tempel[,1]==i),2]
                if (length(k)>=2) {
                  m <- rbind(tempel, t(combn(k,2)))
                  lclu[i] <- nrow(m[duplicated(m), , drop = FALSE])/nrow(t(combn(k,2)))
                } else {lclu[i] <- NA}
              }
              rm(i,k,m)
              k <- c()
              for (i in 1:length(blnetevn$c5)) {
                alteringroup <- rep(0,nrow(cov1))
                for (j in 1:nrow(cov1)) {
                  if (length(tempel[which(tempel[,1]==j),2])>=1) alteringroup[j] <- sum(cov1[tempel[which(tempel[,1]==j),2],which(colnames(cov1)==blnetevn$c5[i])])
                  else alteringroup[j] <- 0
                }
                temp <- cbind(cov1[which(colnames(cov1)==blnetevn$c5[i])],cov1[which(colnames(cov1) %in% blnetevn$c5[-i])],cov1[which(colnames(cov1) %in% blnetevn$c4)],deg,bet,eig,lclu,alteringroup)
                fit1 <- glm(temp, family=binomial)
                k <- unique(c(k,names(which(summary(fit1)$coefficients[,4]<blnetevn$c6))))
              }
              assign("c8",blnetevn$c4[which(blnetevn$c4 %in% k)],envir=blnetevn)
              if (length(blnetevn$c8)>0) { 
                g <- c()
                gm <- list()
                for (i in 1:length(blnetevn$c5)) {
                  g <- c(g,which(names(blnetevn$cov)==blnetevn$c5[i]))
                  gm[[i]] <- matrix(rep(0,nrow(blnetevn$cov)*nrow(blnetevn$cov)),nrow=nrow(blnetevn$cov))
                }
                for (l in 1:length(blnetevn$c5)) {
                  tempgm <- cbind(c(1:nrow(blnetevn$cov)),cov1[,g[l]])
                  tempgm <- tempgm[which(tempgm[,2]==1),]
                  tempgm <- t(combn(tempgm[,1],2))
                  tempgm <- network(tempgm,directed=FALSE)
                  tempgm1 <- nrow(blnetevn$cov)-network.size(tempgm)
                  if (tempgm1>0) add.vertices(tempgm,tempgm1)
                  gm[[l]] <- as.matrix(tempgm)
                }
                rm(i,l)
                for (l in 1:length(blnetevn$c5)) {
                  n <- network(gm[[l]])
                  emf <- "n~"
                  for (m in 1:length(blnetevn$c8)) {
                    k1 <- cov1[,which(names(cov1)==blnetevn$c8[m])]
                    k1[which(is.na(k1)==TRUE)] <- round(mean(k1,na.rm=TRUE))
                    n %v% blnetevn$c8[m] <- k1
                    if (blnetevn$c8[m] %in% blnetevn$c7) {
                      emf <- paste(emf,"+nodematch(","\'",blnetevn$c8[m],"\'",")",sep="")                    
                    } else {
                      emf <- paste(emf,"+absdiff(","\'",blnetevn$c8[m],"\'",")",sep="")                    
                    }
                  }
                  o <- setdiff(union(l,1:length(blnetevn$c5)),intersect(l,1:length(blnetevn$c5)))
                  if (nrow(cov1)<=500) {
                    for (m in 1:length(o)) emf <- paste(emf,"+edgecov(gm[[",o[m],"]])",sep="")
                    emf <- paste(emf,"+edgecov(as.matrix(network(tempel)))",sep="")
                  }
                  emf <- paste(substr(emf, 1, 2), substr(emf, 4, nchar(emf)), sep='')
                  em <- ergm(as.formula(emf))
                  blnetevn$c9 <- union(blnetevn$c9,blnetevn$c8[which(summary(em)$coefs[1:length(blnetevn$c8),4]<blnetevn$c6)])
                }
                assign("c9",blnetevn$c9,envir=blnetevn) 
              } else {assign("c9",character(0),envir=blnetevn)}
              k <- data.frame(blnetevn$c9)
              colnames(k) <- "Dimensions"          
              thdlevel <- gwindow("Salient Dimensions",width = 300, height = 400)
              tg <- ggroup(horizontal = FALSE, cont = thdlevel)
              dims <- gtable(k, expand = TRUE, cont = tg)    
            } else {
              k <- c()
              for (j in 1:length(blnetevn$c5)) {
                temp <- cbind(cov1[which(colnames(cov1)==blnetevn$c5[j])],cov1[which(colnames(cov1) %in% blnetevn$c5[-j])],cov1[which(colnames(cov1) %in% blnetevn$c4)])
                fit1 <- glm(temp, family=binomial)
                k <- unique(c(k,names(which(summary(fit1)$coefficients[,4]<blnetevn$c6))))
              }
              assign("c8",blnetevn$c4[which(blnetevn$c4 %in% k)],envir=blnetevn) 
              if (length(blnetevn$c8)>0) {
                g <- c()
                gm <- list()
                for (i in 1:length(blnetevn$c5)) {
                  g <- c(g,which(names(blnetevn$cov)==blnetevn$c5[i]))
                  gm[[i]] <- matrix(rep(0,nrow(blnetevn$cov)*nrow(blnetevn$cov)),nrow=nrow(blnetevn$cov))
                }
                for (l in 1:length(blnetevn$c5)) {
                  tempgm <- cbind(c(1:nrow(blnetevn$cov)),cov1[,g[l]])
                  tempgm <- tempgm[which(tempgm[,2]==1),]
                  tempgm <- t(combn(tempgm[,1],2))
                  tempgm <- network(tempgm,directed=FALSE)
                  tempgm1 <- nrow(blnetevn$cov)-network.size(tempgm)
                  if (tempgm1>0) add.vertices(tempgm,tempgm1)
                  gm[[l]] <- as.matrix(tempgm)
                }
                rm(i,l)
                for (l in 1:length(blnetevn$c5)) {
                  n <- network(gm[[l]])
                  emf <- "n~"
                  for (m in 1:length(blnetevn$c8)) {
                    k1 <- cov1[,which(names(cov1)==blnetevn$c8[m])]
                    k1[which(is.na(k1)==TRUE)] <- round(mean(k1,na.rm=TRUE))
                    n %v% blnetevn$c8[m] <- k1
                    if (blnetevn$c8[m] %in% blnetevn$c7) {
                      emf <- paste(emf,"+nodematch(","\'",blnetevn$c8[m],"\'",")",sep="")                    
                    } else {
                      emf <- paste(emf,"+absdiff(","\'",blnetevn$c8[m],"\'",")",sep="")                    
                    }
                  }
                  o <- setdiff(union(l,1:length(blnetevn$c5)),intersect(l,1:length(blnetevn$c5)))
                  if (nrow(cov1)<=500) {
                    for (m in 1:length(o)) emf <- paste(emf,"+edgecov(gm[[",o[m],"]])",sep="")
                  }
                  emf <- paste(substr(emf, 1, 2), substr(emf, 4, nchar(emf)), sep='')
                  em <- ergm(as.formula(emf))
                  blnetevn$c9 <- union(blnetevn$c9,blnetevn$c8[which(summary(em)$coefs[1:length(blnetevn$c8),4]<blnetevn$c6)])
                }
                assign("c9",blnetevn$c9,envir=blnetevn) 
              } else {assign("c9",character(0),envir=blnetevn)}
              k <- data.frame(blnetevn$c9)
              colnames(k) <- "Dimensions"  
              thdlevel <- gwindow("Salient Dimensions",width = 300, height = 400)
              tg <- ggroup(horizontal = FALSE, cont = thdlevel)
              dims <- gtable(k, expand = TRUE, cont = tg)       
            }
          })
        } 
	} else {
        if (length(blnetevn$c2)!=0) {
          seclevel <- gwindow("Salient Dimension Options", width=600, height=600, parent = window)
          secg <- ggroup(cont = seclevel, use.scrollwindow=T, horizontal = FALSE)
		  secgv <- gvbox(cont = secg)
          tbl <- gformlayout(cont = secgv)
		  gradio(c("all",as.matrix(unique(blnetevn$cov[which(colnames(blnetevn$cov)==blnetevn$c2)]))), cont = tbl, selected = 1, label="Please select which ecology you want to identify salient dimensions:", handler = function(h,...){
            assign("c3",svalue(h$obj),envir=blnetevn)
          })
          gcheckboxgroup(blnetevn$c4, cont = tbl, label="Please identify categorical variables:", handler = function(h,...){
            assign("c7",svalue(h$obj),envir=blnetevn)
          })
          button <- gbutton("Continue", expand = FALSE, cont = secg, handler = function(h, ...) {
            dispose(seclevel)
            if (blnetevn$c3=="all") {
              cov1 <- blnetevn$cov
			  cov1[is.na(cov1)] <- 0
              if ('adj' %in% ls(envir=blnetevn)) adj1 <- blnetevn$adj
            } else {
              cov1 <- blnetevn$cov[which(blnetevn$cov[which(colnames(blnetevn$cov)==blnetevn$c2)]==blnetevn$c3),]
			  cov1[is.na(cov1)] <- 0
              if ('adj' %in% ls(envir=blnetevn)) adj1 <- blnetevn$adj[which(blnetevn$cov[which(colnames(blnetevn$cov)==blnetevn$c2)]==blnetevn$c3),which(blnetevn$cov[which(colnames(blnetevn$cov)==blnetevn$c2)]==blnetevn$c3)]
            }
            if ('el' %in% ls(envir=blnetevn)) {
              n <- network(adj1)
              emf <- "n~edges+mutual"
              for (m in 1:length(blnetevn$c4)) {
                k1 <- cov1[,which(names(cov1)==blnetevn$c4[m])]
                k1[which(is.na(k1)==TRUE)] <- round(mean(k1,na.rm=TRUE))
                n %v% blnetevn$c4[m] <- k1
                if (blnetevn$c4[m] %in% blnetevn$c7) {
                  emf <- paste(emf,"+nodematch(","\'",blnetevn$c4[m],"\'",")",sep="")                    
                } else {
                  emf <- paste(emf,"+absdiff(","\'",blnetevn$c4[m],"\'",")",sep="")                    
                }	
              }				
              em <- ergm(as.formula(emf))
              blnetevn$c9 <- blnetevn$c4[which(summary(em)$coefs[3:(length(blnetevn$c4)+2),4]<blnetevn$c6)]
              assign("c9",blnetevn$c9,envir=blnetevn) 
              k <- data.frame(blnetevn$c9)
              colnames(k) <- "Dimensions"  
              thdlevel <- gwindow("Salient Dimensions",width = 300, height = 400)
              tg <- ggroup(horizontal = FALSE, cont = thdlevel)
              dims <- gtable(k, expand = TRUE, cont = tg) 
            }
          })
        } else {
          seclevel <- gwindow("Salient Dimension Options", width=600, height=600, parent = window)
          secg <- ggroup(cont = seclevel, use.scrollwindow=T, horizontal = FALSE)
          tbl <- glayout(cont = secg)
          j <- 1
          tbl[j,1] <- "Please identify categorical variables:"
          tbl[j,2] <- gcheckboxgroup(blnetevn$c4, handler = function(h,...){
            assign("c7",svalue(h$obj),envir=blnetevn)
          })
          j <- j + 1        
          rm(j)
          button <- gbutton("Continue", expand = FALSE, cont = secg, handler = function(h, ...) {
            dispose(seclevel)
            if ('el' %in% ls(envir=blnetevn)) {
              adj1 <- blnetevn$adj
              n <- network(as.matrix(adj1))
              emf <- "n~edges+mutual"
              for (m in 1:length(blnetevn$c4)) {
                k1 <- cov1[,which(names(cov1)==blnetevn$c4[m])]
                k1[which(is.na(k1)==TRUE)] <- round(mean(k1,na.rm=TRUE))
                n %v% blnetevn$c4[m] <- k1
                if (blnetevn$c4[m] %in% blnetevn$c7) {
                  emf <- paste(emf,"+nodematch(","\'",blnetevn$c4[m],"\'",")",sep="")                    
                } else {
                  emf <- paste(emf,"+absdiff(","\'",blnetevn$c4[m],"\'",")",sep="")                    
                }	
              }				
              em <- ergm(as.formula(emf))
              blnetevn$c9 <- blnetevn$c4[which(summary(em)$coefs[3:(length(blnetevn$c4)+2),4]<blnetevn$c6)]
              assign("c9",blnetevn$c9,envir=blnetevn) 
              k <- data.frame(blnetevn$c9)
              colnames(k) <- "Dimensions"  
              thdlevel <- gwindow("Salient Dimensions",width = 300, height = 400)
              tg <- ggroup(horizontal = FALSE, cont = thdlevel)
              dims <- gtable(k, expand = TRUE, cont = tg) 
            }
          })
	  }    
      }  
    }
  }})}
}

