
brattr <- function(h,...) {
  if ("cov" %in% ls(envir=blnetevn)) {
    nw <- gwindow("Attribute File",width = 800, height = 600)
    group <- ggroup(horizontal = FALSE, cont = nw)
    vars <- gdf(blnetevn$cov, expand = TRUE,  fill=TRUE, cont = group)
  } else gmessage("Sorry! Attribute file is not loaded.", parent = window)
}

bradj <- function(h,...) {
  if ("adj" %in% ls(envir=blnetevn)) {
    nw <- gwindow("Adjacency Matrix",width = 800, height = 600)
    group <- ggroup(horizontal = FALSE, cont = nw)
    ego <- rownames(blnetevn$adj)
    adj1 <- cbind(ego,blnetevn$adj)
    vars <- gdf(adj1, expand = TRUE,  fill=TRUE, cont = group)
  } else gmessage("Sorry! Network file is not loaded.", parent = window)
}

brel <- function(h,...) {
  if ("el" %in% ls(envir=blnetevn)) {
    nw <- gwindow("Edge List",width = 800, height = 600)
    group <- ggroup(horizontal = FALSE, cont = nw)
    vars <- gdf(blnetevn$el, expand = TRUE,  fill=TRUE, cont = group)
  } else gmessage("Sorry! Network file is not loaded.", parent = window)
}


