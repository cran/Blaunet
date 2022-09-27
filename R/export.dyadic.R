export.dyadic <-
function(blauObj){
  if (is.null(blauObj$dyadic)){
    message("Nothing to export.")
  }
  else{
    return(blauObj$dyadic)
  }
}
