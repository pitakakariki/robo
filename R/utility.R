
check_unique <- function(x, msg="Uniqueness check failed.", na.rm=FALSE) {
  
  rval <- unique(x)
  
  if(na.rm) rval <- rval[!is.na(rval)]
  
  if(length(rval) == 0) stop("No data available.")
  if(length(rval) > 1) stop(msg)
  
  return(rval)
}

diff_plus_mode <- function(x) {
  
  z <- diff(x)
  c(z, mode(z))
}

mode <- function(x) {
  
  tbl <- table(x)
  min(x[tbl==max(tbl)])
}

check_string_arg <- function(arg, argname=deparse(substitute(arg))) {
  
  if(!is.character(arg) || length(arg) != 1 || is.na(arg)) {
    
    stop(sprintf("%s should be a single character string.", argname))
  }
  
  invisible(TRUE)
}

