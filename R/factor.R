#
# Internal functions
#  so that rle works with factors
#  misc other factor stuff
#

rle_wrapper <- function(x) {
  
  if(is.factor(x)) rle_factor(x) else rle(x)
}

rle_factor <- function(x) {
  
  if(any(c(".na", ".sb") %in% levels(x))) stop("Please relabel your factor, .na and .sb are reserved levels.")
  
  z <- rle(as.character(x))
  z$levels <- levels(x)
  
  return(z)
}

# z$values but restore factor structure
# note that this is not an rle inversion!
rle_unwrap <- function(z) {
  
  if(is.null(z$levels)) return(z$values)
  
  factor(z$values, z$levels)
}

# this, however, is an rle inversion :)
rle_inverse <- function(z) {
  
  x <- inverse.rle(z)
  
  if(is.null(z$levels)) x else factor(x, z$levels)
}

# return a single integer encoding unique values in x
binary_mask <- function(x, levels) {
  
  sum(2L^(which(levels %in% x) - 1))
}

# return an integer vector of length(x)
binary_code <- function(x) {

  2L^(as.numeric(as.factor(x)) - 1)
}

`%&%` <- function(A, B) {test <- bitwAnd(A, B); !is.na(test) & (test > 0L)}

# no longer needed?
char_mask <- function(x) {
  
  z <- levels_wrap(x)
  
  if(any(grepl(x=z, pattern="\\|"))) stop("The pipe character | is invalid for behaviour names.")
  
  intersect(z, x) |> paste(collapse="|")
}

levels_wrap <- function(x) {
  
  if(is.factor(x)) return(levels(x))  
  
  unique(x)
}

