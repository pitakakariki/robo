
globalVariables(c("long", "lag_long", ":=", "Start", "End", "mask", "action", "test", ".s"))

#
# Two kinds of smooth are implemented here
#
#  1. discrete point-coded data
#  2. continuous interval-coded data
#
# automate this based on whether we get a bouts object or not
#

# TODO add to docs
# uses min_run now. 1 means no smoothing, so does 0

#' Apply smoothing to behaviour data
#' 
#' @description
#' 
#' Short bouts are often noise in the accelerometer data rather than genuine behaviour changes.
#' Smoothing out short runs, especially singletons, can improve the overall accuracy of behaviour classifications.
#' This function uses run-length encoding to replace short bouts with the previously classified behaviour.
#' This function acts on data frames --- to apply the same smoother to a vector use [smooth_points()].
#'
#' @param data A data frame containing classified behaviour data.
#' @param name_in (optional) The column name containing classified behaviours. Defaults to "Behaviour".
#' @param name_out (optional) The column name for smoothed behaviour. If this isn't supplied, the function
#' will overwrite the original column.
#' @param min_run The minimum number of consecutive observations to trust.
#'
#' @return The input data frame, with a column of smoothed behaviour data.
#'
#' @examples
#' cows |> dplyr::group_by(Animal) |> classify_sl("Y", -0.65) |> smooth_behaviour(name_out="Smoothed") 
#'
#' @export
smooth_behaviour <- function(data, name_in="Behaviour", name_out=name_in, min_run=2) {
  
  data |>
    dplyr::mutate(!!rlang::sym(name_out) := smooth_points(!!rlang::sym(name_in), min_run))
}

#' @rdname smooth_behaviour
#' @export
smooth_behavior <- smooth_behaviour

#' Apply smoothing to a vector
#' 
#' Apply the smoother from [smooth_behaviour()] to a vector of behaviours.
#'
#' @param x A vector of behaviours.
#' @param min_run The minimum number of consecutive observations to trust.
#'
#' @return A vector the same length as `x`, with smoothing applied.
#'
#' @examples
#' bvr <- c("a", "a", "a", "b", "a", "a", "a", "b", "b", "a")
#' smooth_points(bvr, min_run=2)
#' smooth_points(bvr, min_run=3)
#' 
#' @seealso smooth_behaviour
#'
#' @export
smooth_points <- function(x, min_run=2) {
  
  if(min_run < 2) return(x)
  
  #x <- smooth_prep(x)
  
  z <- rle_wrapper(x)
  n <- length(z$values)
  
  # find the first run which is long enough
  j0 <- which(z$lengths >= min_run)[1]
  if(is.na(j0)) {
    
    warning(sprintf("No sub-sequences with length >= %i.", min_run))
    
    x[] <- attr(x, "na")
    return(smooth_present(x, x))
  }
  
  # extend that first value back to the start
  v0 <- z$values[j0]
  z$values[1:j0] <- v0

  # scan along the runs
  # note that we have ensured all runs before 'j' are long enough
  j <- j0
  v <- v0
  while(j <= n) {
    
    # if this one is long enough, record value and move to the next one
    if(z$lengths[j] >= min_run) {
      
      v <- z$values[j] # carry last good value
      j <- j + 1L
      next
    }
    
    # if this one isn't long enough, replace its value with the last good v
    # since j-1 is long enough, so is the new merged run
    z$values[j] <- v
    j <- j + 1L
  }
  
  # note that rval will be character
  rval <- rle_inverse(z)
  
  # this should never happen
  if(min(rle_wrapper(rval)$lengths) < min_run) warning("Smoothing failed - some runs are still too short.")

  #smooth_present(rval, x)
  rval
}

#' Apply smoothing to bouts data
#' 
#' @description
#' 
#' Take a data frame where rows represent bouts, and apply a smoothing algorithm
#' to ensure all bouts have a minimum length.
#' 
#' * Long bouts are kept.
#' * Long series of short bouts are merged, with the behaviour decided by voting.
#' * Short series of short bouts, or isolated short bouts, are merged into their neighbours.
#'
#' Any consecutive bouts of the same behaviour are merged. If the total duration within a group is
#' less than the minimum bout length, no bouts are returned for that group.
#'
#' @param data A data frame where each row is a bout.
#'   The data frame should have `Start`, `End` and `Length` columns.
#' @param min_length Minimum bout length in seconds.
#' @param behaviour (optional) The name of the behaviour column. Defaults to `"Behaviour"`.
#'
#' @return A data frame with the same columns as `data` but with all bouts having
#'   duration at least `min_length` seconds.
#'
#' @examples
#' \donttest{
#' bouts <- goats |> dplyr::group_by(Animal) |> classify_sl("X", -0.60) |> group_daily() |> bouts()
#' smooth_bouts(bouts, 300)
#'}
#'
#' @export
smooth_bouts <- function(data, min_length=2, behaviour=attr(data, "behaviour")) {
  
  # start by checking that the necessary columns are there
  smooth_check(data, behaviour)
  min_length <- as.difftime(min_length, units="secs")
  
  rval <- dplyr::group_modify(data, smooth_bouts_split, min_length, behaviour)

  if(is.factor(data[[behaviour]])) rval[[behaviour]] <- factor(rval[[behaviour]], levels(data[[behaviour]]))
  attr(rval, "behaviour") <- attr(data, "behaviour")
  
  return(rval)
}

# Don't smooth over gaps in the data
smooth_bouts_split <- function(data, .y, min_length, behaviour) {
  
  data |>
    dplyr::mutate(test = Start != dplyr::lag(End)) |>
    dplyr::group_by(.s = cumsum(!is.na(test) & test), .add=TRUE) |>
    dplyr::group_modify(smooth_bouts_one, min_length, behaviour) |>
    dplyr::ungroup(.s) |> dplyr::select(-.s)
  
}

# Case I: long bout, keep
# Case II: long group of short bouts, merge based on plurality
#    IIa: ties decided by adjacent bouts
#    IIb: if still tied split between previous and next
# Case III: short group of short bouts
#    IIIa: merge with previous or next based on eligible plurality
#    IIIb: split between previous and next
# radix for stable sort (probably overly cautious)

smooth_bouts_one <- function(data, .y, min_length, behaviour) {
  
  bvr <- rlang::sym(behaviour)
  lev <- levels_wrap(data[[behaviour]])
  
  data |>
    
    # split into long-enough and runs of short bouts
    # new "run" if bout is "long" or is the first "short" in a series
    dplyr::mutate(
      long = (Length >= min_length),
      lag_long = dplyr::lag(long)
    ) |>
    dplyr::group_by(run = cumsum(is.na(lag_long) | lag_long | long), .add=TRUE) |>
    
    # summarise runs of short bouts
    dplyr::summarise(.groups="drop_last",
      
      {{behaviour}} := smooth_bouts_vote(!!bvr, Length),
      Start = head(Start, 1),
      End = tail(End, 1),
      Length = difftime(End, Start, units=units(Length)),
      mask = binary_mask(!!bvr, lev)
    ) |>
    
    # compare previous and next behaviours; calculate midpoint
    dplyr::mutate(
      
      left = binary_code(!!bvr) %&% dplyr::lag(mask),
      right = binary_code(!!bvr) %&% dplyr::lead(mask),
      mid = meantime(Start, End)
    ) |>
    
    # decide keep, merge, or split
    dplyr::mutate(action = dplyr::case_when(
    
      Length >= min_length ~ "keep",
      left & !right ~ "merge_left",
      !left & right ~ "merge_right",
      TRUE ~ "split"
    
    )) |>
    
    # apply keep, merge, or split
    dplyr::mutate(
      
      Start = dplyr::case_when(
        
        dplyr::lag(action) == "merge_right" ~ dplyr::lag(Start),
        dplyr::lag(action) == "split" ~ dplyr::lag(mid),
        TRUE ~ Start
      ),
      
      End = dplyr::case_when(
        
        dplyr::lead(action) == "merge_left" ~ dplyr::lead(End),
        dplyr::lead(action) ==  "split" ~ dplyr::lead(mid),
        TRUE ~ End
      )
    ) |>
    dplyr::filter(action == "keep") |>
    congeal(behaviour)
}

# nb radix sort (for stability) is probably over-cautious
smooth_bouts_vote <- function(bvr, Length) {
  
  dplyr::tibble(bvr, Length) |>
    dplyr::group_by(bvr, .add=TRUE) |>
    dplyr::summarise(Length = sum(Length), .groups="drop") |>
    dplyr::arrange(Length, bvr) |>
    dplyr::pull(bvr) |>
    tail(1)
  
}

# merge repeated behaviour bouts into single intervals
# internal only, doesn't need to restore factor levels?
congeal <- function(data, behaviour) {
  
  if(nrow(data) < 1) return(data)
  
  run_lengths <- rle_wrapper(data[[behaviour]])$lengths
  run <- rep(seq_along(run_lengths), times=run_lengths)
  
  data |> dplyr::group_by(run=.env$run, .add=TRUE) |>
    
    dplyr::summarise(.groups="drop_last",
                     
      {{behaviour}} := check_unique(!!rlang::sym(behaviour)),
      Start = head(Start, 1),
      End = tail(End, 1),
      Length = difftime(End, Start, units=units(Length))
                     
    ) |> dplyr::select(-run)
}

# calculate the time halfway between a and b
# sorry about the pun
meantime <- function(a, b) {
  
  # note to self: (a+b)  / 2 doesn't work here
  a + (b - a)/2
}

# TODO - anything to address below?
# "prep" and "present"
# deal with NAs and factor levels (and order)
# x <- prep(x) adjusts and adds attributes
# present(z, x) makes z look like x did at the start
# attributes: na, class, levels
# make character because rle doesn't work on factors
# we assume either character or factor, maybe allow numeric?
# rle works with int or num
# unclass instead of char?

smooth_prep <- function(x) {
  
  tx <- typeof(x)
  fx <- is.factor(x)
  lx <- levels(x)
  
  x <- as.character(x)
  na <- make.unique(c(unique(x), "NA")) |> tail(1)
  
  x <- ifelse(is.na(x), na, x)
  
  structure(x, na=na, type=tx, factor=fx, levels=lx)
}

smooth_present <- function(z, x) {
  
  z <- ifelse(z == attr(x, "na"), NA, z)
  
  z <- as(z, attr(x, "type"))
  
  if(attr(x, "factor")) z <- factor(z, attr(x, "levels"))
  
  return(z)
}


# Does the data frame have the columns needed to make it a bouts data frame?
smooth_check <- function(data, behaviour="Behaviour") {
  
  # checks to make sure columns are valid  
  if(!(behaviour %in% names(data))) stop("Invalid behaviour column.")
  if(!("Start" %in% names(data))) stop("Data frame does not contain a Start column.")
  if(!("End" %in% names(data))) stop("Data frame does not contain an End column.")
  if(!("Length" %in% names(data))) stop("Data frame does not contain a Length column.")

  invisible()
}

smooth_dt <- function(x, units) {
  
  as.difftime(x, units=units)
}


