
globalVariables(c("dt", "count"))

# TODO

# need unit test for time zone preservation here
# both for bouts and bump

# does group and summarise work for datetimes?
# correct mode chosen when ambiguous?

# make sure factors are preserved


#' Convert observations to bouts
#'
#' @description
#' 
#' Take a data frame containing instantaneous behaviour observations and convert to a
#' data frame describing bouts. Bouts are assumed to start at the first instantaneous observation,
#' and end at the first instantaneous observation of a different behaviour. The final bout within a
#' group is assumed to end shortly after the final observation. This is determined by looking for
#' the most common time difference between observations.
#' 
#' @param data A data frame containing behaviour data.
#' @param behaviour (optional) The name of the behaviour column. Defaults to `"Behaviour"``.
#' @param time (optional) The name of the time column. Defaults to `"Time"`.
#' @param units (optional) Units for bout durations, one of
#' `"secs"`, `"mins"`, `"hours"`, `"days"`, or `"weeks"`. The default is `"mins"` (minutes).
#' @param gap (optional) Maximum time between observations, in seconds, before a gap is treated as missing data.
#' The default is based on the smallest time difference in the first 20 observations,
#' either 30s (smallest <= 5s) otherwise 300s.
#' 
#' @return
#' A table with one bout per row. The first column name is determined by the `behaviour` argument.
#' \describe{
#'   \item{&lt;behaviour&gt;:}{The behaviour during this bout.}
#'   \item{Start:}{Start time for this bout.}
#'   \item{End:}{End time for this bout.}
#'   \item{Length:}{Duration of this bout.}
#' }
#'
#' Existing grouping variables are preserved. The name of the behaviour
#' column is stored as an attribute \code{"behaviour"} of the result.
#'  
#' @examples
#' 
#' cows |> dplyr::group_by(Animal) |> classify_sl("Y", -0.65) |> group_daily() |> bouts()
#'
#' @export
bouts <- function(data, behaviour="Behaviour", time="Time", units="mins", gap) {

  # checks to make sure columns are valid  
  if(!(behaviour %in% names(data))) {
    
    if(missing(behaviour)) stop("Behaviour not specified.")
    stop("Invalid behaviour column.")
  }
  if(!(time %in% names(data))) stop("Invalid time column.")
  if(!inherits(data[[time]], "POSIXct")) stop("Time column does not contain times.")    
  if(missing(gap)) gap <- autogap(data[[time]])
  
  rval <- dplyr::group_modify(data, bouts_split, behaviour, time, units, gap)
  
  attr(rval, "behaviour") <- behaviour

  return(rval)
}

bouts_split <- function(data, .y, behaviour, time, units, gap) {
  
  # nb diff for POSIXct doesn't take units prior to R 4.4.0
  
  tm <- data[[time]]
  
  .s <- c(tm[1], tm) |>
    as.numeric() |> diff() |> # differences in seconds
    (\(x) x > gap)() |>
    cumsum()
  
  data |>
    dplyr::group_by(.s=.s, .add=TRUE) |>
    dplyr::group_modify(bouts_one, behaviour, time, units) |>
    dplyr::ungroup(.s) |> dplyr::select(-.s)
}
  
bouts_one <- function(data, .y, behaviour, time, units) {

  bvr <- data[[behaviour]]
  tm <- bump(data[[time]])
  
  bout_rle <- rle_wrapper(bvr)
  
  # first bout starts at obs 1
  # final bout ends at n+1
  z <- 1 + c(0, cumsum(bout_rle$lengths))

  rval <- dplyr::tibble(
    Behaviour = rle_unwrap(bout_rle),
    Start = tm[head(z, -1)],
    End = tm[tail(z, -1)],
    Length = difftime(End, Start, units=units)
  )
  
  names(rval)[1] <- behaviour
  
  return(rval)
}

# Internal
# Take the modal difference between time[i] and time[i+1]
# Add a time[n+1] entry reflecting that modal time 
bump <- function(time) {
  
  # find the most common time interval
  # break ties: smallest interval
  b <- dplyr::tibble(dt=diff(time)) |>
    dplyr::group_by(dt, .add=TRUE) |>
    dplyr::summarise(count=dplyr::n(), .groups="drop") |>
    dplyr::arrange(count, dt) |>
    dplyr::pull(dt) |> head()
  
  c(time, tail(time, 1)+b)
}

# Internal
# guess an appropriate gap to exclude from bouts
# either 30s if it looks like 1 Hz data
# or 5min if it looks like Hobo data
autogap <- function(x) {
  
  test <- head(x, 20) |> as.numeric() |> diff() |> min()
  
  if(test <= 5) 30 else 300
}




