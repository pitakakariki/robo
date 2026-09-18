
globalVariables(c("Timestep", "Duration", "Length", "Day", "Hour"))

#' Group by day
#' 
#' Group a data frame by day, after adding a column for the day.
#' If the data frame was already grouped, those previous groups are preserved.
#'
#' @param data A data frame to be grouped by day.
#' @param time (optional) The name of the time column. Defaults to `"Time"`.
#' @param name (optional) The name to give to the day column. Defaults to `"Day"`.
#'
#' @return A grouped data frame, with a new column for day, and grouped by day.
#'
#' @examples
#' cows |> dplyr::group_by(Animal) |> group_daily()
#'
#' @export
group_daily <- function(data, time="Time", name="Day") {
  
  check_string_arg(time)
  check_string_arg(name)
  
  data |>
    
    dplyr::mutate({{name}} := lubridate::as_date(!!rlang::sym(time))) |>
    
    dplyr::group_by(!!rlang::ensym(name), .add=TRUE)
  
}

#' Group by hour
#' 
#' Group a data frame by day and hour, after adding columns for day and hour.
#' If the data frame was already grouped, those previous groups are preserved.
#'
#' @param data A data frame to be grouped by hour.
#' @param time (optional) The name of the time column. Defaults to `"Time"`.
#' @param name_day (optional) The name to give to the day column. Defaults to `"Day"`.
#' @param name_hour (optional) The name to give to the hour column. Defaults to `"Hour"`.
#'
#' @return A grouped data frame, with a columns for day and hour, and grouped by day and hour.
#'
#' @examples
#' cows |> dplyr::group_by(Animal) |> group_hourly()
#'
#' @export
group_hourly <- function(data, time="Time", name_day="Day", name_hour="Hour") {
  
  data |>
    
    dplyr::mutate(
      {{name_day}} := lubridate::date(!!rlang::sym(time)),
      {{name_hour}} := lubridate::hour(!!rlang::sym(time)), .after=!!time) |>
    
    dplyr::group_by(!!rlang::sym(name_day), !!rlang::sym(name_hour), .add=TRUE)
}

#' Summarise data grouped by time
#' 
#' @description
#' 
#' Take a grouped data frame and summarise the duration of behaviours in each group.
#' 
#' Durations for a given instantaneous observation are assumed to span the time from the observation
#' itself to the time of the next observation, unless that duration exceeds `max_step` seconds.
#' The duration of the final observation in a group is assumed to be the modal time step within the group.
#'
#' @param data A data frame grouped by time period.
#' @param behaviour (optional) The name of the behaviour column. Defaults to `"Behaviour"`.
#' @param time (optional) The name of the time column. Defaults to `"Time"`.
#' @param units (optional) Units for bout durations, one of `"hours"`, `"mins"`, `"secs"`.
#'   The default is `"hours"`.
#' @param max_step (optional) The maximum duration, in seconds, to attribute to an observation.
#'   Defaults to 5.
#'
#' @return An ungrouped data frame with any grouping columns,
#'   a column for each behaviour duration, and a column for total duration.
#'
#' @examples
#' goats |> dplyr::group_by(Animal) |> classify_sl("X", -0.60) |> group_daily() |> summarise_time()
#'
#' @export
summarise_time <- function(data, behaviour="Behaviour", time="Time", units=c("hours", "mins", "secs"), max_step=5) {
  
  units <- match.arg(units)
  
  conversion <- switch(units, secs=1, mins=60, hours=3600, stop("Invalid units argument."))
  
  z0 <- data |>
    
    dplyr::filter(!is.na(!!rlang::sym(behaviour))) |>

    dplyr::mutate(Timestep = diff_plus_mode(as.numeric(!!rlang::sym(time)))) |>
  
    dplyr::mutate(Timestep = pmax(Timestep, max_step)) |>
  
    dplyr::group_by(!!rlang::sym(behaviour), .add=TRUE) |>
    
    dplyr::summarise(Duration=sum(Timestep)/conversion, .groups="drop_last")
    
  # totals for each behaviour
  z1 <- z0 |>
    
    tidyr::pivot_wider(names_from=!!behaviour, values_from=Duration, values_fill=0)
  
  # totals for all behaviours
  z2 <- z0 |>
    
    dplyr::summarise(Total=sum(Duration), .groups="drop")
  
  # z1 and z2 come through summarise
  # join will be by grouping variables
  suppressMessages(dplyr::left_join(z1, z2))
}

#' @rdname summarise_time
#' @export
summarize_time <- summarise_time

#' Summarise grouped bout data
#' 
#' Take a grouped data frame produced by [bouts()], and summarise the number and duration of bouts in each group.
#'
#' @param data A grouped data frame where each row is a bout.
#'   The data frame should have a `Length` column.
#' @param behaviour (optional) The name of the behaviour column. Defaults to `"Behaviour"``.
#'
#' @return An ungrouped data frame in "long" form, with a row for each combination
#'   of group and behaviour, with the number and duration of that kind of bout for that group.
#'
#' @examples
#' bouts <- goats |> dplyr::group_by(Animal) |> classify_sl("X", -0.60) |> group_daily() |> bouts()
#' summarise_bouts(bouts)
#'
#' @seealso bouts
#'
#' @export
summarise_bouts <- function(data, behaviour="Behaviour") {
  
  data |>
    dplyr::group_by(!!rlang::sym(behaviour), .add=TRUE) |>
    dplyr::summarise(Number=dplyr::n(), Duration=sum(Length), .groups="drop_last")
  
}

#' @rdname summarise_bouts
#' @export
summarize_bouts <- summarise_bouts
