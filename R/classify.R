
#' Classify behaviour from an accelerometer axis
#' 
#' @description
#' 
#' Use this function to convert a single accelerometer axis into a behavioural classification,
#' usually Standing, Lying.
#' This classifier uses a simple decision rule: it chooses one of two behaviours based on whether
#' the acceleration measured by the axis is below or above a user-supplied threshold.
#' 
#' @param data A data frame containing accelerometer data.
#' @param axis The name of a column in `data` to be used for this classification.
#' @param threshold The accelerometer threshold to use for classification.
#' @param name (optional) The name for the new column with the classified behaviour. Defaults to "Behaviour".
#' @param below (optional) The behaviour to assign when below the threshold. Defaults to "Standing".
#' @param above (optional) The behaviour to assign when above the threshold. Defaults to "Lying".
#' @param min_run (optional) The shortest consecutive sequence to keep. Defaults to 0 (no smoothing).
#' @param comparison (optional) One of `"<"` or `"<="`, determines whether values on the threshold are
#' assigned to `above` or `below`. Defaults to "<".
#'
#' @return a copy of `data` with a new column `data[[name]]` containing the accelerometer-derived behaviours.
#'
#' @examples
#' goats |> dplyr::group_by(Animal) |> classify_sl("X", -0.60)
#'
#' @export
classify_sl <- function(data, axis, threshold, name="Behaviour", below="Standing", above="Lying", min_run=0, comparison="<") {
  
  check_axis(data, axis)
  compare <- check_comparison(comparison)

  acceleration <- data[[axis]]    
  behaviour <- ifelse(compare(acceleration, threshold), below, above) |> factor(c(below, above))
  
  data[[name]] <- behaviour
  if(min_run > 1) data <- smooth_behaviour(data, name, name, min_run)
    
  return(data)
}

#' Classify behaviour from two accelerometer axes
#' 
#' @description
#' 
#' Use this function to convert two accelerometer axes into a behavioural classification,
#' usually Standing, Lying Left, Lying Right.
#' This classifier uses a two-part decision rule: it chooses the first behaviour (S vs L or R) based
#' on the first axis and threshold, or it uses the second axis and threshold
#' to choose between the second and third behaviour (L vs R). 
#'
#' @param data A data frame containing accelerometer data.
#' @param axis1 The name of a column in `data` to be used for the SL classification.
#' @param threshold1 The accelerometer threshold to use for the SL classification.
#' @param axis2 The name of a column in `data` to be used for the LR classification.
#' @param threshold2 The accelerometer threshold to use for the LR classification.
#' @param name (optional) The name for the new column with the classified behaviour. Defaults to "Behaviour".
#' @param below1 (optional) The name for the first behaviour. Defaults to "Standing".
#' @param below2 (optional) The name for the second behaviour. Defaults to "Lying Left"
#' @param above2 (optional) The name for the third behaviour. Defaults to "Lying Right"
#' @param min_run (optional) The shortest consecutive sequence to keep. Defaults to 0 (no smoothing).
#' @param comparison1 (optional) one of `"<"`, `"<="`, `">"`, or `">="`,
#' determines the comparison between the first axis and first threshold. Defaults to "<".
#' @param comparison2 (optional) one of `"<"` or `"<="`,
#' determines whether values on the threshold are assigned to `below2` or `above2`. Defaults to "<".
#'
#' @return a copy of `data` with a new column `data[[name]]` containing the accelerometer-derived behaviours.
#'
#' @examples
#' classify_slr(goats, "X", -0.6, "Z", -0.65)
#'
#' @export
classify_slr <- function(data, axis1, threshold1, axis2, threshold2,
  name="Behaviour", below1="Standing", below2="Lying Left", above2="Lying Right",
  min_run=0, comparison1="<", comparison2="<") {
  
  check_axis(data, axis1)
  check_axis(data, axis2)
  
  compare1 <- check_comparison(comparison1)
  compare2 <- check_comparison(comparison2)
  
  behaviour <- ifelse(
    compare1(data[[axis1]], threshold1),
    below1,
    ifelse(
      compare2(data[[axis2]], threshold2),
      below2,
      above2
    )
  ) |> factor(c(below1, below2, above2))
  
  data[[name]] <- behaviour
  if(min_run > 1) data <- smooth_behaviour(data, name, name, min_run)
  
  return(data)
}

# check that axis is a column in data
check_axis <- function(data, axis) {
  
  check_string_arg(axis)
  if(!(axis %in% names(data))) stop(sprintf("No %s axis in data.", axis))
  
  invisible(TRUE)
}

# check that the comparison argument is valid and return the appropriate function
# by default < and <= are allowed
# if gt is TRUE then > and >= are also allowed
check_comparison <- function(comparison, gt=FALSE) {
  
  check_string_arg(comparison)
  
  if(gt) {
    if(!(comparison %in% c("<", "<=", ">", ">="))) stop("comparison must be one of <, <=, >, >=.")
  } else {
    if(!(comparison %in% c("<", "<="))) stop("comparison must be one of <, <=.")
  }
  
  c(`<`=`<`, `<=`=`<-`, `>`=`>`, `>=`=`>=`)[[comparison]]
}
