
#' @importFrom utils head
#' @importFrom utils tail
#' @importFrom stats na.omit
#' @importFrom rlang .data
#' @importFrom rlang .env
#' @importFrom methods as
NULL

#' @importFrom readr problems
#' @export
readr::problems

#' Example datasets
#'
#' @description
#'
#' Examples of (cleaned) HOBO accelerometer data. One data set, `cows`, is from dairy
#' cows, with horizontally mounted devices, and with y-axis and z-axis recorded.
#' The second, `goats`, is from dairy goats, with vertically mounted devices, and
#' all three axes recorded.
#' 
#' Both data sets have been de-identified. They are suitable for learning to use
#' the robo package, but should be used with caution since the de-identification
#' may have made some aspects of the data unrealistic.
#' 
#' @format
#' \describe{
#' \item{`Animal`}{Animal ID.}
#' \item{`Time`}{Time and date.}
#' \item{`X`}{x-axis acceleration (g) (only in `goats`).}
#' \item{`Y`}{y-axis acceleration (g).}
#' \item{`Z`}{z-axis acceleration (g).}
#' }
#' 
#' @name datasets
#' @aliases cows goats
NULL

