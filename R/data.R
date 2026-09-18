
globalVariables(c("N", "Time"))

#' Read HOBO CSVs from a directory
#' 
#' Read every CSV in a directory using [read_hobo()] and join them into a single data frame.
#' 
#' @param dir Path to the directory.
#' @param tz (optional) Time zone to use for the time column.
#'   If this argument is missing, it will be inferred from the HOBO column heading. 
#' 
#' @return A single data frame containing data from the whole directory.
#' 
#' @examples
#' cows_path <- hobo_example("cows")
#' cows_raw <- read_hobo_dir(cows_path, tz="America/New_York")
#' 
#' @seealso read_hobo
#' 
#' @export
read_hobo_dir <- function(dir, tz) {
  
  file_list <- list.files(dir, full.names=TRUE, pattern="*.csv$")
  if(length(file_list) < 1) stop("No CSVs found in directory")
  
  lapply(file_list, read_hobo, tz=tz) |>
    dplyr::bind_rows()
}

#' Read a single HOBO CSV
#' 
#' Read an exported HOBO CSV into R as a data frame.
#' See `vignette("tutorial")` for the expected export format.
#' Animal ID is inferred from the "Plot Title" header.
#' 
#' @param file Path to the HOBO CSV file.
#' @param tz (optional) Time zone to use for the time column.
#'   If this argument is missing, it will be inferred from the HOBO column heading. 
#' 
#' @return A data frame:
#' 
#' \describe{
#' \item{`Animal`}{Animal ID.}
#' \item{`Time`}{Time and date.}
#' \item{`X`, `Y`, `Z`}{Acceleration (g) for axes present in the data.}
#' }
#' 
#' @examples
#' file_cow01 <- file.path(hobo_example("cows"), "cow01.csv")
#' cow01 <- read_hobo(file_cow01)
#' 
#' @export
read_hobo <- function(file, tz) {
  
  details <- check_hobo_header(file)
  header <- c("N", "Time", details$axes)
  types <- paste0("ic", ifelse(details$is_axis, "d", "c") |> paste(collapse=""))
  
  if(missing(tz)) tz <- details$tz

  data <- readr::read_csv(file, skip=2, col_names=header, col_types=types, progress=FALSE)
  
  data |>
    dplyr::select(-N) |>
    dplyr::mutate(Animal = details$id, .before=1) |>
    dplyr::mutate(Time = lubridate::mdy_hms(Time, tz=tz)) |>
    structure(problems = attr(data, "problems"))
}

check_hobo_header <- function(file) {
  
  header <- readLines(file, 2)
  
  # remove any BOM markers
  header <- gsub("^\\x{feff}", "", header)
  
  # first header line gives animal/device id
  re_id <- '"Plot Title: ([[:alnum:]]*)"'
  emsg_id <- 'First header line not recognised. Expected "Plot Title: <animal-id>"'
  if(!grepl(re_id, header[1])) stop(emsg_id)
  id <- sub(re_id, '\\1', header[1])
  
  # second line is column headings
  h2 <- utils::read.csv(text=header[2], header=FALSE)
  if(nrow(h2) != 1) stop("Unable to read column headings")
  
  # first column is just "#"
  if(h2$V1 != "#") warning('Expected "#" for first column heading')
  
  # second column eg "Date Time, GMT-04:00"
  re_tz <- "Date Time, GMT([+-])([[:digit:]]{2}):([[:digit:]]{2})"
  emsg_tz <- 'Expected "Date Time" in second column heading'
  if(!grepl(re_tz, h2$V2)) stop(emsg_tz)
  tz_sign <- sub(re_tz, "\\1", h2$V2)
  tz_hour <- sub(re_tz, "\\2", h2$V2) |> as.numeric()
  tz_min <- sub(re_tz, "\\3", h2$V2)
  if(tz_min != "00") stop("Fractional offsets are not supported, please specify the timezone manually")
  
  # note +/- flip because (who knows???)
  tz <- sprintf("Etc/GMT%s%i", c(`+`="-", `-`="+")[tz_sign], tz_hour)
  
  # remaining columns should be accelerometers
  axes <- h2 |> unlist() |> unname() |> tail(-2)
  re_axes <- "([[:upper:]]) Accel, g \\(LGR S/N: [[:digit:]]+, SEN S/N: [[:digit:]]+\\)"
  is_axis <- grepl(re_axes, axes)
  if(!all(is_axis)) {
    warning("Ignoring columns not recognised as accelerometer readings")
  }
  axes[is_axis] <- sub(re_axes, "\\1", axes[is_axis])
  axes[!is_axis] <- sprintf("Unknown%i", seq_len(sum(!is_axis)))
  
  list(id=id, tz=tz, axes=axes, is_axis=is_axis) 
}

#' Get path to example files
#'
#' Get the local path to the example files bundled with robo.
#'
#' @param animal Which set of example files to get, either "cows" or "goats".
#' 
#' @return A path to the example files as a character string.
#' 
#' @examples
#' hobo_example("goats")
#' 
#' @export
hobo_example <- function(animal=c("cows", "goats")) {
  
  animal <- match.arg(animal)
  
  if(animal=="cows") return(system.file("extdata/cow", package="robo"))
  if(animal=="goats") return(system.file("extdata/goat", package="robo"))
  
  stop("Invalid example")
}
