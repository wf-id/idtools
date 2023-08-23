#' Clean up excel date field
#' @description Sometimes excel dates are strings, sometimes 5 digit codes, sometimes both
#' @param x a string to be formatted as a date
#' @param time default \code{FALSE}, whether to return a dateime instead of date
#' @param verbose default \code{FALSE} hides any warning messages from \code{janitor::excel_numeric_to_date()} and/or \code{lubridate::parse_date_time()}
#'
#' @return a `Date` when \code{time=FALSE} or a `POSIXct` when \code{time=TRUE}
#' @export
#'
#' @examples
#'     clean_excel_date("32545")
#'     clean_excel_date("12/2/2021")
#'     clean_excel_date("6/28/2021")
#'
clean_excel_date <- function(x, time = FALSE, verbose = FALSE){
  formats_try <- c('mdy','ymd', 'dmY', 'Ym')
  if(time) formats_try <- c(formats_try, paste(formats_try, 'HM'))

  run <- function(x, v = verbose){

    if(v) return(x)

    return(suppressWarnings(x))
  }

  a <- run(dplyr::case_when(
    nchar(x) == 5 & stringr::str_detect(x, "/", negate = TRUE) ~
        janitor::excel_numeric_to_date(as.numeric(x), include_time = time),
    TRUE ~ lubridate::parse_date_time(x, formats_try, quiet = !verbose)
  ))
  if(time){
    return(a)
  } else {
    return(as.Date(a))
  }
}
