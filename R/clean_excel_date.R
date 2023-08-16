#' Clean up excel date field
#' @description Sometimes excel dates are strings, sometimes 5 digit codes, sometimes both
#' @param x a string to be formatted as a date
#' @param time default \code{FALSE}, whether to return a dateime instead of date
#'
#' @importFrom dplyr case_when
#' @importFrom janitor excel_numeric_to_date
#' @importFrom stringr str_detect
#' @importFrom lubridate mdy
#' @importFrom lubridate parse_date_time
#' @return a `Date` when \code{time=FALSE} or a `POSIXct` when \code{time=TRUE}
#' @export
#'
#' @examples
#'     clean_excel_date("32545")
#'     clean_excel_date("12/2/2021")
#'     clean_excel_date("6/28/2021")
#'
clean_excel_date <- function(x, time = FALSE){
  formats_try <- c('mdy','ymd', 'dmY', 'Ym')
  if(time) formats_try <- c(formats_try, paste(formats_try, 'HM'))

  a <- dplyr::case_when(
    nchar(x) == 5 & stringr::str_detect(x, "/", negate = TRUE) ~
        janitor::excel_numeric_to_date(as.numeric(x), include_time = time),
    TRUE ~ lubridate::parse_date_time(x, formats_try, quiet = TRUE)
  )
  if(time){
    return(a)
  } else {
    return(as.Date(a))
  }
}
