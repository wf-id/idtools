#' Make a Percentage
#'
#' @param x the value to be converted
#' @param digitz the number of digits desired with default of 0
#'     (e.g. .95111 = 95 for a value of 0 digits)
#' @param type do you want it to be a character or number
#' @export

make_perc <- function(x, digitz = 0, type = "numeric"){

  if(!is.numeric(digitz)){
    stop("Please provide a valid option for digitz")
  }

  if(!is.numeric(x)){
    warning("Non-numeric values found for x, coercing to doubles")

    x <- suppress_all(is.numeric(x))
  }

  type <- match.arg(type,
                    choices = c("numeric", "character"), several.ok = FALSE)

  perc <- round(x*100, digits = digitz)

  if(type != "numeric"){
    perc <- paste0(perc, "%")
  }

  perc

}
