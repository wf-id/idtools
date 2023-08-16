#' Percent Change Calculations
#' @description Compute the percent change between two numbers or determine parameters to obtain a certain percent change.
#'
#' Like `stats.power.xxx.test()` functions, exactly one argument should be null and that parameter is calculated
#' @param start starting value
#' @param end ending value
#' @param p percent change
#' @param returnall whether or not to return all arguments. defaults to \code{FALSE} and only returns the NULL parameter
#'
#' @export
#'
#' @examples
#' perc_change(0.75, .375) #50% decrease
#' perc_change(0.75, end = NULL, p = -.5, returnall = TRUE) #50% decrease
#'
perc_change <- function(start, end, p = NULL, returnall = FALSE){

  if (sum(vapply(list(start, end, p), is.null,
                 NA)) != 1) stop("exactly one of 'start', 'end', 'pchange' must be NULL")


  if(is.null(p)){

    stopifnot(is.numeric(start), is.numeric(end))

    out <-  (end - start) / start
    p <- out

  }

  stopifnot(is.numeric(p))

  if(is.null(end)){

    stopifnot(is.numeric(start))

    out <- start*(p+1)
    end <- out

  }

  if(is.null(start)) {
    stopifnot(is.numeric(end))

    out <- end - (p+1)
    start <- out
  }

  if(isFALSE(returnall)){

    return(out)

  } else{

    return(list(start = start, end = end, p = p))
  }


}
