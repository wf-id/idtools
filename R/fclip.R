#' Truncation of Values
#' @param x a vector of values
#' @param xmin a double representing the minimum bound
#' @param xmax a double representing the upper bound
#' @export
#' @author <https://github.com/rmcelreath/rethinking/blob/master/R/utilities.r>
#' @examples
#' x <- rnorm(100, 1,2)
#'
#' hist(x)
#'
#' hist(fclip(x, 0, 3))
#'
#'
fclip <- function( x , xmin=NULL , xmax=NULL ) {
  if ( !is.null(xmin) ) x <- ifelse( x < xmin , xmin , x )
  if ( !is.null(xmax) ) x <- ifelse( x > xmax , xmax , x )
  x
}
