#' Get Duplicates from Vector or Data Frame
#'
#' @description Find and return all duplicates (not just the first indices like
#'      \code{\link[base]{duplicated}} does)
#'
#' @param x A vector to get duplicate data from. Alternatively, the **quoted**
#'      column name to return duplicates from when \code{data} is provided.
#' @param data A data frame to get duplicate data from. Defaults \code{NULL}.
#'
#' @examples
#' # vector example
#' a <- c(1,2,3,4,4,5,6,7,7,8,9,10)
#' get_dups(x = a)
#'
#' #data.frame example
#' b <- data.frame(id = a, counter = seq_along(a))
#' get_dups(x = 'id', data = b)
#' @export
get_dups <- function(x, data=NULL){

  if(is.null(data)) {
    if(!is.vector(x)) stop("x should be a vector unless data is supplied.")
    return(x[x %in% x[duplicated(x)]])
  } else {
    if(!is.data.frame(data)) stop('data should be a data frame')
    if(!x %in% names(data)) stop('x must be a column of data')
    return(data[data[[x]] %in% data[[x]][duplicated(data[[x]])], ])
  }
}
