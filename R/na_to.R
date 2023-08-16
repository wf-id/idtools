#' Replace NAs
#' @description Allows for replacement of \code{NA} values in a vector.
#'
#' Unlike \code{dplyr::replace_na()}, the replacement can be a vector (i.e. works within \code{dplyr::mutate} where \code{NA} values can be replaced with values in another column).
#'
#' @param x a vector
#' @param to value of length 1 to change \code{NA} values to, or a vector of same length as x where any \code{NA} values in x will be replaced with the corresponding values in the 'to' vector. Default \code{0}.
#'
#' @importFrom dplyr if_else
#' @export
#' @seealso [nan_to()], [inf_to()], [nonfinite_to()]
na_to <- function(x, to = 0){
  dplyr::if_else(is.na(x), to, x)
}



#' Replace NaNs
#' @description Allows for replacement of \code{NaN} values in a vector.
#'
#'
#' @param x a vector
#' @param to value of length 1 to change \code{NaN} values to, or a vector of same length as x where any \code{Nan} values in x will be replaced with the corresponding values in the 'to' vector. Default \code{0}.
#'
#' @importFrom dplyr if_else
#' @export
#' @seealso [na_to()], [inf_to()], [nonfinite_to()]
nan_to <- function(x, to = 0){
  dplyr::if_else(is.nan(x), to, x)
}



#' Replace infinite values
#' @description Allows for replacement of \code{Inf} and \code{-Inf} values in a vector.
#'
#'
#' @param x a vector
#' @param to value of length 1 to change \code{Inf} and \code{-Inf} values to, or a vector of same length as x where any \code{Inf}/\code{-Inf} values in x will be replaced with the corresponding values in the 'to' vector. Default \code{0}.
#'
#' @importFrom dplyr if_else
#' @export
#' @seealso [na_to()], [nan_to()], [nonfinite_to()]
inf_to <- function(x, to = 0){
  dplyr::if_else(is.infinite(x), to, x)
}



#' Replace all non-finite values
#' @description Allows for replacement of non-finite values in a vector.
#' This is a shortcut for replacing \code{NA}, \code{NaN}, \code{Inf}, and \code{-Inf} values with some other value.
#'
#' @param x a vector
#' @param to value of length 1 to change non-finite values to, or a vector of same length as x where any non-finite values in x will be replaced with the corresponding values in the 'to' vector. Default \code{0}.
#'
#' @importFrom dplyr if_else
#' @export
#' @seealso [na_to()], [nan_to()], [inf_to()]
nonfinite_to <- function(x, to = 0){
  dplyr::if_else(is.finite(x),x,to)
}
