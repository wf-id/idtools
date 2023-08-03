#' Shorthand for glue::glue
#' @description see \link[glue]{glue} documentation
#' @param ... parameters passed to \code{glue()}
#'
#' @importFrom glue glue
#' @return a glue character string
#' @export
gg <- function(...){
  glue::glue(...)
}


#' Shorthand for dplyr::glimpse
#' @description see \link[dplyr]{glimpse} documentation
#' @param ... parameters passed to \code{glimpse()}
#'
#' @importFrom dplyr glimpse
#' @return x original x is (invisibly) returned, allowing \code{glimpse()} to be used within a data pipe line.
#' @export
g <- function(...){
  dplyr::glimpse(...)
}


#' Shorthand for here::here
#' @description see \link[here]{here} documentation
#' @param ... parameters passed to \code{here()}
#'
#' @importFrom here here
#' @return root path to project plus any additional path components provided
#' @export
hh <- function(...){
  here::here(...)
}
