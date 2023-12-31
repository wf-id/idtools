#' Shortcut for adding percentages to \code{dplyr::count} summaries
#'
#' This performs the equivalent of: \cr\cr and is copied from Bob Rudis
#'     at \url{https://github.com/hrbrmstr/hrbrmisc/blob/master/R/dplyr.r}
#' \code{count(x, col) |> mutate(pct=n/100, percent=scales::percent(pct))}
#'
#' @param x a \code{tbl} to count
#' @param wt (Optional) If omitted, will count the number of rows. If specified, will
#'           perform a "weighted" tally by summing the (non-missing) values of variable wt.
#' @param sort unlike \code{dplyr::count} this defaults to \code{TRUE}
#' @param \dots variables to group by
#' @examples
#' library(dplyr)
#' iris |>
#'   add_count_pct(Species)
#'
#' @export
add_count_pct <- function(x, ..., wt=NULL, sort=TRUE) {

  n <- pct <- NULL

  vars <- lazyeval::lazy_dots(...)
  wt <- substitute(wt)
  counted <- dplyr::count_(x, vars, wt=wt, sort=sort)
  dplyr::mutate(counted, pct=n/100, percent=scales::percent(pct))

}

