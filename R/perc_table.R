#' Express Table Entries as % of Marginal Table
#' @description like \code{prop.table()} but formats output as a percentage, much
#'    easier on the eyes
#' @param x table
#' @param margin margins to split by (1 for rows, 2 for columns)
#' @param accuracy A number to round to. Use (e.g.) 0.01 to show 2 decimal places of precision.
#'  If NULL, the default, uses a heuristic that should ensure breaks have the minimum number of
#'  digits needed to show the difference between adjacent values.
#'  @importFrom scales percent
#'
#' @export
perc.table <- function(x, margin = NULL, accuracy = NULL){
  p <- prop.table(x, margin)
  d <- length(dim(p))
  t <- apply(p, MARGIN = 1:d,
             function(y)scales::percent(y, accuracy))
  print(t, quote = FALSE, right = TRUE)
}
