#' Verbose Counter Completion in Loops
#'
#' @description In a for loop, print the (rough) percent of loops that are
#'      finished.
#'
#' @param counter_value number of times the for loop has gone through
#' @param counter_length length of the for loop
#'
#' @examples
#' for(i in 1:28){
#'   cat("\n\ti = ",i)
#'   counter_verbose(i,28)
#' }
#' @export
counter_verbose <- function(counter_value, counter_length){
  if(counter_length > 11){
    full_range <- 1:counter_length
    a <- cut(full_range, 10, labels = F)
    a <- full_range[!duplicated(a)]
    a <- c(a[-1], counter_length)
    if(counter_value %in% a){
      cat('\n',10*which(counter_value==a),'% complete')
    }
  } else {
    cat('\n',counter_value,'of',counter_length,'complete')
  }
}
