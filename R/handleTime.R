#' Add Time to R Console Handle
#'
#' @description Add the time to the console for when you want to casually time
#'      your code but down want to wrap lots of code with \code{\link[base]{system.time}}
#'      or intermittently check the \code{\link[base]{Sys.time}}.
#'
#' @param time if \code{TRUE} (default), will put the date/time in the R Console
#'      handle, \code{FALSE} will revert to the standard \code{> } handle.
#' @param format desired format of the timestamp. defaults to \code{\%m/\%d \%r}
#'
#' @examples
#' # turn it on:
#' handleTime()
#'
#' #turn if off:
#' handleTime(FALSE)
#'
#' @export
handleTime <- function(time = T, format = "%m/%d %r"){
  if(time==T){
    updatePrompt <- function(...) {options(prompt=paste(format(Sys.time(),format),"> ")); return(TRUE)}
  }
  else{
    updatePrompt <- function(...) {options(prompt=paste("> ")); return(TRUE)}
  }
  invisible(addTaskCallback(updatePrompt))
}
