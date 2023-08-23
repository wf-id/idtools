
#' Set Working Directory to the Path of the Current Script
#'
#' @description Changes the current working directory to the Current Script in
#'      view in the source editor.
#'
#' @export
setwd_currentfile <- function(){
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
}
