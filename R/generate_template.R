#'Generate Template File
#' @description this function reads in most types of text/code-based files
#'    (e.g. `.sql`, `.txt`, `.R`) and collapses them into a single character string.
#'    supports \code{glue::glue} syntax within the file.
#' @param template_path the path to a file
#' @param glue_open default \code{\{}, the opening delimiter
#' @param glue_close default \code{\}}, the closing delimiter
#' @param envir environment to evaluate the \code{glue} expression. default
#'    is \code{parent.frame()}.
#'
#' @return a character string of file contents
#' @export
generate_template <- function(template_path,
                              glue_open = "{", glue_close = "}",
                              envir = parent.frame()){

  if(!file.exists(template_path)){
    stop("The file you have specified does not exist.")
  }

  check_encoding <- all(validUTF8(readLines(template_path, 2, warn = FALSE)))

  if(!check_encoding){
    f <- file(template_path, open="r", encoding="UTF-16LE")
    out <- glue::glue(paste(readLines(f, warn = FALSE),collapse = "\n"),
                      .open = glue_open, .close = glue_close,
                      .envir = envir)
    close(f)
  } else{
    out <- glue::glue(paste(readr::read_lines(template_path),collapse = "\n"),
                      .open = glue_open, .close = glue_close,
                      .envir = envir)
  }
  return(out)
}
