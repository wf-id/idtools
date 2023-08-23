#' Write CSV with Meta Information
#'
#' Writes a corresponding README file with some summary
#' information about the csv. Uses d`ata.table::fread` to
#' write the actual file. Only supports those outputs
#' available to `data.table::fread`
#'
#' @param x a data.frame to be written
#' @param filepath a string representing the filepath of the file
#' @param .max_n an integer indicating the maximum number of
#'     unique values to print
#' @examples
#'
#' tmp <- tempfile(fileext = ".csv")
#' out <- write_with_meta(warpbreaks, tmp)
#'
#' writeLines(readLines(out))
#' @export

write_with_meta <- function(x, filepath, .max_n = 10){

  stopifnot(is.data.frame(x))

  stopifnot(tools::file_ext(filepath) %in% c("csv", "csvy"))

  .max_n <- as.integer(.max_n)

  stopifnot(is.integer(.max_n))

  if(!requireNamespace("yaml", quietly = TRUE)){
    stop("'write_with_meta' relies on the package 'data.table' to write the file header; please add this to your library with install.packages('yaml') and try again.")
  }
  if(!requireNamespace("yaml", quietly = TRUE)){
    stop("'write_with_meta' relies on the package 'yaml' to write the file header; please add this to your library with install.packages('yaml') and try again.")
  }

  target_directory <- dirname(filepath)
  sans_ex <- tools::file_path_sans_ext(basename(filepath))
  target_readme <- sprintf("README-%s.Md",sans_ex)

  target_path<- file.path(target_directory, target_readme)

  if(file.exists(target_path)){
    unlink(target_path)
    cli::cli_alert_warning("{target_path} exists and will be overwritten")
  }



  extract_meta <- function(y, max_n = .max_n){
    list(
      class = class(y),
      value_information = if(any(class(y)%in% c("character"))) {
        y_unique = unlist(unique(y))
        list(if(length(y_unique)>max_n) c(y_unique[1:max_n], "...") else y_unique)
      } else if (any(class(y) %in% c("Date", "POSIXct", "POSIXt"))){
        list(min_date = format(min(y, na.rm = T),'%Y-%m-%d'),
             max_date = format(max(y, na.rm = T),'%Y-%m-%d'))
      } else {
        as.list(summary(y))
      }
    )}

  out <- lapply(x, extract_meta)
  msg<- sprintf("%s contains the following fields:\n", basename(filepath))
  to_write_md <- sprintf("# %s\n\n%s\n```\n%s\n```\n", sans_ex, msg, yaml::as.yaml(out))

  cli::cli_alert_info("Writing your reference file to {target_path}")

  cat(to_write_md, file = target_path)

  cli::cli_alert_info("Writing your file to {filepath}")

  data.table::fwrite(x, file = filepath)

  cli::cli_alert_success("{filepath} was sucessfully written")

  return(invisible(target_path))



}

# d <- data.frame(d = seq.Date(Sys.Date()-9,
#                              Sys.Date(),
#                              by = "day"),
#                 f = factor(rep(letters[1:2],5)),
#                 character = sample(letters, 10),
#                 num = rnorm(10),
#                 pos = as.POSIXct(seq.POSIXt(Sys.time()-9,Sys.time(), length.out = 10)))
#
# lapply(d, extract_meta)
# min(d$d)
