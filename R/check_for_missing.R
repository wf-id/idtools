#' Check for Missing Values
#' @description Identifies whether missing values are present in a data.frame.
#'    Default behaviour will return an error if any missing values are present
#'    from select columns.
#' @param x a data.frame
#' @param colnames names in the data.frame that should not contain missing values.
#'    If \code{NULL} (default) all columns are considered
#' @param error default \code{TRUE}, prints a table of columns with the number of
#'    missing values to the console and returns an error if any missing values are
#'    found. If \code{FALSE} and missing values are found, a warning is displayed
#'    and the columns with number of missing are return as a data.frame. If no
#'    missing values are found, a success message is printed returns \code{NULL}
#'
#'
#' @export
check_for_missing <- function(x, colnames = NULL, error = TRUE){

  if(is.null(colnames)) colnames <- names(x)

  if(any(!colnames %in% names(x))) stop('not all column names are in the data frame')

  n_missing_df <- x |> dplyr::select(dplyr::all_of(colnames)) |>
    dplyr::summarise(dplyr::across(dplyr::everything(), \(x)sum(is.na(x))))

  n_missing <- n_missing_df |> rowSums()

  message <- 'No missing values detected'

  if(n_missing>0) {

    message <- 'Missing values found, see counts'

    n_missing_df <- n_missing_df |>
      tidyr::pivot_longer(dplyr::everything(),names_to = 'variable',
                          values_to = 'n_missing') |>
      dplyr::filter(n_missing>0)


    if(error) {
      n_missing_df |> print()
      stop(paste(message,'above'))
    } else{
        cli::cli_alert_warning(paste(message,'returned'))
        return(n_missing_df)
      }

  } else{
    cli::cli_alert_success(message)
    return(NULL)
  }

}
