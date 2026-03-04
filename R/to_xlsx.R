#' Write dataframe to excel
#' @description Writes a dataframe to an xlsx using \code{openxlsx::saveWorkbook}
#' @param df a dataframe
#' @param file A character string naming an xlsx file
#' @param sheet A name for the worksheet
#' @param overwrite If \code{TRUE}, overwrite any existing file.
#' @param table_style Any excel table style name or "none" (see "formatting" vignette in \code{openxlsx} package).
#' @param max_col_width maximum column width allowed, default \code{22}
#' @param ... other parameters passed to \code{openxlsx::writeDataTable}
#'
#' @export
#'
to_xlsx <- function(df, file, sheet = 'Sheet1',
                    overwrite = TRUE,
                    table_style = 'TableStyleMedium16',
                    max_col_width = 22, ...){


  requireNamespace('openxlsx')
  options("openxlsx.dateFormat" = "yyyy-mm-dd")
  options("openxlsx.datetimeFormat" = "yyyy-mm-dd  hh:mm AM/PM")

  wb <- openxlsx::createWorkbook()


  openxlsx::addWorksheet(wb, sheet)


  openxlsx::writeDataTable(wb, df,
                 sheet = sheet,
                 tableStyle = table_style, ...)

  width_vec <- apply(df, 2, function(x) max(nchar(as.character(x)) + 2,
                                             na.rm = TRUE))
  width_vec <- pmax(nchar(names(df)), width_vec)
  width_vec <- sapply(width_vec, \(x)pmin(x,max_col_width))
  openxlsx::setColWidths(wb, sheet, cols = 1:ncol(df), widths = width_vec)


  openxlsx::saveWorkbook(wb, file, overwrite = overwrite)


}
