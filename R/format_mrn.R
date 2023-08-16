#' Format a Medical Record Number (MRN)
#' @description formats an MRN with leading zeros
#' @param x a string or number to be formatted as an MRN
#' @param length length of the string to return; will be padded with leading zeros when the string length is less than this value. defaults is \code{9}
#'
#' @importFrom stringr str_pad
#' @export
#'
#' @examples
#' format_mrn(123456)
#'
format_mrn <- function(x, length = 9){

  stringr::str_pad(x, length, side= "left", pad = "0")

}



#' Safely pad a vector to the same length
#' @description formats a value with the same leading or trailing value
#' @param x a vector of strings or numbers to be padded
#' @param length length of the string to return; default of \code{NULL} uses the longest character string in the vector
#' @param pad_with one character, default = \code{'0'}
#' @param pad_n if supplied, length is ignored. This is the number of 0s to pad on top of the longest character string. Example: if pad_n = 1 and the longest string contains 5 characters, all strings will be padded to a length of 5+1 = 6 characters.
#' @param side which side of the string to pad with zero. default = \code{left}
#'
#' @importFrom stringr str_pad
#' @export
#'
#' @examples
#' safe_pad(c(123,123456,1234567))
#' safe_pad(c(123,123456,1234567), 9)
#' safe_pad(c(123,123456,1234567), pad_n = 1)
#' safe_pad(c(123,123456,1234567), pad_with = ' ')
safe_pad <- function(x, length = NULL, pad_with = '0',
                     pad_n = NULL, side = 'left'){

  if(nchar(pad_with)>1) stop('pad_with must be 1 character only')
  maxcharlen <- max(nchar(x), na.rm = T)

  if(!is.null(pad_n)){
    if(!is.null(length)) warning('length is ignored since pad_n was provided')
    l <- maxcharlen+pad_n
  } else {
    l <- ifelse(is.null(length), maxcharlen, length)
    if(maxcharlen>l) stop('the longest character string is longer than the length requested')
  }
  stringr::str_pad(x, l, side= side, pad = pad_with)

}



