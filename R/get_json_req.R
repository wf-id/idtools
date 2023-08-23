#' GET JSON Request
#' @description Quick method for accessing JSON data from an open API. Used frequently
#'      when interfacing with the national weather service API.
#'
#' @param url api url
#' @param verbose default \code{TRUE} will tell you whether or not the request was good.
#' @param useragent user agent passed to the \code{httr::GET} call
#'
#' @export
#'
#'
get_json_req <- function(url, verbose = T,
                         useragent = httr::user_agent(
                           paste('Wake Forest School of Medicine,',
                                 'IDResearch@wakeforest.edu'))){
  requireNamespace('httr')
  requireNamespace('jsonlite')
  req <- httr::GET(url, useragent)
  if(req$status_code==200){
    if(verbose)cat('Good request\n')
    json <- httr::content(req, as = "text", encoding = 'UTF-8')
    out <- jsonlite::fromJSON(json)
    return(list(req = out, status = req$status_code))
  } else {
    if(verbose)cat('Bad request; status ',req$status_code,'\n')
    return(list(req = req, status = req$status_code))
  }

}
