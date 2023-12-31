% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_json_req.R
\name{get_json_req}
\alias{get_json_req}
\title{GET JSON Request}
\usage{
get_json_req(
  url,
  verbose = T,
  useragent = httr::user_agent(paste("Wake Forest School of Medicine,",
    "IDResearch@wakeforest.edu"))
)
}
\arguments{
\item{url}{api url}

\item{verbose}{default \code{TRUE} will tell you whether or not the request was good.}

\item{useragent}{user agent passed to the \code{httr::GET} call}
}
\description{
Quick method for accessing JSON data from an open API. Used frequently
when interfacing with the national weather service API.
}
