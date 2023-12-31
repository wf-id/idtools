% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/format_mrn.R
\name{format_mrn}
\alias{format_mrn}
\title{Format a Medical Record Number (MRN)}
\usage{
format_mrn(x, length = 9)
}
\arguments{
\item{x}{a string or number to be formatted as an MRN}

\item{length}{length of the string to return; will be padded with leading zeros when the string length is less than this value. defaults is \code{9}}
}
\description{
formats an MRN with leading zeros
}
\examples{
format_mrn(123456)

}
