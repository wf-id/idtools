% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/perc_table.R
\name{perc.table}
\alias{perc.table}
\title{Express Table Entries as \% of Marginal Table}
\usage{
perc.table(x, margin = NULL, accuracy = NULL)
}
\arguments{
\item{x}{table}

\item{margin}{margins to split by (1 for rows, 2 for columns)}

\item{accuracy}{A number to round to. Use (e.g.) 0.01 to show 2 decimal places of precision.
If NULL, the default, uses a heuristic that should ensure breaks have the minimum number of
digits needed to show the difference between adjacent values.}
}
\description{
like \code{prop.table()} but formats output as a percentage, much
easier on the eyes
}
