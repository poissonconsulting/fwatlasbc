% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add-cut-to-rms.R
\name{fwa_add_cut_to_rms}
\alias{fwa_add_cut_to_rms}
\title{Add Cut to River Meter}
\usage{
fwa_add_cut_to_rms(x, y, cut = "cut")
}
\arguments{
\item{x}{A data frame with integer columns blk and rm.}

\item{y}{A data frame with integer columns blk, rm_start, rm_end and column specified in cut.}

\item{cut}{A string of the name of the column in y with the values.}
}
\value{
A tibble of x with cut column from y.
}
\description{
Adds value(s) in cut column in y to to column of same name in x based on blk and rm_start and rm_end in y.
}
