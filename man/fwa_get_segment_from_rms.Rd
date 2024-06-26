% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get-segment-from-rms.R
\name{fwa_get_segment_from_rms}
\alias{fwa_get_segment_from_rms}
\title{Get Segment from River Meter}
\usage{
fwa_get_segment_from_rms(x, segment = "segment")
}
\arguments{
\item{x}{A data frame with integer columns blk and rm.}

\item{segment}{A string of the name of the column in x, which must be a character or factor vector, that specifies which segment each rms belongs to.}
}
\value{
A sf tibble with integer columns blk, rm_start, rm_end, character/factor segment and a line geometry.
}
\description{
Gets sf tibble of section blk, rm_start, rm_end, segment and line geometry.
}
