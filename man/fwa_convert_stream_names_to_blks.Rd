% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert-stream-names-to-blks.R
\name{fwa_convert_stream_names_to_blks}
\alias{fwa_convert_stream_names_to_blks}
\title{Converts Stream Names to Blue Line Keys}
\usage{
fwa_convert_stream_names_to_blks(names)
}
\arguments{
\item{names}{A character vector of stream names.}
}
\value{
An positive integer vector of blue line keys.
}
\description{
Each stream name is converted to a blue line key
by calculating it's integer hash.
}
\details{
This function is only expected to be used when a blue line
key does not already exist.
}
\examples{
fwa_convert_stream_names_to_blks(c("a stream name", "a stream name2"))
}
