% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add-stream-name-to-blk.R
\name{fwa_add_stream_names_to_blk}
\alias{fwa_add_stream_names_to_blk}
\title{Add Blue Line Key(s) to Stream Name}
\usage{
fwa_add_stream_names_to_blk(x, stream_name = fwatlasbc::fwa_stream_name)
}
\arguments{
\item{x}{A data frame with whole numeric column blk.}

\item{stream_name}{A data frame with whole numeric column blk and character column stream_name.}
}
\value{
A tibble with the columns of x plus an integer column blk.
}
\description{
Adds blue line keys (blk) to stream names.
There may be more than one stream with the same name.
}
\examples{
fwa_add_stream_names_to_blk(data.frame(blk = 360886335L))
}
