% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add-blks-to-stream-name.R
\name{fwa_add_blks_to_stream_name}
\alias{fwa_add_blks_to_stream_name}
\title{Add Blue Line Key(s) to Stream Name}
\usage{
fwa_add_blks_to_stream_name(x, stream_name = fwatlasbc::fwa_stream_name)
}
\arguments{
\item{x}{A data frame with character column stream_name.}

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
fwa_add_blks_to_stream_name(data.frame(stream_name = "Sangan River"))
}
