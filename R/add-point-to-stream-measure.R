add_point_to_stream_measure <- function(x, streams) {
  stream <- streams[streams$blk == x$blk[1],]
  x$proportion <- x$stream_measure / stream$length
  x$geometry <- stream |>
    sf::st_line_sample(sample = x$proportion) |>
    sf::st_cast("POINT")
  x
}

#' Add Point to Stream Measure
#'
#' Adds point geometry for stream measure in m based on the blue line key (blk)
#' and the proportion of the stream measure along the stream.
#' If proportion is >= 1 then the geometry is the top of the stream.
#'
#' @param x An data frame with columns blk and stream_measure.
#' @param streams An sf object of spatial linestrings with blk column.
#' @param ... Additional columns to group by when assigning.
#' @return An updated version of x with numeric column proportion
#' giving the proportion of the stream measure along the stream
#' and a geometry column.
#' @seealso [fwa_snap_stream_measure_to_point()]
#' @export
#' @examples
#' \dontrun{
#' watershed <- fwa_add_watershed_to_blk(data.frame(blk = 356308001, rm = 1000))
#' network <- fwa_add_collection_to_polygon(watershed)
#' network$blk <- network$blue_line_key
#' streams <- fwa_join_stream_segments(network)
#' points <- fwa_add_rms_to_blk(data.frame(blk = 356308001))
#' points <- fwa_snap_stream_measure_to_point(points, streams)
#' points <- points[c("blk", "stream_measure")]
#' fwa_add_point_to_stream_measure(points, streams)
#' }
fwa_add_point_to_stream_measure <- function(x, streams, ...) {
  chk::chk_data(x)
  chk::chk_s3_class(streams, "sf")

  check_names(x, c("blk", "stream_measure"))
  chk_not_subset(colnames(x), "..fwa_id")
  chk_whole_numeric(x$blk)
  chk_gt(x$blk)
  chk_numeric(x$stream_measure)
  chk_gte(x$stream_measure)

  check_names(streams, "blk")
  chk_whole_numeric(streams$blk)
  chk_not_any_na(streams$blk)
  chk_gt(streams$blk)
  check_key(streams, "blk")

  chk_join(x, streams, by = "blk")

  chk_s3_class(sf::st_geometry(streams), "sfc_LINESTRING")

  check_dim(x, dim = nrow, values = TRUE)
  check_dim(streams, dim = nrow, values = TRUE)

  crs <- sf::st_crs(streams)

  x <- sf::st_drop_geometry(x)

  chk_not_subset(colnames(x), c("geometry"))

  streams <- streams |>
    dplyr::mutate(length = sf::st_length(streams),
                  length = units::set_units(.data$length, "m"),
                  length = as.numeric(length)) |>
    dplyr::select("blk", "length")

  x |>
    dplyr::mutate(..fwa_id = 1:dplyr::n()) |>
    group_split_sf(.data$blk, ...) |>
    lapply(add_point_to_stream_measure, streams = streams) |>
    dplyr::bind_rows() |>
    dplyr::arrange(.data$..fwa_id) |>
    dplyr::select(!c("..fwa_id")) |>
    identity()
}
