add_point_to_stream_measure <- function(x, streams) {
  stream <- streams[streams$blk == x$blk[1],]
  sample <- x$stream_measure / stream$length
  x$geometry <- stream |>
    sf::st_line_sample(sample = sample) |>
    sf::st_cast("POINT")
  x
}

#' Snap Stream Measure to Point
#'
#' Assigns closest stream measure in m to each spatial point.
#' If the blue line key (blk) is missing then it is also assigned
#' together with the distance to the stream (distance_to_stream) in m.
#'
#' @param x An sf object of spatial points with optional integer column blk.
#' @param streams An sf object of spatial linestrings with blk column.
#' @param ... Additional columns to group by when assigning.
#' @return An updated version of x with integer columns blk and stream_measure and numeric column distance_to_stream.
#' @seealso [fwa_snap_rm_to_point()]
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
  chk_not_subset(colnames(x), c("..fwa_id"))
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
