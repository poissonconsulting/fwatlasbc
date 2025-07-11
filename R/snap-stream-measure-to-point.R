nearest_stream <- function(x, streams) {
  index <- sf::st_nearest_feature(x, streams)
  streams <- streams[index, ]
  x$..fwa_blk <- streams$blk

  point <- sf::st_nearest_points(x, streams, pairwise = TRUE) |>
    sf::st_line_sample(sample = 1) |>
    sf::st_cast("POINT") |>
    sf::st_buffer(1e-07)

  splits <- list()
  for (i in seq_len(nrow(x))) {
    splits[i] <- lwgeom::st_split(streams[i, ], point[i]) |> sf::st_geometry()
  }

  length <- splits |>
    purrr::map(1L) |>
    purrr::map(sf::st_length) |>
    purrr::map_dbl(as.numeric) |>
    unlist() |>
    round(6)

  x$stream_measure <- length
  x$distance_to_stream <- sf::st_distance(x, streams, by_element = TRUE)
  x$distance_to_stream <- as.numeric(x$distance_to_stream)

  x
}

adjust_streams <- function(streams, x) {
  if (ncol(streams) == 2) {
    return(streams)
  }

  cols <- streams |>
    colnames() |>
    setdiff(c("blk", "geometry"))

  cols <- x |>
    as_tibble() |>
    dplyr::select(tidyselect::vars_select_helpers$all_of(cols)) |>
    dplyr::slice(1L) |>
    dplyr::select(tidyselect::vars_select_helpers$where(function(x) !is.na(x[1]))) |>
    colnames()

  if (!length(cols)) {
    return(streams)
  }
  streams |>
    dplyr::semi_join(as_tibble(x), by = cols)
}

snap_stream_measure_to_point <- function(x, streams) {
  if (!is.na(x$blk[1])) {
    streams <- streams[streams$blk == x$blk[1], ]
  }
  streams <- adjust_streams(streams, x)

  if (!nrow(streams)) {
    x$stream_measure <- NA_real_
    x$distance_to_stream <- NA_real_
    return(x)
  }

  nearest_stream(x, streams)
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
#' fwa_snap_stream_measure_to_point(points, streams)
#' }
fwa_snap_stream_measure_to_point <- function(x, streams, ...) {
  chk::chk_s3_class(x, "sf")
  chk::chk_s3_class(streams, "sf")

  if (!has_name(x, "blk")) x$blk <- NA_integer_

  chk_whole_numeric(x$blk)
  chk_gt(x$blk)
  chk_not_subset(colnames(x), c("..fwa_id", "..fwa_blk"))

  check_names(streams, "blk")
  chk_whole_numeric(streams$blk)
  chk_not_any_na(streams$blk)
  chk_gt(streams$blk)
  check_key(streams, "blk")

  if (!nrow(x)) {
    x$stream_measure <- double(0)
    x$distance_to_stream <- numeric(0)
    return(x)
  }
  if (!nrow(streams)) {
    x$stream_measure <- NA_real_
    x$distance_to_stream <- NA_real_
    return(x)
  }

  chk_s3_class(sf::st_geometry(x), "sfc_POINT")
  chk_s3_class(sf::st_geometry(streams), "sfc_LINESTRING")

  streams <- same_crs(streams, x)

  streams <- streams |>
    dplyr::select("blk", ...)

  x |>
    dplyr::mutate(..fwa_id = seq_len(dplyr::n())) |>
    group_split_sf(.data$blk, ...) |>
    lapply(snap_stream_measure_to_point, streams = streams) |>
    dplyr::bind_rows() |>
    dplyr::arrange(.data$..fwa_id) |>
    dplyr::mutate(
      ..fwa_blk = as.integer(.data$..fwa_blk),
      blk = as.integer(.data$blk),
      blk = dplyr::if_else(is.na(.data$..fwa_blk), .data$blk, .data$..fwa_blk)
    ) |>
    dplyr::select(!c("..fwa_id", "..fwa_blk")) |>
    identity()
}
