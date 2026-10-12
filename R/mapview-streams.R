#' Map View Streams
#'
#' Plots stream linestrings with the start of each linestring as a blue
#' point and the end as a red point so that the direction of the streams
#' can be seen.
#'
#' @param x An sf data frame with a column blk and linestrings of streams
#' (or stream segments).
#' @inheritParams fwa_mapview
#' @param ... Additional arguments passed to `mapview::mapview()` when
#' plotting the linestrings.
#' @export
#' @seealso [`fwa_flip_streams()`]
#' @examples
#' \dontrun{
#' watershed <- fwa_add_watershed_to_blk(data.frame(blk = 356308001, rm = 1000))
#' network <- fwa_add_collection_to_polygon(watershed)
#' network <- select(network, blk = blue_line_key)
#' fwa_mapview_streams(network)
#' }
fwa_mapview_streams <- function(
  x,
  layer = NULL,
  zcol = NULL,
  legend = FALSE,
  ...
) {
  rlang::check_installed("mapview")

  chk_s3_class(x, "sf")
  chk_s3_class(sf::st_geometry(x), "sfc_LINESTRING")
  check_names(x, "blk")
  chk_null_or(layer, vld = vld_string)
  chk_null_or(zcol, vld = vld_string)
  chk_flag(legend)

  if (!is.null(layer)) {
    check_names(x, layer)
  }
  if (!is.null(zcol)) {
    check_names(x, zcol)
  }

  chk_whole_numeric(x$blk)
  chk_not_any_na(x$blk)
  chk_gt(x$blk)

  geometry <- sf::st_geometry(x)

  starts <- sf::st_set_geometry(
    x,
    sf::st_cast(sf::st_line_sample(geometry, sample = 0), "POINT")
  )
  ends <- sf::st_set_geometry(
    x,
    sf::st_cast(sf::st_line_sample(geometry, sample = 1), "POINT")
  )

  fwa_mapview(x, layer = layer, zcol = zcol, legend = legend, ...) +
    fwa_mapview(
      starts,
      col.regions = "blue",
      layer.name = "start",
      legend = FALSE
    ) +
    fwa_mapview(ends, col.regions = "red", layer.name = "end", legend = FALSE)
}
