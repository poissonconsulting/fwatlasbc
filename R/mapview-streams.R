halve_linestrings <- function(x) {
  geometry <- sf::st_geometry(x)
  x <- sf::st_drop_geometry(x)

  down <- x
  down$..fwa_half <- "down"
  down$geometry <- lwgeom::st_linesubstring(geometry, 0, 0.5)

  up <- x
  up$..fwa_half <- "up"
  up$geometry <- lwgeom::st_linesubstring(geometry, 0.5, 1)

  dplyr::bind_rows(down, up) |>
    sf::st_sf()
}

#' Map View Streams
#'
#' Plots stream linestrings with the downstream half (the start) of each
#' linestring in red and the upstream half (the end) in blue,
#' together with the start as a red point labelled "down" and the end as a
#' blue point labelled "up", so that the direction of the streams can be seen.
#' Streams oriented upstream (as produced by [`fwa_flip_streams()`])
#' are red at their mouth and blue at their source.
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
  legend = FALSE,
  ...
) {
  rlang::check_installed("mapview")

  chk_s3_class(x, "sf")
  chk_s3_class(sf::st_geometry(x), "sfc_LINESTRING")
  check_names(x, "blk")
  chk_not_subset(colnames(x), "..fwa_half")
  chk_null_or(layer, vld = vld_string)
  chk_flag(legend)

  if (!is.null(layer)) {
    check_names(x, layer)
  }

  chk_whole_numeric(x$blk)
  chk_not_any_na(x$blk)
  chk_gt(x$blk)

  geometry <- sf::st_geometry(x)

  downs <- sf::st_set_geometry(
    x,
    sf::st_cast(sf::st_line_sample(geometry, sample = 0), "POINT")
  )
  ups <- sf::st_set_geometry(
    x,
    sf::st_cast(sf::st_line_sample(geometry, sample = 1), "POINT")
  )

  streams <- halve_linestrings(x)

  fwa_mapview(
    streams,
    layer = layer,
    zcol = "..fwa_half",
    legend = legend,
    color = c("red", "blue"),
    ...
  ) +
    fwa_mapview(
      downs,
      col.regions = "red",
      layer.name = "down",
      legend = FALSE
    ) +
    fwa_mapview(ups, col.regions = "blue", layer.name = "up", legend = FALSE)
}
