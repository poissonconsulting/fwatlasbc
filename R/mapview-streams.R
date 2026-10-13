split_linestring <- function(x) {
  coords <- sf::st_coordinates(x)[, c("X", "Y"), drop = FALSE]
  n <- nrow(coords)
  lengths <- sqrt(diff(coords[, "X"])^2 + diff(coords[, "Y"])^2)
  total <- sum(lengths)
  position <- (cumsum(lengths) - lengths / 2) / total
  if (total == 0) {
    position <- rep(0.5, n - 1)
  }
  pieces <- purrr::map(seq_len(n - 1), function(i) {
    sf::st_linestring(coords[c(i, i + 1), , drop = FALSE])
  })
  list(pieces = pieces, position = position)
}

split_linestrings <- function(x) {
  geometry <- sf::st_geometry(x)
  splits <- purrr::map(geometry, split_linestring)
  n <- purrr::map_int(splits, function(split) length(split$pieces))

  x <- sf::st_drop_geometry(x)[rep(seq_len(nrow(x)), n), , drop = FALSE]
  x$..fwa_position <- unlist(purrr::map(splits, "position"))
  x$geometry <- sf::st_sfc(
    unlist(purrr::map(splits, "pieces"), recursive = FALSE),
    crs = sf::st_crs(geometry)
  )
  sf::st_sf(x)
}

#' Map View Streams
#'
#' Plots stream linestrings colored as a gradient from blue at the start
#' to red at the end of each linestring, with the start as a blue point
#' labelled "down" and the end as a red point labelled "up",
#' so that the direction of the streams can be seen.
#' Streams oriented upstream (as produced by [`fwa_flip_streams()`])
#' run from blue at their mouth to red at their source.
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
  chk_not_subset(colnames(x), "..fwa_position")
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

  streams <- split_linestrings(x)

  fwa_mapview(
    streams,
    layer = layer,
    zcol = "..fwa_position",
    legend = legend,
    color = grDevices::colorRampPalette(c("blue", "red")),
    at = seq(0, 1, by = 0.1),
    ...
  ) +
    fwa_mapview(
      downs,
      col.regions = "blue",
      layer.name = "down",
      legend = FALSE
    ) +
    fwa_mapview(ups, col.regions = "red", layer.name = "up", legend = FALSE)
}
