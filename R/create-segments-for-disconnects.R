#' Stitch Segments
#'
#' Adds segments between the disconnected parts of a MULTILINESTRING.
#'
#' @param x A multiline string for a stream.
#' @param ... Unused.
#' @param tolerance
#'
#' @returns A data frame with the stitched segments added into the geometries.
#' @export
#' @details
#' The `fwa_stitch_segments()` assumes that the segments within the
#' MULTILINESTRING are in order and the correct direction to be stitched
#' together.
#'
#' If the segments could not be joined they will be returned as a MULTILINESTRING,
#' if the segments have no gaps left the row will be returned as a LINESTRING.
#'
#'
#'
#' @examples
fwa_stitch_segments <- function(x, ..., tolerance = 1) {
  chk_s3_class(x, "data.frame")
  chk_s3_class(x, "sf")
  check_data(x, nrow = TRUE)
  chk_unused(...)
  chk_gt(tolerance)

  # error handling for when no stiching or things don't have gaps - test to figure out what happens and then build in error handling

  sf_column_name <- sf_column_name(x)

  # if all linestrings then exit
  if (inherits(x[[sf_column_name]], "sfc_LINESTRING")) {
    msg("All geometries are LINESTRING nothing to stitch. \n", tidy = FALSE)
    return(x)
  }

  split_df <-
    x |>
    sf::st_zm() |>
    sf::st_sf() |>
    dplyr::rename(geometry := !!sf_column_name) |>
    dplyr::rowwise() |>
    dplyr::group_split()

  stiched_streams <- list()
  for (i in 1:length(split_df)) {

    segments <- st_cast(split_df[[i]][["geometry"]], "LINESTRING")
    df_distances <- segment_end_to_start_distance(segments)

    new_segments <- list()
    for (j in 1:nrow(df_distances)) {

      if (df_distances[i, ]$distance >= tolerance) {
        next
      }

      end <- st_line_sample(segments[df_distances[j,]$end], sample = 1)
      start <- st_line_sample(segments[df_distances[j,]$start], sample = 0)

      line <- st_linestring(rbind(st_coordinates(start), st_coordinates(end)))
      new_segments <- c(new_segments, list(line))
    }

    new_sf <- st_sfc(new_segments, crs = st_crs(x))
    new_sf <- st_sf(geometry = new_sf) |>
      st_zm()

    all_segments <- dplyr::bind_rows(split_df[[i]], new_sf) |>
      st_sf()

    multi <- st_combine(all_segments)
    multi <- st_cast(multi, "MULTILINESTRING")
    multi_sf <- st_sf(geometry = multi) |>
      sf::st_line_merge()

    stiched_df <-
      split_df[[i]] |>
      tibble::tibble() |>
      dplyr::mutate(
        multi_sf
      )

    stiched_df

    stiched_streams <- c(stiched_streams, list(stiched_df))
  }

  dplyr::bind_rows(stiched_streams) |>
    dplyr::rename(!!sf_column_name := geometry) |>
    sf::st_set_geometry(sf_column_name)

}


segment_end_to_start_distance <- function(segments) {

  coords_list <- lapply(segments, function(seg) st_coordinates(seg))
  end_points <- st_sfc(lapply(coords_list[-length(coords_list)], function(x) st_point(x[nrow(x), 1:2])), crs = st_crs(segments))
  start_points <- st_sfc(lapply(coords_list[-1], function(x) st_point(x[1, 1:2])), crs = st_crs(segments))


  distances <- numeric(length(end_points))
  for(i in seq_along(end_points)) {
    distances[i] <- as.numeric(st_distance(end_points[i], start_points[i], which = "Euclidean"))
  }

  df <- data.frame(
    end = 1:(length(segments) - 1),
    start = 2:length(segments),
    distance = distances
  )

  return(df)
}
