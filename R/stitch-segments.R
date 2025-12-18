#' Stitch Segments
#'
#' Adds segments between the disconnected parts of a MULTILINESTRING.
#'
#' @param x A spatial data frame.
#' @param ... Unused.
#' @param tolerance A numeric value for the maximum euclidean distance between
#'   the end a segment to the start of a segment that will create a stitched
#'   segment. If the distance is greater then the tolerance then a segment will
#'   not be created.
#'
#' @returns A data frame with the stitched segments added into the geometries.
#' @export
#' @details
#' The `fwa_stitch_segments()` assumes that the segments within the
#' MULTILINESTRING are in order and the correct direction to be stitched
#' together.
#'
#' If the segments could not be joined they will be returned as a
#' MULTILINESTRING, if the segments have no gaps left the row will be returned
#' as a LINESTRING.
#'
#' @examples
#' \dontrun{
#' test_stream <- sf::st_sfc(
#'   sf::st_multilinestring(
#'     c(sf::st_linestring(matrix(c(0, 0, 1, 1), ncol = 2, byrow = TRUE)),
#'       sf::st_linestring(matrix(c(1.05, 1.05, 2, 2), ncol = 2, byrow = TRUE)),
#'       sf::st_linestring(matrix(c(2.1, 2.1, 3, 3), ncol = 2, byrow = TRUE))
#'     )
#'   ),
#'   crs = 26911
#'  )
#'
#'  df <- data.frame(
#'   name = "stream 1",
#'   blk = 1,
#'   geometry = test_stream
#'  ) |>
#'  sf::st_set_geometry("geometry")
#'
#'  # to create two 2 segments to connect the whole line
#'  fwa_stitch_segments(df)
#'
#'  # tolerance is set so only 1 segment is added and the line is still not connected
#'  fwa_stitch_segments(df, tolerance = 0.1)
#' }
fwa_stitch_segments <- function(x, ..., tolerance = 5) {
  chk_s3_class(x, "data.frame")
  chk_s3_class(x, "sf")
  chk_not_subset(colnames(x), "..geometry")
  chk_unused(...)
  chk_gt(tolerance)

  x <- x |>
    sf::st_zm() |>
    sf::st_sf()
      
  # if no rows then return
  if (nrow(x) == 0){
    return(x)
  }

  sf_column_name <- sf_column_name(x)

  # if all LINESTRINGS then return
  if (inherits(x[[sf_column_name]], "sfc_LINESTRING")) {
    return(x)
  }

  # if the sfc column isn't geometry
  # check if geometry is in the other column names
  if (sf_column_name != "geometry" & "geometry" %in% colnames(x)) {
    # if so rename to reserved name
    x <- rename(x, "..geometry" = "geometry")
  }

  x <- x |>
    dplyr::rename("geometry" := !!sf_column_name) |>
    stitch_segments(tolerance) |>
    dplyr::rename(!!sf_column_name := "geometry") |>
    sf::st_set_geometry(sf_column_name)

  if ("..geometry" %in% colnames(x)) {
    x <- rename(x, "geometry" = "..geometry")
  }

  x
}

stitch_segments <- function(x, tolerance) {
  x |>
    dplyr::rowwise() |>
    dplyr::group_split() |>
    purrr::map(stitch_segs, tolerance = tolerance) |>
    dplyr::bind_rows()
}

stitch_segs <- function(x, tolerance) {
  sfc <- x[["geometry"]]
    # early exit if not a MULTILINESTRING
    if (!inherits(sfc, "sfc_MULTILINESTRING")) {
      return(x)
    }

    segments <- sf::st_cast(sfc, "LINESTRING")
    df_distances <- segment_end_to_start_distance(segments)

    new_segments <- list()
    for (j in 1:nrow(df_distances)) {

      if (df_distances[j, ]$distance >= tolerance) {
        next
      }

      end <- sf::st_line_sample(segments[df_distances[j,]$end], sample = 1)
      start <- sf::st_line_sample(segments[df_distances[j,]$start], sample = 0)

      line <- sf::st_linestring(rbind(sf::st_coordinates(start), sf::st_coordinates(end)))
      new_segments <- c(new_segments, list(line))
    }

    new_sf <- sf::st_sfc(new_segments, crs = sf::st_crs(x))
    new_sf <- sf::st_sf(geometry = new_sf) |>
      sf::st_zm()

    all_segments <- dplyr::bind_rows(x, new_sf) |>
      sf::st_sf()

    multi <- sf::st_combine(all_segments) |>
      sf::st_cast("MULTILINESTRING")
    multi_sf <- sf::st_sf(geometry = multi) |>
      sf::st_line_merge()

    x |>
      tibble::tibble() |>
      dplyr::mutate(
        multi_sf
      )
}

segment_end_to_start_distance <- function(segments) {

  coords_list <- lapply(segments, function(seg) sf::st_coordinates(seg))
  end_points <- sf::st_sfc(lapply(coords_list[-length(coords_list)], function(x) sf::st_point(x[nrow(x), 1:2])), crs = sf::st_crs(segments))
  start_points <- sf::st_sfc(lapply(coords_list[-1], function(x) sf::st_point(x[1, 1:2])), crs = sf::st_crs(segments))

  distances <- numeric(length(end_points))
  for(i in seq_along(end_points)) {
    distances[i] <- as.numeric(sf::st_distance(end_points[i], start_points[i], which = "Euclidean"))
  }

  data.frame(
    end = 1:(length(segments) - 1),
    start = 2:length(segments),
    distance = distances
  )
}
