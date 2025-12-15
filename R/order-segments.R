#' Get Stream Segments in Order
#'
#' Orders stream segments so they are in order to be stitched together.
#'
#' @param x A spatial data frame.
#'
#' @returns A spatial data frame.
#' @export
#' @details The function works by locating the longest segment and then finding
#' the nearest segment to it and continues this pattern until all segments have
#' been iterated through.
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
#'  fwa_order_segments(df)
#' }
fwa_order_segments <- function(x) {

  chk_s3_class(x, "data.frame")
  chk_s3_class(x, "sf")

  sf_column_name <- sf_column_name(x)

  rename_flag <- FALSE
  # if the sfc column isn't geometry
  if (sf_column_name != "geometry") {
    # check if geometry is in the other column names
    if ("geometry" %in% colnames(x)) {
      # if so rename to reserved name
      rename_flag <- TRUE
      x <- rename(x, ".dsklsjhtiu3" = "geometry")
    }
  }

  # if no rows then return
  if (nrow(x) == 0){
    return(x)
  }

  # if all LINESTRINGS then return
  if (inherits(x[[sf_column_name]], "sfc_LINESTRING")) {
    return(x)
  }

  split_df <-
    x |>
    sf::st_zm() |>
    sf::st_sf() |>
    dplyr::rename("geometry" := !!sf_column_name) |>
    dplyr::rowwise() |>
    dplyr::group_split()

  stiched_streams <- list()
  for (i in 1:length(split_df)) {

    # early exit if not a MULTILINESTRING
    if (!inherits(split_df[[i]][["geometry"]], "sfc_MULTILINESTRING")) {
      stiched_streams <- c(stiched_streams, list(split_df[[i]]))
      next
    }

  df_df <- split_multilinestring(split_df[[i]][["geometry"]])
  distance_df <- calculate_end_to_start_distances(df_df)

  segments <- sf::st_cast(split_df[[i]][["geometry"]], "LINESTRING")

  segment_order <- order_segments_dynamic(segments, distance_df)


  multi <- sf::st_combine(segments[segment_order])
  multi <- sf::st_cast(multi, "MULTILINESTRING")

  stiched_df <-
    split_df[[i]] |>
    tibble::tibble() |>
    dplyr::mutate(
      geometry = multi
    )

  stiched_streams <- c(stiched_streams, list(stiched_df))
  }

  x <- dplyr::bind_rows(stiched_streams) |>
    dplyr::rename(!!sf_column_name := "geometry") |>
    sf::st_set_geometry(sf_column_name)

  if (rename_flag) {
    x <- rename(x, "geometry" = ".dsklsjhtiu3")
  }

  x
}

split_multilinestring <- function(mls) {

  segs <- sf::st_cast(mls, "LINESTRING")

  # Remove empty or degenerate segments
  segs <- segs[!sf::st_is_empty(segs)]
  coords_list <- lapply(segs, sf::st_coordinates)
  segs <- segs[sapply(coords_list, nrow) >= 2]
  coords_list <- coords_list[sapply(coords_list, nrow) >= 2]

  # Extract start and end coordinates
  starts <- lapply(coords_list, function(c) c[1, 1:2])
  ends   <- lapply(coords_list, function(c) c[nrow(c), 1:2])

  # Build tibble
  tibble::tibble(
    linestring = segs,
    x_start = purrr::map_dbl(starts, 1),
    y_start = purrr::map_dbl(starts, 2),
    x_end   = purrr::map_dbl(ends,   1),
    y_end   = purrr::map_dbl(ends,   2)
  )
}


calculate_end_to_start_distances <- function(df) {

  n <- nrow(df)

  # Create all combinations of lines where i != j
  combos <- expand.grid(i = 1:n, j = 1:n) |>
    dplyr::filter(.data$i != .data$j)

  # Compute Euclidean distance from end of line i to start of line j
  dist_df <- combos |>
    dplyr::mutate(
      distance = sqrt((df$x_end[.data$i] - df$x_start[.data$j])^2 + (df$y_end[.data$i] - df$y_start[.data$j])^2)
    ) |>
    dplyr::select(from = "i", to = "j", "distance")

  dist_df
}

order_segments_dynamic <- function(segments, distance_df) {
  number_of_segments <- length(segments)
  segment_length <- as.numeric(sf::st_length(segments))

  # Start with the longest segment
  longest_segment <- which.max(segment_length)
  orders_used <- longest_segment

  segment_order <- longest_segment

  # Copy of distance_df to filter each pass
  remaining_distances <- distance_df

  while(length(orders_used) < number_of_segments) {

    # Find all distances involving any currently used segment
    current_search <- remaining_distances |>
      dplyr::filter(.data$from %in% orders_used | .data$to %in% orders_used)

    if(nrow(current_search) == 0) break

    # Pick the shortest distance
    next_df <- current_search |>
      dplyr::arrange(.data$distance) |>
      dplyr::slice(1)

    # Add the connected segments
    current_from_to <- c(next_df$from, next_df$to)
    new_segment <- setdiff(current_from_to, orders_used)

    if (new_segment %in% next_df$from) {
      segment_order <- c(next_df$from, segment_order)
    } else (
      segment_order <- c(segment_order, next_df$to)
    )

    orders_used <- unique(c(orders_used, next_df$from, next_df$to))

    # Remove distances involving used segments (to avoid reusing)
    remaining_distances <- remaining_distances |>
      dplyr::filter(next_df$from != .data$from) |>
      dplyr::filter(next_df$to != .data$to)
  }

  segment_order
}

# this take the first one in the list
which.max(c(1L, 2L, 55L, 55L))


