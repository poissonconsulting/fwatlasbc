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
  chk_not_subset(colnames(x), "..geometry")

  x <- x |>
    sf::st_zm()
        
  ## if no rows then return
  if (nrow(x) == 0){
    return(x)
  }

  sf_column_name <- sf_column_name(x)

  ## if all LINESTRINGS then return
  if (inherits(x[[sf_column_name]], "sfc_LINESTRING")) {
    return(x)
  }

  rename_flag <- FALSE
  ## if the sfc column isn't geometry check if geometry is in the other column names
  if (sf_column_name != "geometry" && "geometry" %in% colnames(x)) {
    ## if so rename to reserved name
    rename_flag <- TRUE
    x <- rename(x, "..geometry" = "geometry")
  }

  ordered_segments <-
    x |>
    sf::st_sf() |>
    dplyr::rename("geometry" := !!sf_column_name) |>
    order_segments()

  x <- dplyr::bind_rows(ordered_segments) |>
    dplyr::rename(!!sf_column_name := "geometry") |>
    sf::st_set_geometry(sf_column_name)

  if (rename_flag) {
    x <- rename(x, "geometry" = "..geometry")
  }

  x
}

order_segments <- function(x) {

  split_df <- x |>
    dplyr::rowwise() |>
    dplyr::group_split()
        
  stitched_streams <- list()
  for (i in 1:length(split_df)) {

    ## early exit if not a MULTILINESTRING
    if (!inherits(split_df[[i]][["geometry"]], "sfc_MULTILINESTRING")) {
      stitched_streams <- c(stitched_streams, list(split_df[[i]]))
      next
    }

  segment_start_ends <- split_multilinestring(split_df[[i]][["geometry"]])
  distance_df <- calculate_end_to_start_distances(segment_start_ends)
  segments <- sf::st_cast(split_df[[i]][["geometry"]], "LINESTRING")
  segment_order <- order_segments_dynamic(segments, distance_df)

  multi <- sf::st_combine(segments[segment_order])
  multi <- sf::st_cast(multi, "MULTILINESTRING")

  stitched_df <-
    split_df[[i]] |>
    tibble::tibble() |>
    dplyr::mutate(
      geometry = multi
    )

  stitched_streams <- c(stitched_streams, list(stitched_df))
  }
  stitched_streams
}

split_multilinestring <- function(geometry) {

  segments <- sf::st_cast(geometry, "LINESTRING")
  segments <- segments[!sf::st_is_empty(segments)]

  coords <- lapply(segments, sf::st_coordinates)
  keep <- lengths(coords) >= 2

  segments <- segments[keep]
  coords   <- coords[keep]

  starts <- lapply(coords, \(c) c[1, 1:2])
  ends   <- lapply(coords, \(c) c[nrow(c), 1:2])

  tibble::tibble(
    linestring = segments,
    start_pt = sf::st_sfc(lapply(starts, sf::st_point), crs = sf::st_crs(geometry)),
    end_pt   = sf::st_sfc(lapply(ends,   sf::st_point), crs = sf::st_crs(geometry))
  )
}


calculate_end_to_start_distances <- function(df) {

  dmat <- sf::st_distance(
    df$end_pt,
    df$start_pt,
    which = "Euclidean"
  )

  ## Convert to long form and drop self-pairs
  tibble::as_tibble(dmat, rownames = "from") |>
    dplyr::mutate(across(dplyr::starts_with("V"), as.numeric)) |>
    tidyr::pivot_longer(
      dplyr::starts_with("V"),
      names_to = "to",
      values_to = "distance"
    ) |>
    dplyr::mutate(
      from = as.integer(.data$from),
      to = as.integer(stringr::str_extract(.data$to, "\\d")),
      distance = as.numeric(.data$distance)
    ) |>
    dplyr::filter(.data$from != .data$to)
}

order_segments_dynamic <- function(segments, distance_df) {
  number_of_segments <- length(segments)
  segment_length <- as.numeric(sf::st_length(segments))

  # Start with the longest segment
  longest_segment <- which.max(segment_length)
  orders_used <- longest_segment
  segment_order <- longest_segment

  ## Copy of distance_df to filter each pass
  remaining_distances <- distance_df

  while(length(orders_used) < number_of_segments) {

    ## Find all distances involving any current segments
    current_search <- remaining_distances |>
      dplyr::filter(.data$from %in% orders_used | .data$to %in% orders_used)

    if (nrow(current_search) == 0) break

    ## Pick the shortest distance
    next_df <- current_search |>
      dplyr::arrange(.data$distance) |>
      dplyr::slice(1)

    ## Track the order of the segments
    current_from_to <- c(next_df$from, next_df$to)
    new_segment <- setdiff(current_from_to, orders_used)

    if (new_segment %in% next_df$from) {
      segment_order <- c(next_df$from, segment_order)
    } else (
      segment_order <- c(segment_order, next_df$to)
    )

    ## Track which segments have been used
    orders_used <- unique(c(orders_used, next_df$from, next_df$to))

    ## Remove distances involving used segments (to avoid reusing)
    remaining_distances <- remaining_distances |>
      dplyr::filter(next_df$from != .data$from) |>
      dplyr::filter(next_df$to != .data$to) |>
      dplyr::filter(!(.data$from == next_df$to & .data$to == next_df$from))
  }
  segment_order
}
