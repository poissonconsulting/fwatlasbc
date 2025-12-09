#' Title
#'
#' @param x A multiline string for a stream.
#' @param tolerance
#'
#' @returns
#' @export
#'
#' @examples
create_segments_for_disconnects <- function(x, tolerance = 1) {

  segments <- st_cast(x, "LINESTRING")
  df_distances <- segment_end_to_start_distance(segments)

  new_segments <- list()
  for (i in 1:nrow(df_distances)) {

    if (df_distances[i, ]$distance >= tolerance) {
      next
    }

    end <- st_line_sample(segments[df_distances[i,]$end], n = 1, density = 1, sample = 1)
    start <- st_line_sample(segments[df_distances[i,]$start], n = 1, density = 1, sample = 0)

    line <- st_linestring(rbind(st_coordinates(start), st_coordinates(end)))
    new_segments <- c(new_segments, list(line))
  }

  new_sf <- st_sfc(new_segments, crs = st_crs(x))
  new_sf <- st_sf(geometry = new_sf) %>%
    st_zm()

  all_segments <- rbind(data.frame(segments), new_sf) %>%
    st_sf()

  multi <- st_combine(all_segments)
  multi <- st_cast(multi, "MULTILINESTRING")
  multi_sf <- st_sf(geometry = multi) %>%
    sf::st_line_merge()

  multi_sf
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
#
#
#
#
#
#
#
#
# # Function assumes
# # 1. Segments are in order within the multiline string
# # 2. Direction of segments are correct
# # 3. Ending criteria is based on tolerance, try 5 m as starting value
# # Function takes in a multiline string
# # Function outputs a linestring if everything was combined other multiline string of the value
#
#
# library(sf)
#
# # example 1, 3 segments straight line
# seg_1 <-
#   st_linestring(
#     matrix(
#       c(
#         0,   0,
#         1,   1
#       ),
#       ncol = 2,
#       byrow = TRUE
#     )
#   )
#
# seg_2 <- st_linestring(matrix(c(
#   1.05, 1.05,
#   2,    2
# ), ncol = 2, byrow = TRUE))
#
# seg_3 <- st_linestring(matrix(c(
#   2.1,  2.1,
#   3,    3
# ), ncol = 2, byrow = TRUE))
#
#
# test_stream_1 <- st_sfc(
#   st_multilinestring(c(seg_1, seg_2, seg_3)),
#   crs = 26911
# )
#
# test_1 <- tibble::tibble(
#   stream = "stream a",
#   geometry = test_stream_1
# ) %>%
#   poisspatial::ps_activate_sfc()
#
# mapview::mapview(test_stream_1)
#
#
#
#
#
#
#
#
#
#
# # takes multiline string and creats a set of line strings for it
# segments <- st_cast(test_1[["geometry"]], "LINESTRING")
#
# segments <- st_cast(segments, "LINESTRING")
#
#
#
# distance_df <- segment_end_to_start_distance(test_1$geometry)
#
#
# new_segments <- list()
# for (i in 1:nrow(distance_df)) {
#   end <- st_line_sample(segments[distance_df[i,]$end], n = 1, density = 1, sample = 1)
#   start <- st_line_sample(segments[distance_df[i,]$start], n = 1, density = 1, sample = 0)
#
#   line <- st_linestring(rbind(st_coordinates(start), st_coordinates(end)))
#   new_segments <- c(new_segments, list(line))
# }
#
#
#
# new_sf <- st_sfc(new_segments, crs = st_crs(segments))
# new_sf <- st_sf(geometry = new_sf) %>%
#   st_zm()
#
# all_segments <- rbind(data.frame(segments), new_sf)
#
# multi <- st_combine(all_segments)
# multi <- st_cast(multi, "MULTILINESTRING")
# multi_sf <- st_sf(geometry = multi) %>%
#   sf::st_line_merge()
#
#
# mapview(multi_sf)
#
