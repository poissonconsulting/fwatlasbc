fwa_point_to_watershed <- function(point) {
  # expect sfc object length 1
  # get projection
  # then api call to return polygon in same projection
}

fwa_xy_to_watershed <- function(x, y, epsg = getOption("fwa.epsg", 3005)) {
  # get projection
  # then api call to return polygon in same projection
}

fwa_blue_line_key_to_watershed <- function(blue_line_key, distance_upstream = 0,
                                           epsg = getOption("fwa.epsg", 3005)) {
  chk_whole_number(blue_line_key)
  chk_gt(blue_line_key)
  chk_whole_number(distance_upstream)
  chk_gte(distance_upstream)

  # api call
}
