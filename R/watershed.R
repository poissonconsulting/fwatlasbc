fwa_point_to_watershed <- function(point) {
  chk_sf_sfc(point)
  point <- sf::st_geometry(point)
  chk_sfc_point(point)
  coords <- sf::st_coordinates(point)
  epsg <- get_epsg(point)
  fwa_xy_to_watershed(x = coords[1], y = coords[2], epsg = epsg)
}

fwa_xy_to_watershed <- function(x, y, epsg = getOption("fwa.epsg", 3005)) {
  chk_number(x)
  chk_number(y)
  chk_whole_number(epsg)

  index <- fwapgr::fwa_index_point(x = x, y = y, srid = epsg)
  wshed <- fwa_watershed_at_measure(blue_line_key = index$blue_line_key,
                           downstream_route_measure = index$downstream_route_measure,
                           epsg = epsg)
  wshed["geometry"]
}

fwa_blue_line_key_to_watershed <- function(blue_line_key, distance_from_mouth = 0,
                                           epsg = getOption("fwa.epsg", 3005)) {
  chk_whole_number(blue_line_key)
  chk_gt(blue_line_key)
  chk_whole_number(distance_from_mouth)
  chk_gte(distance_from_mouth)

  wshed <- fwa_watershed_at_measure(blue_line_key = blue_line_key,
                            downstream_route_measure = distance_from_mouth,
                            epsg = epsg)
  wshed$blue_line_key <- blue_line_key
  wshed$distance_from_mouth <- distance_from_mouth
  wshed[c("blue_line_key", "distance_from_mouth", "geometry")]
}
