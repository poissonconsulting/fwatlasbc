#' FWA get collection
#'
#' Get collection within watershed.
#'
#' @param collection_id A character string of the collection id.
#' names correspond to column names and the list values correspond to the desired
#' value, e.g. `list(gnis_name = "Sangan River")`.
#' @param watershed A single row sf object or sfc object of the watershed polygon (sfc geometry must inherit from sfc_POLYGON).
#' @param intersect A flag specifying whether to intersect the individual features with the watershed. Useful if the watershed is not a true watershed.
#' @param epsg A positive whole number of the epsg to return the collection to.
#' @param limit A positive whole number indicating the maximum number of features to return.
#' @return A sf object
#' @seealso \code{\link[fwapgr]{fwa_collection}}.
#' @export
#' @examples
#' \dontrun{
#' watershed <- fwa_blue_line_key_to_watershed(356308001)
#' fwa_get(watershed, "whse_basemapping.fwa_stream_networks_sp"))
#' }
fwa_get <- function(collection_id,
                    watershed = NULL,
                    intersect = FALSE,
                    epsg = getOption("fwa.epsg", 3005),
                    limit = getOption("fwa.limit", 10000)){

  chk_null_or(watershed, vld = vld_sf_sfc)
  chk_string(collection_id)
  chk_flag(intersect)
  chk_whole_number(epsg)
  chk_gt(epsg)
  chk_null_or(limit, vld = vld_whole_number)

  if(is.null(watershed)) {
    x <- fwapgr::fwa_collection(
        collection_id,
        limit = limit,
        epsg = epsg)
    x <- mutate(x, across(matches("gnis_id_\\d"), as.integer))
    return(x)
  }

  watershed <- sf::st_geometry(watershed)
  chk_sfc_polygon(watershed)

  bbox <- bbox(watershed)
  x <- fwapgr::fwa_collection(collection_id,
                              bbox = bbox,
                              limit = limit,
                              epsg = epsg)

  watershed <- sf::st_transform(watershed, epsg)
  watershed <- sf::st_make_valid(watershed)
  suppressMessages(x <- x[sf::st_intersects(x, watershed, sparse = FALSE)[,1],])
  x <- sf::st_make_valid(x)
  if(intersect) {
    suppressWarnings(x <- sf::st_intersection(x, watershed))
    x <- sf::st_make_valid(x)
  }
  x
}

#' FWA get stream network
#'
#' Get stream network features within watershed.
#'
#' @inheritParams fwa_get
#' @return A sf object
#' @seealso \code{\link[fwapgr]{fwa_collection}}.
#' @export
#' @examples
#' \dontrun{
#' watershed <- fwa_blue_line_key_to_watershed(356308001)
#' fwa_get_stream_network(watershed)
#' }
fwa_get_stream_network <- function(watershed = NULL,
                                   intersect = FALSE,
                                   epsg = getOption("fwa.epsg", 3005),
                                   limit = getOption("fwa.limit", 10000)) {
  fwa_get(watershed,
          intersect = intersect,
          epsg = epsg,
          limit = limit,
          collection_id = "whse_basemapping.fwa_stream_networks_sp")
}

#' FWA get lakes
#'
#' Get lake features within watershed.
#'
#' @inheritParams fwa_get
#' @return A sf object
#' @seealso \code{\link[fwapgr]{fwa_collection}}.
#' @export
#' @examples
#' \dontrun{
#' watershed <- fwa_blue_line_key_to_watershed(356308001)
#' fwa_get_lakes(watershed)
#' }
fwa_get_lakes <- function(watershed = NULL,
                          intersect = FALSE,
                          epsg = getOption("fwa.epsg", 3005),
                          limit = getOption("fwa.limit", 10000)) {
  fwa_get(watershed = watershed,
          intersect = intersect,
          epsg = epsg,
          limit = limit,
          collection_id = "whse_basemapping.fwa_lakes_poly")
}

#' FWA get transport lines
#'
#' Get transport line features within watershed.
#'
#' @inheritParams fwa_get
#' @return A sf object
#' @seealso \code{\link[fwapgr]{fwa_collection}}.
#' @export
#' @examples
#' \dontrun{
#' watershed <- fwa_blue_line_key_to_watershed(356308001)
#' fwa_get_transport_lines(watershed)
#' }
fwa_get_transport_lines <- function(watershed = NULL,
                                    intersect = FALSE,
                                    epsg = getOption("fwa.epsg", 3005),
                                    limit = getOption("fwa.limit", 10000)) {
  fwa_get(watershed = watershed,
          intersect = intersect,
          epsg = epsg,
          limit = limit,
          collection_id = "whse_basemapping.transport_line")
}

#' FWA get cultural lines
#'
#' Get cultural line features within watershed.
#'
#' @inheritParams fwa_get
#' @return A sf object
#' @seealso \code{\link[fwapgr]{fwa_collection}}.
#' @export
#' @examples
#' \dontrun{
#' watershed <- fwa_blue_line_key_to_watershed(356308001)
#' fwa_get_cultural_lines(watershed)
#' }
fwa_get_cultural_lines <- function(watershed = NULL,
                                   intersect = FALSE,
                                   epsg = getOption("fwa.epsg", 3005),
                                   limit = getOption("fwa.limit", 10000)) {
  fwa_get(watershed = watershed,
          intersect = intersect,
          epsg = epsg,
          limit = limit,
          collection_id = "whse_basemapping.trim_cultural_lines")
}

#' FWA get cultural points
#'
#' Get cultural point features within watershed.
#'
#' @inheritParams fwa_get
#' @return A sf object
#' @seealso \code{\link[fwapgr]{fwa_collection}}.
#' @export
#' @examples
#' \dontrun{
#' watershed <- fwa_blue_line_key_to_watershed(356308001)
#' fwa_get_cultural_points(watershed)
#' }
fwa_get_cultural_points <- function(watershed = NULL,
                                    intersect = FALSE,
                                    epsg = getOption("fwa.epsg", 3005),
                                    limit = getOption("fwa.limit", 10000)) {
  fwa_get(watershed = watershed,
          intersect = intersect,
          epsg = epsg,
          limit = limit,
          collection_id = "whse_basemapping.trim_cultural_points")
}

#' FWA get wetlands
#'
#' Get wetland features within watershed.
#'
#' @inheritParams fwa_get
#' @return A sf object
#' @seealso \code{\link[fwapgr]{fwa_collection}}.
#' @export
#' @examples
#' \dontrun{
#' watershed <- fwa_blue_line_key_to_watershed(356308001)
#' fwa_get_wetlands(watershed)
#' }
fwa_get_wetlands <- function(watershed = NULL,
                             intersect = FALSE,
                             epsg = getOption("fwa.epsg", 3005),
                             limit = getOption("fwa.limit", 10000)) {
  fwa_get(watershed = watershed,
          intersect = intersect,
          epsg = epsg,
          limit = limit,
          collection_id = "whse_basemapping.fwa_wetlands_poly")
}

#' FWA get railway tracks
#'
#' Get railway track features within watershed.
#'
#' @inheritParams fwa_get
#' @return A sf object
#' @seealso \code{\link[fwapgr]{fwa_collection}}.
#' @export
#' @examples
#' \dontrun{
#' watershed <- fwa_blue_line_key_to_watershed(356308001)
#' fwa_get_railway_tracks(watershed)
#' }
fwa_get_railway_tracks <- function(watershed = NULL,
                                   intersect = FALSE,
                                   epsg = getOption("fwa.epsg", 3005),
                                   limit = getOption("fwa.limit", 10000)) {
  fwa_get(watershed = watershed,
          intersect = intersect,
          epsg = epsg,
          limit = limit,
          collection_id = "whse_basemapping.gba_railway_tracks_sp")
}

#' FWA get transmission lines
#'
#' Get transmission line features within watershed.
#'
#' @inheritParams fwa_get
#' @return A sf object
#' @seealso \code{\link[fwapgr]{fwa_collection}}.
#' @export
#' @examples
#' \dontrun{
#' watershed <- fwa_blue_line_key_to_watershed(356308001)
#' fwa_get_transmission_lines(watershed)
#' }
fwa_get_transmission_lines <- function(watershed = NULL,
                                       intersect = FALSE,
                                       epsg = getOption("fwa.epsg", 3005),
                                       limit = getOption("fwa.limit", 10000)) {
  fwa_get(watershed = watershed,
          intersect = intersect,
          epsg = epsg,
          limit = limit,
          collection_id = "whse_basemapping.gba_transmission_lines_sp")
}

#' FWA get obstructions
#'
#' Get obstruction features within watershed.
#'
#' @inheritParams fwa_get
#' @return A sf object
#' @seealso \code{\link[fwapgr]{fwa_collection}}.
#' @export
#' @examples
#' \dontrun{
#' watershed <- fwa_blue_line_key_to_watershed(356308001)
#' fwa_get_obstructions(watershed)
#' }
fwa_get_obstructions <- function(watershed = NULL,
                                 intersect = FALSE,
                                 epsg = getOption("fwa.epsg", 3005),
                                 limit = getOption("fwa.limit", 10000)) {
  fwa_get(watershed,
          intersect = intersect,
          epsg = epsg,
          limit = limit,
          collection_id = "whse_basemapping.fwa_obstructions_sp")
}

#' FWA get rivers
#'
#' Get river features within watershed.
#'
#' @inheritParams fwa_get
#' @return A sf object
#' @seealso \code{\link[fwapgr]{fwa_collection}}.
#' @export
#' @examples
#' \dontrun{
#' watershed <- fwa_blue_line_key_to_watershed(356308001)
#' fwa_get_rivers(watershed)
#' }
fwa_get_rivers <- function(watershed = NULL,
                           intersect = FALSE,
                           epsg = getOption("fwa.epsg", 3005),
                           limit = getOption("fwa.limit", 10000)) {
  fwa_get(watershed = watershed,
          intersect = intersect,
          epsg = epsg,
          limit = limit,
          collection_id = "whse_basemapping.fwa_rivers_poly")
}

#' FWA get manmade waterbodies
#'
#' Get manmade waterbody features within watershed.
#'
#' @inheritParams fwa_get
#' @return A sf object
#' @seealso \code{\link[fwapgr]{fwa_collection}}.
#' @export
#' @examples
#' \dontrun{
#' watershed <- fwa_blue_line_key_to_watershed(356308001)
#' fwa_get_manmade_waterbodies(watershed)
#' }
fwa_get_manmade_waterbodies <- function(watershed = NULL,
                                        intersect = FALSE,
                                        epsg = getOption("fwa.epsg", 3005),
                                        limit = getOption("fwa.limit", 10000)) {
  fwa_get(watershed = watershed,
          intersect = intersect,
          epsg = epsg,
          limit = limit,
          collection_id = "whse_basemapping.fwa_manmade_waterbodies_poly")
}

#' FWA get named_streams
#'
#' Get named stream features within watershed.
#'
#' @inheritParams fwa_get
#' @return A sf object
#' @seealso \code{\link[fwapgr]{fwa_collection}}.
#' @export
#' @examples
#' \dontrun{
#' watershed <- fwa_blue_line_key_to_watershed(356308001)
#' fwa_get_named_streams(watershed)
#' }
fwa_get_named_streams <- function(watershed = NULL,
                                  intersect = FALSE,
                                  epsg = getOption("fwa.epsg", 3005),
                                  limit = getOption("fwa.limit", 10000)) {
  fwa_get(watershed = watershed,
          intersect = intersect,
          epsg = epsg,
          limit = limit,
          collection_id = "whse_basemapping.fwa_named_streams")
}

#' #' FWA get glaciers
#' #'
#' #' Get glacier features within watershed.
#' #'
#' #' @inheritParams fwa_get
#' #' @return A sf object
#' #' @seealso \code{\link[fwapgr]{fwa_collection}}.
#' #' @export
#' #' @examples
#' #' \dontrun{
#' #' watershed <- fwa_blue_line_key_to_watershed(356308001)
#' #' fwa_get_glaciers(watershed)
#' #' }
#' fwa_get_glaciers <- function(watershed = NULL,
#'                              intersect = FALSE,
#'                              epsg = getOption("fwa.epsg", 3005),
#'                              limit = getOption("fwa.limit", 10000)) {
#' fwa_get(watershed = watershed,
#'         epsg = epsg,
#'         intersect = intersect,
#'         limit = limit,
#'        collection_id = "whse_basemapping.fwa_glaciers_poly")
#' }
