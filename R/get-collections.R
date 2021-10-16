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
