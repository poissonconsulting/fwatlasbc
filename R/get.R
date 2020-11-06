#' FWA get collection
#'
#' Get collection within watershed.
#'
#' @param watershed A single row sf object or sfc object of the watershed polygon (sfc geometry must inherit from sfc_POLYGON).
#' @param collection_id A character string of the collection id.
#' names correspond to column names and the list values correspond to the desired
#' value, e.g. `list(gnis_name = "Sangan River")`.
#' @param limit A positive whole number indicating the maximum number of features to return.
#' @return A sf object
#' @seealso \code{\link[fwapgr]{fwa_collection}}.
#' @export
#' @examples
#' \dontrun{
#' watershed <- fwa_blue_line_key_to_watershed(356308001)
#' fwa_get(watershed, "whse_basemapping.fwa_stream_networks_sp"))
#' }
fwa_get <- function(watershed,
                    collection_id,
                    limit = getOption("fwa.limit", 1000)){

  chk_whole_number(limit)
  chk_string(collection_id)
  bbox <- NULL
  epsg <- getOption("fwa.epsg", 3005)

  if(!is.null(watershed)){
    chk_sf_sfc(watershed)
    watershed <- sf::st_geometry(watershed)
    chk_sfc_polygon(watershed)
    epsg <- get_epsg(watershed)
    watershed_bbox <- watershed
    if(epsg != 4326){
      watershed_bbox <- sf::st_transform(watershed, 4326)
    }
    bbox <- sf::st_bbox(watershed_bbox)
  }

  x <- fwapgr::fwa_collection(collection_id,
                              bbox = bbox,
                              limit = limit,
                              epsg = epsg)
  # eliminates features inside bbox but outside watershed
  x[sf::st_intersects(x, watershed, sparse = FALSE)[,1],]
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
fwa_get_stream_network <- function(watershed,
                                   limit = getOption("fwa.limit", 1000)) {
  fwa_get(watershed,
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
fwa_get_lakes <- function(watershed,
                          limit = getOption("fwa.limit", 1000)) {
  fwa_get(watershed,
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
fwa_get_transport_lines <- function(watershed,
                                    limit = getOption("fwa.limit", 1000)) {
  fwa_get(watershed,
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
fwa_get_cultural_lines <- function(watershed,
                                   limit = getOption("fwa.limit", 1000)) {
  fwa_get(watershed,
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
fwa_get_cultural_points <- function(watershed,
                                    limit = getOption("fwa.limit", 1000)) {
  fwa_get(watershed,
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
fwa_get_wetlands <- function(watershed,
                             limit = getOption("fwa.limit", 1000)) {
  fwa_get(watershed,
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
fwa_get_railway_tracks <- function(watershed,
                                   limit = getOption("fwa.limit", 1000)) {
  fwa_get(watershed,
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
fwa_get_transmission_lines <- function(watershed,
                                       limit = getOption("fwa.limit", 1000)) {
  fwa_get(watershed,
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
fwa_get_obstructions <- function(watershed,
                                 limit = getOption("fwa.limit", 1000)) {
  fwa_get(watershed,
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
fwa_get_rivers <- function(watershed,
                           limit = getOption("fwa.limit", 1000)) {
  fwa_get(watershed,
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
fwa_get_manmade_waterbodies <- function(watershed,
                                        limit = getOption("fwa.limit", 1000)) {
  fwa_get(watershed,
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
fwa_get_named_streams <- function(watershed,
                                  limit = getOption("fwa.limit", 1000)) {
  fwa_get(watershed,
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
#' fwa_get_glaciers <- function(watershed,
#'                              limit = getOption("fwa.limit", 1000)) {
#'   fwa_get(watershed,
#'           limit = limit,
#'           collection_id = "whse_basemapping.fwa_glaciers_poly")
#' }



