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

  chk_string(collection_id)
  chk_null_or(watershed, vld = vld_sf_sfc)
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
