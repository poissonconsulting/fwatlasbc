#' FWA stream network
#'
#' Get stream network features from gnis_name.
#'
#' @inheritParams fwapgr::fwa_collection
#' @param gnis_name A character string of the gnis_name.
#' @param columns A vector of character strings of the columns to retain.
#' @return A sf object
#' @export
#' @examples
#' fwa_stream_gnis('Sangan River')
fwa_stream_gnis <- function(gnis_name,
                            columns = NULL,
                            bbox = NULL,
                            epsg = 4326){

  chk_string(gnis_name)
  collectionid <- "whse_basemapping.fwa_named_streams"
  filter <- list(gnis_name = gnis_name)

  fwapgr::fwa_collection(collection_id = collectionid,
                 properties = columns,
                 filter = filter,
                 bbox = bbox,
                 epsg = epsg)
}

