#' Add elevatr Elevation to Point
#'
#' Adds elevations from the Amazon Web Services Terrain Tiles using
#' [`elevatr::get_elev_point()`].
#' Unlike [`fwa_add_gm_elevation_to_point()`] no API key is required.
#'
#' The connection timeout is increased to 60 seconds and the download is
#' attempted up to three times if it times out.
#'
#' @param x An sf object of spatial points.
#' @param zoom A whole number between 1 and 14 of the zoom level of the
#' Terrain Tiles.
#' Higher zoom levels have a finer resolution but take longer to download.
#' @return An updated version of x with numeric column elevation.
#' @seealso [`elevatr::get_elev_point()`] and
#' [`fwa_add_gm_elevation_to_point()`]
#' @export
#' @examples
#' \dontrun{
#' rm <- fwa_add_rms_to_blk(data.frame(blk = 356308001))
#' fwa_add_elevatr_elevation_to_point(rm)
#' }
fwa_add_elevatr_elevation_to_point <- function(x, zoom = 12) {
  # elevatr uses progress at runtime but only suggests it
  rlang::check_installed(
    c("elevatr", "httr", "progress"),
    reason = "to get elevations from the Terrain Tiles."
  )

  chk_s3_class(x, "sf")
  chk_s3_class(sf::st_geometry(x), "sfc_POINT")
  chk_whole_number(zoom)
  chk_range(zoom, c(1, 14))

  if (!nrow(x)) {
    x$elevation <- numeric(0)
    return(x)
  }

  points <- sf::st_sf(geometry = sf::st_geometry(x))

  elevation <- get_elevatr_elevation(points, zoom, call = rlang::current_env())

  x$elevation <- as.numeric(elevation)
  x
}

is_timeout <- function(x) {
  grepl(
    "timeout was reached|timed out",
    conditionMessage(x),
    ignore.case = TRUE
  )
}

wait_seconds <- function(seconds) {
  Sys.sleep(seconds)
}

get_elevatr_elevation <- function(points, zoom, call, tries = 3L) {
  # httr's default connection timeout is 10 seconds.
  # with_config() covers the tile checks in elevatr that do not pass on `...`
  # while `config` covers the downloads in parallel workers.
  config <- httr::config(connecttimeout = 60)
  for (try in seq_len(tries)) {
    elevation <- tryCatch(
      httr::with_config(
        config,
        suppressMessages(
          elevatr::get_elev_point(
            points,
            src = "aws",
            z = zoom,
            config = config
          )
        )
      )$elevation,
      error = identity
    )
    if (!inherits(elevation, "error")) {
      return(elevation)
    }
    if (!is_timeout(elevation)) {
      rlang::cnd_signal(elevation)
    }
    if (try < tries) {
      wait_seconds(2^try)
    }
  }
  rlang::abort(
    c(
      paste0(
        "Downloading the Terrain Tiles timed out after ",
        tries,
        " attempts."
      ),
      i = "Try again later, reduce `zoom` or use `source = \"google\"`."
    ),
    parent = elevation,
    call = call
  )
}

add_elevation_to_point <- function(x, source, zoom) {
  if (source == "elevatr") {
    return(fwa_add_elevatr_elevation_to_point(x, zoom = zoom))
  }
  fwa_add_gm_elevation_to_point(x)
}

check_source <- function(
  source,
  fun,
  warn = TRUE,
  user_env = rlang::caller_env(2)
) {
  if (is.null(source)) {
    if (warn) {
      lifecycle::deprecate_soft(
        "0.0.2",
        I(paste0("Calling `", fun, "()` without specifying `source`")),
        details = paste0(
          "The default `source = \"google\"` is being used. ",
          "Please set `source = \"google\"` or `source = \"elevatr\"`."
        ),
        user_env = user_env
      )
    }
    source <- "google"
  }
  chk_string(source)
  chk_subset(source, c("elevatr", "google"))
  source
}
