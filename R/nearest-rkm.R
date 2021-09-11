nearest_rkm <- function(x, rkm) {
  blk <- x$blue_line_key
  index <- sf::st_nearest_feature(x, rkm)
  x$blue_line_key <- rkm$blue_line_key[index]
  x$blue_line_key[is.na(x$blue_line_key)] <- blk[is.na(x$blue_line_key)]
  x$rkm <- rkm$rkm[index]
  x$distance_to_rkm <- sf::st_distance(x, rkm[index,], by_element = TRUE)
  x$distance_to_rkm <- as.numeric(x$distance_to_rkm)
  x
}

blue_line_key_nearest_rkm <- function(blue_line_key, x, rkm) {
  if(is.na(blue_line_key)) {
    x <- x[is.na(x$blue_line_key),]
  } else {
    x <- x[!is.na(x$blue_line_key) & x$blue_line_key == blue_line_key,]
    rkm <- rkm[rkm$blue_line_key == blue_line_key,]
  }
  nearest_rkm(x, rkm)
}

#' Nearest Rkm
#'
#' Assigns spatial points to river kilometre with matching blue_line_key.
#' If the blue_line_key is missing then it also assigns the blue_line_key.
#'
#' @param x An sf data frame of spatial points with optional blue_line_key column.
#' @param rkm An sf data frame of spatial point with blue_line_key and rkm columns.
#'
#' @return An updated version of x with column rkm and distance_to_rkm in m.
#' @family rkm
#' @export
#' @examples
#' \dontrun{
#' rkm <- fwa_rkm(blue_line_key = 356308001, interval = 1000)
#' fwa_nearest_rkm(rkm[2:4,], rkm[3:5,])
#' }
fwa_nearest_rkm <- function(x, rkm) {

  lifecycle::deprecate_soft("0.0.0.9004", "fwa_nearest_rkm()", "fwa_nearest_rm()")

  chk::chk_s3_class(x, "sf")
  chk::chk_s3_class(rkm, "sf")

  if(is.null(x$blue_line_key)) x$blue_line_key <- NA_integer_

  check_data(x, values = list(blue_line_key = c(1L, .Machine$integer.max, NA)))
  check_data(rkm, values = list(blue_line_key = c(1L, .Machine$integer.max),
                                rkm = 1))

  if(!nrow(x)) {
    x$rkm <- numeric(0)
    x$distance_to_rkm <- numeric(0)
    return(x)
  }
  if(!nrow(rkm)) {
    x$rkm <- NA_real_
    x$distance_to_rkm <- NA_real_
    return(x)
  }
  blue_line_keys <- unique(x$blue_line_key)
  if(length(blue_line_keys) == 1L) {
    if(!is.na(blue_line_keys))
      rkm <- rkm[rkm$blue_line_key == blue_line_keys,]
    return(nearest_rkm(x, rkm))
  }

  x$..fwatlasbc.id <- 1:nrow(x)
  x <- lapply(blue_line_keys, blue_line_key_nearest_rkm, x, rkm)
  x <- do.call("rbind", x)
  x <- x[order(x$..fwatlasbc.id),]
  x$..fwatlasbc.id <- NULL
  x
}
