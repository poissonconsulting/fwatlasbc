nearest_rm <- function(x, rm) {
  blk <- x$blue_line_key
  index <- sf::st_nearest_feature(x, rm)
  x$blue_line_key <- rm$blue_line_key[index]
  x$blue_line_key[is.na(x$blue_line_key)] <- blk[is.na(x$blue_line_key)]
  x$rm <- rm$rm[index]
  x$distance_to_rm <- sf::st_distance(x, rm[index,], by_element = TRUE)
  x$distance_to_rm <- as.numeric(x$distance_to_rm)
  x
}

blue_line_key_nearest_rm <- function(blue_line_key, x, rm) {
  if(is.na(blue_line_key)) {
    x <- x[is.na(x$blue_line_key),]
  } else {
    x <- x[!is.na(x$blue_line_key) & x$blue_line_key == blue_line_key,]
    rm <- rm[rm$blue_line_key == blue_line_key,]
  }
  nearest_rm(x, rm)
}

#' Nearest River Metre
#'
#' Assigns spatial points to river metre with matching blue_line_key.
#' If the blue_line_key is missing then it also assigns the blue_line_key.
#'
#' @param x An sf data frame of spatial points with optional blue_line_key column.
#' @param rm An sf data frame of spatial point with blue_line_key and rm columns.
#'
#' @return An updated version of x with column rm and distance_to_rm in m.
#' @family rm
#' @export
#' @examples
#' \dontrun{
#' rm <- fwa_rm(blue_line_key = 356308001, interval = 1000)
#' fwa_nearest_rm(rm[2:4,], rm[3:5,])
#' }
fwa_nearest_rm <- function(x, rm) {
  chk::chk_s3_class(x, "sf")
  chk::chk_s3_class(rm, "sf")

  if(is.null(x$blue_line_key)) x$blue_line_key <- NA_integer_

  check_data(x, values = list(blue_line_key = c(1L, .Machine$integer.max, NA)))
  check_data(rm, values = list(blue_line_key = c(1L, .Machine$integer.max),
                               rm = 1L))

  if(!nrow(x)) {
    x$rm <- integer(0)
    x$distance_to_rm <- numeric(0)
    return(x)
  }
  if(!nrow(rm)) {
    x$rm <- NA_integer_
    x$distance_to_rm <- NA_real_
    return(x)
  }
  blue_line_keys <- unique(x$blue_line_key)
  if(length(blue_line_keys) == 1L) {
    if(!is.na(blue_line_keys))
      rm <- rm[rm$blue_line_key == blue_line_keys,]
    return(nearest_rm(x, rm))
  }

  x$..fwatlasbc.id <- 1:nrow(x)
  x <- lapply(blue_line_keys, blue_line_key_nearest_rm, x, rm)
  x <- do.call("rbind", x)
  x <- x[order(x$..fwatlasbc.id),]
  x$..fwatlasbc.id <- NULL
  x
}
