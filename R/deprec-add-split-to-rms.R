#' @describeIn fwa_add_upstream_split_to_rms
#' Soft-deprecated `r lifecycle::badge('deprecated')`
fwa_add_split_to_rms <- function(x, y) {
  lifecycle::deprecate_soft("0.0.2", "fwa_add_split_to_rms()", "fwa_add_upstream_split_to_rms()")
  fwa_add_upstream_split_to_rms(x, y)
}
