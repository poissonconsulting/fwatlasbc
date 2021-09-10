chk_sf_sfc <- function(x, x_name = NULL){
  if(vld_sf_sfc(x)) {
    return(invisible(x))
  }
  if (is.null(x_name)) {
    x_name <- deparse_backtick_chk(substitute(x))
  }
  if(!vld_s3_class(x, "sfc") && !vld_s3_class(x, "sf")) {
    chkor_vld(vld_s3_class(x, "sfc"), vld_s3_class(x, "sf"), x_name = x_name)
  }
}


# chk_sf_sfc_null <- function(x){
#   if(vld_sf_sfc_null(x)) {
#     return(invisible())
#   }
# if(!vld_s3_class(x, "sfc") && !vld_s3_class(x, "sf")) {
#   chkor_vld(vld_s3_class(x, "sfc"), vld_s3_class(x, "sf"), x_name = x_name)
# }
# }

chk_sfc_polygon <- function(x, x_name = NULL){
  if(vld_sfc_polygon(x)) {
    return(invisible(x))
  }
  if (is.null(x_name)) {
    x_name <- deparse_backtick_chk(substitute(x))
  }
  chk_s3_class(x, "sfc_POLYGON", x_name = x_name)
  chk_length(x, 1L, x_name = x_name)
}

# chk_sfc_polygon_null <- function(x){
#   if(vld_sfc_polygon_null(x)) {
#     return(invisible())
#   }
#   chk_s3_class(x, "sfc_POLYGON")
#   chk_length(x, 1L)
# }

chk_sfc_point <- function(x, x_name = NULL){
  if(vld_sfc_point(x)) {
    return(invisible(x))
  }
  if (is.null(x_name)) {
    x_name <- deparse_backtick_chk(substitute(x))
  }
  chk_s3_class(x, "sfc_POINT", x_name = x_name)
  chk_length(x, 1L, x_name = x_name)
}
