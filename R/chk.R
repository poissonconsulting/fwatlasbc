chk_sf_sfc <- function(x){
  if(vld_sf_sfc(x)) {
    return(invisible())
  }
  chkor(chk_s3_class(x, "sfc"), chk_s3_class(x, "sf"))
}

# chk_sf_sfc_null <- function(x){
#   if(vld_sf_sfc_null(x)) {
#     return(invisible())
#   }
#   chkor(chk_s3_class(x, "sfc"), chk_s3_class(x, "sf"))
# }

chk_sfc_polygon <- function(x){
  if(vld_sfc_polygon(x)) {
    return(invisible())
  }
  chk_s3_class(x, "sfc_POLYGON")
  chk_length(x, 1L)
}

# chk_sfc_polygon_null <- function(x){
#   if(vld_sfc_polygon_null(x)) {
#     return(invisible())
#   }
#   chk_s3_class(x, "sfc_POLYGON")
#   chk_length(x, 1L)
# }

chk_sfc_point <- function(x){
  if(vld_sfc_point(x)) {
    return(invisible())
  }
  chk_s3_class(x, "sfc_POINT")
  chk_length(x, 1L)
}
