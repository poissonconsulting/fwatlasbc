vld_sfc_polygon <- function(x){
  vld_s3_class(x, "sfc_POLYGON") && length(x) == 1
}

# vld_sfc_polygon_null <- function(x){
#   if(is.null(x)) return(TRUE)
#   vld__sfc_polygon(x)
# }

vld_sfc_point <- function(x){
  vld_s3_class(x, "sfc_POINT") && length(x) == 1
}

vld_sf_sfc <- function(x){
  vld_s3_class(x, "sfc") || vld_s3_class(x, "sf")
}

# vld_sf_sfc_null <- function(x){
#   if(is.null(x)) return(TRUE)
#   vld_s3_class(x, "sfc") || vld_s3_class(x, "sf")
# }

# vld_watershed <- function(x){
#   if(is.null(x)) return(TRUE)
#   vld_polygon(x)
# }
