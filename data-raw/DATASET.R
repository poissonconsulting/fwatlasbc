named_streams <- fwapgr::fwa_collection("whse_basemapping.fwa_named_streams")

named_streams <- tibble::as_tibble(named_streams)
named_streams$geometry <- NULL

chk::check_key(named_streams, c("blue_line_key", "gnis_name"))

usethis::use_data(named_streams, overwrite = TRUE, internal = TRUE)
