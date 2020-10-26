named_streams <- fwapgr::fwa_collection("fwa_named_streams")

named_streams <- sf::st_set_geometry(named_streams, NULL)
named_streams <- tibble::as_tibble(named_streams)

usethis::use_data(named_streams, overwrite = TRUE)
