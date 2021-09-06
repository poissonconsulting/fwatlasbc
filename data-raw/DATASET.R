library(dplyr)

named_streams <- fwapgr::fwa_collection("whse_basemapping.fwa_named_streams")

named_streams <- as_tibble(named_streams)
named_streams$geometry <- NULL

named_streams <- named_streams |>
  select(blue_line_key, gnis_name) |>
  distinct()

usethis::use_data(named_streams, overwrite = TRUE, internal = TRUE)
