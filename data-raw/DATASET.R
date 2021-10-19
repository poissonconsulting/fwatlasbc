library(dplyr)
library(readr)

fwa_collection_names <- read_csv("data-raw/fwa-collection-names.csv")

fwa_collection_names <- arrange(fwa_collection_names, CollectionName)

chk::check_key(fwa_collection_names, "CollectionName")

usethis::use_data(fwa_collection_names, overwrite = TRUE)

coll_id <- "whse_basemapping.fwa_named_streams"

# first 10,000
named_streams1 <- coll_id |>
  fwapgr::fwa_query_collection() |>
  as_tibble() |>
  select(blue_line_key, gnis_name)

# 10,001 - 20,000
named_streams2 <- coll_id |>
  fwapgr::fwa_query_collection(offset = 9999) |>
  slice(-1) |>
  as_tibble() |>
  select(blue_line_key, gnis_name)

named_streams <- named_streams1 |>
  bind_rows(named_streams2) |>
  distinct() |>
  rename(BLK = blue_line_key, StreamName = gnis_name)

usethis::use_data(named_streams, overwrite = TRUE, internal = TRUE)
