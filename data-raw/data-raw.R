library(dplyr)
library(readr)

fwa_collection_name <- read_csv("data-raw/fwa-collection-name.csv")

fwa_collection_name <- arrange(fwa_collection_name, collection_name)

chk::check_key(fwa_collection_name, "collection_name")

usethis::use_data(fwa_collection_name, overwrite = TRUE)

coll_id <- "whse_basemapping.fwa_named_streams"

# first 10,000
fwa_stream_name <- coll_id |>
  fwapgr::fwa_query_collection(nocache = TRUE) |>
  as_tibble() |>
  select(blue_line_key, gnis_name)

# 10,001 - 20,000
fwa_stream_name2 <- coll_id |>
  fwapgr::fwa_query_collection(offset = 9999, nocache = TRUE) |>
  slice(-1) |>
  as_tibble() |>
  select(blue_line_key, gnis_name)

fwa_stream_name <- fwa_stream_name |>
  bind_rows(fwa_stream_name2) |>
  distinct() |>
  rename(blk = blue_line_key, stream_name = gnis_name) |>
  mutate(blk = as.integer(blk))

chk::check_data(fwa_stream_name,
                values = list(blk = 1L, stream_name = ""),
                exclusive = TRUE, order = TRUE, key = "blk")

usethis::use_data(fwa_stream_name, overwrite = TRUE)
