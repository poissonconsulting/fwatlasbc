test_that("fwa_collection_name", {
  chk::check_data(fwa_collection_name,
    values = list(collection_name = "", collection = ""),
    exclusive = TRUE, order = TRUE, key = "collection_name"
  )
  expect_snapshot_data(fwa_collection_name, "collection_name")
})

test_that("fwa_stream_name", {
  chk::check_data(fwa_stream_name,
    values = list(blk = 1L, stream_name = ""),
    exclusive = TRUE, order = TRUE, key = "blk"
  )
  expect_snapshot_data(fwa_stream_name, "stream_name")
})
