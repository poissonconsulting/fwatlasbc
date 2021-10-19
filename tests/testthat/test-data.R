test_that("fwa_collection_name", {
  chk::check_data(fwa_collection_name,
                  values = list(collection_name = "", collection = ""),
                  exclusive = TRUE, order = TRUE, key = "collection_name")
  expect_snapshot_data(fwa_collection_name, "collection_name")
})
