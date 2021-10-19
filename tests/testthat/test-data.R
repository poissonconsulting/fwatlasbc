test_that("fwa_collection_names", {
  chk::check_data(fwa_collection_names,
                  values = list(CollectionName = "", Collection = ""),
                  exclusive = TRUE, order = TRUE, key = "CollectionName")
  expect_snapshot_data(fwa_collection_names)
})
