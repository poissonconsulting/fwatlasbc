test_that("fwa_collection_names", {
  chk::check_data(fwa_collection_names,
                  values = list(collection_name = "", collection = ""),
                  exclusive = TRUE, order = TRUE, key = "collection_name")
  expect_snapshot_data(fwa_collection_names, "collection_names")
})
