test_that("fwa_add_intersection_to_point works simple case", {
  rm <- fwa_add_rms_to_blk(data.frame(blk = 356308001))
  y <- sf::st_cast(rm[rm$rm == 15000,])
  y$name <- "ne"

  x <- fwa_add_intersection_to_geometry(rm, y)
  expect_s3_class(x, "sf")
  expect_identical(colnames(x), c("blk", "rm", "elevation", "geometry", "ne"))
  expect_identical(x$ne, c(rep(FALSE, 15), TRUE, rep(FALSE, 4)))
})
