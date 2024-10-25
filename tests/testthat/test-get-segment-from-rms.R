test_that("fwa_get_segment_from_rms sf", {
  x <- dplyr::tribble(
    ~blk, ~rm, ~parent_blk, ~parent_rm, ~x, ~y, ~popn,
    1, 0, NA_integer_, NA_integer_, 0, 0, "p1",
    1, 1, NA_integer_, NA_integer_, 1, 0, "p2",
    1, 2, NA_integer_, NA_integer_, 2, 0, "p2",
    1, 3, NA_integer_, NA_integer_, 3, 0, "p4",
    2, 0, 1, 0.5, 0.5, 0, "pa",
    2, 1, 1, 0.5, 0.5, 1, "pa",
    2, 2, 1, 0.5, 0.5, 2, "pc"
  ) |>
    sf::st_as_sf(coords = c("x", "y"), dim = "XY")

  y <- data.frame(blk = 1L, rm = c(1, 2, 4), section = c(3L, 7L, 10L))

  x <- fwa_add_section_to_rms(x, y)
  z <- fwa_get_segment_from_rms(x, "popn")

  expect_s3_class(z, "tbl")
  expect_s3_class(z, "sf")
  expect_identical(colnames(z), c("popn", "blk", "rm_start", "rm_end", "geometry"))
  expect_snapshot_data(z, "getsegmentsf")
})
