test_that("errors if data frame isn't active sf table", {
  test_stream <- sf::st_sfc(
    sf::st_multilinestring(
      c(sf::st_linestring(matrix(c(0, 0, 1, 1), ncol = 2, byrow = TRUE)),
        sf::st_linestring(matrix(c(2.1, 2.1, 3, 3), ncol = 2, byrow = TRUE)),
        sf::st_linestring(matrix(c(1.05, 1.05, 2, 2), ncol = 2, byrow = TRUE))
      )
    ),
    crs = 26911
  )

  df <- data.frame(
    name = "stream 1",
    blk = 1,
    geometry = test_stream
  )

  expect_chk_error(fwa_order_segments(df))
})

test_that("errors if not a dataframe is passed", {
  test_stream <- sf::st_sfc(
    sf::st_multilinestring(
      c(sf::st_linestring(matrix(c(0, 0, 1, 1), ncol = 2, byrow = TRUE)),
        sf::st_linestring(matrix(c(2.1, 2.1, 3, 3), ncol = 2, byrow = TRUE)),
        sf::st_linestring(matrix(c(1.05, 1.05, 2, 2), ncol = 2, byrow = TRUE))
      )
    ),
    crs = 26911
  )

  expect_chk_error(fwa_order_segments(test_stream))
})

test_that("returns empty dataframe if empty", {
  test_stream <- sf::st_sfc(
    sf::st_multilinestring(
      c(sf::st_linestring(matrix(c(0, 0, 1, 1), ncol = 2, byrow = TRUE)),
        sf::st_linestring(matrix(c(1.05, 1.05, 2, 2), ncol = 2, byrow = TRUE))
      )
    ),
    crs = 26911
  )

  df <- data.frame(
    name = "stream 1",
    blk = 1,
    geometry = test_stream
  ) |>
    sf::st_set_geometry("geometry")

  df <- df[0,]

  output <- fwa_order_segments(df)

  expect_identical(output, df)
})

test_that("orders a single multilinestring of points", {
  test_stream <- sf::st_sfc(
    sf::st_multilinestring(
      c(sf::st_linestring(matrix(c(0, 0, 1, 1), ncol = 2, byrow = TRUE)),
        sf::st_linestring(matrix(c(2.1, 2.1, 3, 3), ncol = 2, byrow = TRUE)),
        sf::st_linestring(matrix(c(1.05, 1.05, 2, 2), ncol = 2, byrow = TRUE))
      )
    ),
    crs = 26911
  )

  df <- data.frame(
    name = "stream 1",
    blk = 1,
    geometry = test_stream
  ) |>
    sf::st_set_geometry("geometry")

  output <- fwa_order_segments(df)

  expect_s3_class(output, "data.frame")
  expect_s3_class(output, "sf")
  expect_equal(colnames(output), c("name", "blk", "geometry"))
  expect_s3_class(output$geometry, "sfc_MULTILINESTRING")
  expect_equal(nrow(output), 1)

  segments <- sf::st_cast(output$geometry, "LINESTRING")
  expect_equal(
    segments[[1]],
    sf::st_linestring(matrix(c(0, 0, 1, 1), ncol = 2, byrow = TRUE))
  )
  expect_equal(
    segments[[2]],
    sf::st_linestring(matrix(c(1.05, 1.05, 2, 2), ncol = 2, byrow = TRUE))
  )
  expect_equal(
    segments[[3]],
    sf::st_linestring(matrix(c(2.1, 2.1, 3, 3), ncol = 2, byrow = TRUE))
  )
})

test_that("orders two rows of multilinestring of points", {
  test_stream_1 <- sf::st_sfc(
    sf::st_multilinestring(
      c(sf::st_linestring(matrix(c(2.1, 2.1, 3, 3), ncol = 2, byrow = TRUE)),
        sf::st_linestring(matrix(c(0, 0, 1, 1), ncol = 2, byrow = TRUE)),
        sf::st_linestring(matrix(c(1.05, 1.05, 2, 2), ncol = 2, byrow = TRUE))
      )
    ),
    crs = 26911
  )

  test_stream_2 <- sf::st_sfc(
    sf::st_multilinestring(
      c(
        sf::st_linestring(matrix(c(1.05, 0.6, 1.05, 2), ncol = 2, byrow = TRUE)), #1
        sf::st_linestring(matrix(c(0, 0, 1, 0.4), ncol = 2, byrow = TRUE)), #2
        sf::st_linestring(matrix(c(-1.5, 1, -1, 3), ncol = 2, byrow = TRUE)), #3
        sf::st_linestring(matrix(c(1.05, 2.10, -1.0,  1), ncol = 2, byrow = TRUE)) #4
      )
    ),
    crs = 26911
  )

  df <- data.frame(
    name = c("stream 1", "stream 2"),
    blk = c(1, 2),
    geometry = c(test_stream_1, test_stream_2)
  ) |>
    sf::st_set_geometry("geometry")

  output <- fwa_order_segments(df)

  expect_s3_class(output, "data.frame")
  expect_s3_class(output, "sf")
  expect_equal(colnames(output), c("name", "blk", "geometry"))
  expect_s3_class(output$geometry, "sfc_MULTILINESTRING")
  expect_equal(nrow(output), 2)

  segments_1 <- sf::st_cast(output[1,]$geometry, "LINESTRING")
  expect_equal(
    segments_1[[1]],
    sf::st_linestring(matrix(c(0, 0, 1, 1), ncol = 2, byrow = TRUE))
  )
  expect_equal(
    segments_1[[2]],
    sf::st_linestring(matrix(c(1.05, 1.05, 2, 2), ncol = 2, byrow = TRUE))
  )
  expect_equal(
    segments_1[[3]],
    sf::st_linestring(matrix(c(2.1, 2.1, 3, 3), ncol = 2, byrow = TRUE))
  )

  segments_2 <- sf::st_cast(output[2,]$geometry, "LINESTRING")
  expect_equal(
    segments_2[[1]],
    sf::st_linestring(matrix(c(0, 0, 1, 0.4), ncol = 2, byrow = TRUE))
  )
  expect_equal(
    segments_2[[2]],
    sf::st_linestring(matrix(c(1.05, 0.6, 1.05, 2), ncol = 2, byrow = TRUE))
  )
  expect_equal(
    segments_2[[3]],
    sf::st_linestring(matrix(c(1.05, 2.10, -1.0,  1), ncol = 2, byrow = TRUE))
  )
  expect_equal(
    segments_2[[4]],
    sf::st_linestring(matrix(c(-1.5, 1, -1, 3), ncol = 2, byrow = TRUE))
  )
})

test_that("table geometry column can be named something else", {
  test_stream <- sf::st_sfc(
    sf::st_multilinestring(
      c(sf::st_linestring(matrix(c(0, 0, 1, 1), ncol = 2, byrow = TRUE)),
        sf::st_linestring(matrix(c(2.1, 2.1, 3, 3), ncol = 2, byrow = TRUE)),
        sf::st_linestring(matrix(c(1.05, 1.05, 2, 2), ncol = 2, byrow = TRUE))
      )
    ),
    crs = 26911
  )

  df <- data.frame(
    name = "stream 1",
    blk = 1,
    geometry = test_stream
  ) |>
    sf::st_set_geometry("geometry") |>
    dplyr::rename(streams = geometry, geometry = blk)

  output <- fwa_order_segments(df)

  expect_s3_class(output, "data.frame")
  expect_s3_class(output, "sf")
  expect_equal(colnames(output), c("name", "geometry", "streams"))
  expect_s3_class(output$streams, "sfc_MULTILINESTRING")
  expect_equal(nrow(output), 1)

  segments <- sf::st_cast(output$streams, "LINESTRING")
  expect_equal(
    segments[[1]],
    sf::st_linestring(matrix(c(0, 0, 1, 1), ncol = 2, byrow = TRUE))
  )
  expect_equal(
    segments[[2]],
    sf::st_linestring(matrix(c(1.05, 1.05, 2, 2), ncol = 2, byrow = TRUE))
  )
  expect_equal(
    segments[[3]],
    sf::st_linestring(matrix(c(2.1, 2.1, 3, 3), ncol = 2, byrow = TRUE))
  )
})
