# fwa_add_elevatr_elevation_to_point errors after three timeouts

    Code
      fwa_add_elevatr_elevation_to_point(make_elevatr_points())
    Condition
      Error in `fwa_add_elevatr_elevation_to_point()`:
      ! Downloading the Terrain Tiles timed out after 3 attempt(s) with a connection timeout of 10 seconds.
      i Increase the timeout or number of attempts using the options `fwa.elevatr_timeout` and `fwa.elevatr_tries`, e.g., `options(fwa.elevatr_timeout = 20, fwa.elevatr_tries = 5)`.
      i Alternatively try again later, reduce `zoom` or use `source = "google"`.
      Caused by error in `elevatr::get_elev_point()`:
      ! Timeout was reached [s3.amazonaws.com]

# fwa_add_elevatr_elevation_to_point respects tries option

    Code
      fwa_add_elevatr_elevation_to_point(make_elevatr_points())
    Condition
      Error in `fwa_add_elevatr_elevation_to_point()`:
      ! Downloading the Terrain Tiles timed out after 1 attempt(s) with a connection timeout of 30 seconds.
      i Increase the timeout or number of attempts using the options `fwa.elevatr_timeout` and `fwa.elevatr_tries`, e.g., `options(fwa.elevatr_timeout = 60, fwa.elevatr_tries = 3)`.
      i Alternatively try again later, reduce `zoom` or use `source = "google"`.
      Caused by error in `elevatr::get_elev_point()`:
      ! Timeout was reached [s3.amazonaws.com]

# fwa_add_elevatr_elevation_to_point errors with invalid options

    Code
      fwa_add_elevatr_elevation_to_point(x)
    Condition
      Error in `fwa_add_elevatr_elevation_to_point()`:
      ! `getOption("fwa.elevatr_timeout")` must be greater than 0, not 0.

---

    Code
      fwa_add_elevatr_elevation_to_point(x)
    Condition
      Error in `fwa_add_elevatr_elevation_to_point()`:
      ! `getOption("fwa.elevatr_tries")` must be greater than or equal to 1, not 0.

---

    Code
      fwa_add_elevatr_elevation_to_point(x)
    Condition
      Error in `fwa_add_elevatr_elevation_to_point()`:
      ! `getOption("fwa.elevatr_tries")` must be a whole number (non-missing integer scalar or double equivalent).

