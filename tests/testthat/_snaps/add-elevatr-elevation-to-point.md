# fwa_add_elevatr_elevation_to_point errors after three timeouts

    Code
      fwa_add_elevatr_elevation_to_point(make_elevatr_points())
    Condition
      Error in `fwa_add_elevatr_elevation_to_point()`:
      ! Downloading the Terrain Tiles timed out after 3 attempts.
      i Try again later, reduce `zoom` or use `source = "google"`.
      Caused by error in `elevatr::get_elev_point()`:
      ! Timeout was reached [s3.amazonaws.com]

