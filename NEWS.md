<!-- NEWS.md is maintained by https://cynkra.github.io/fledge, do not edit -->

# fwatlasbc 0.0.1.9009

- Fixed internal `group_split()` so that active geometry preserved.


# fwatlasbc 0.0.1.9008

- Added `swap_branches_rms()`.
- Added `fwa_parent_stream_name_rms()`.
- Added `fwa_parent_blk_rms()`.
- Added `stream_name = fwatlasbc:fwa_stream_name` argument to `fwa_add_blks_to_stream_name()`.
- Added `fwa_mapview_rms()`.
- Added `fwa_swap_branches_rms()`.


# fwatlasbc 0.0.1.9007

- Added `fwa_add_intersection_to_geometry()`.
- Added `chunk_size` and `digits` arguments to `fwa_add_gm_elevation_to_point()`.
- Added `fwa_add_gm_elevation_to_point()`.


# fwatlasbc 0.0.1.9006

- Renamed `fwa_add_rm_to_lon_lat()` to `fwa_add_blk_to_lon_lat()`.


# fwatlasbc 0.0.1.9005

- Soft-deprecated `fwa_add_collection_to_watershed()` for `fwa_add_collection_to_polygon()`.
- Change behaviour of `fwa_add_collection_to_watershed()` so copies blue_line_key to blk if present.
- Added `fwa_add_split_to_rms()`.


# fwatlasbc 0.0.1.9004

- Added `fwa_convert_stream_network_to_rms()`.
- Renamed `fwa_add_section_to_rm()` to `fwa_add_section_to_rms()`


# fwatlasbc 0.0.1.9002

- Added `fwa_add_rm_to_lon_lat()`.
- Added `fwa_add_stream_name_to_blk()`.
- Exported `fwa_stream_name` data.
- Moved `rm` argument in `fwa_add_watershed_to_blk()` to column in argument x.


# fwatlasbc 0.0.1.9001

- Switched column names to snake case.
- In `fwa_add_rms_to_blk()` removed Z coordinate from returned geometry column and added as a numeric elevation column.


# fwatlasbc 0.0.1.9000

- Same as previous version.


# fwatlasbc 0.0.1

* Added a `NEWS.md` file to track changes to the package.
