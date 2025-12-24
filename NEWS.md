<!-- NEWS.md is maintained by https://fledge.cynkra.com, contributors should not edit this file -->

# fwatlasbc 0.0.1.9020

- Adding `fwa_stitch_segments()` creates a new segment to join disconnected segments.
- Adding `fwa_order_segments()` orders segments within a stream based on the start and end point of each segment.
- Skipping failing tests and reported bug (#56)[https://github.com/poissonconsulting/fwatlasbc/issues/96]

# fwatlasbc 0.0.1.9019

- Skipped failing tests on R-universe.
- Added action to message slack channel with weekly R-CMD-check results.
- Added `fwa_add_point_to_stream_measure()`.
- Added `fwa_get_segment_from_rms()` to get line geometry for cuts or sections.

# fwatlasbc 0.0.1.9018

- Added `fwa_convert_stream_names_to_blks()`.

# fwatlasbc 0.0.1.9017

- Added `reverse` argument to allow streams to be reversed.
- `fwa_convert_streams_to_rms()` now buffers to deal with inaccuracies in streams network.
- Fixed `fwa_convert_streams_to_rms()` getting wrong half of split!

# fwatlasbc 0.0.1.9016

- Added `snap_mouths = FALSE` argument to `fwa_snap_rm_to_rms()`.

# fwatlasbc 0.0.1.9015

- Added `fwa_add_new_blk_rm_to_blk_rm()`, `fwa_join_stream_segments()`, `fwa_convert_streams_to_rms()`, `fwa_mapview_rms_to_rms()`, `fwa_mapview_rms_to_rms()`, and `fwa_snap_rms_to_rms()`.
- `fwa_snap_rm_to_point()` now retains blk if there is no match.
- Updated stream names.
- Added `nocache = getOption("fwa.nocache", FALSE)` argument.
- Replaced .`data$name` with `name` in select statements.
- `fwa_add_watershed_to_blk()` now provides general error if watershed is undefined for blk at rm.

- Collections no longer include `upstream_area_ha`.

- `distance_to_lon_lat` now rounded to mm.

# fwatlasbc 0.0.1.9014

- Added `fwa_convert_rms_to_streams()` to convert rms sf point object into sf linestring object.
- Added `fwa_prune_rms()` to prune tips.
- Added `fwa_add_cut_rm()`.
- `fwa_add_section_to_rms()` now preserves geometry.
- `fwa_add_section_to_rms()` now preserves sf.

# fwatlasbc 0.0.1.9013

- Added `fwa_add_downstream_split_to_rms()`.
- Soft-deprecated `fwa_add_split_to_rms()` for `fwa_add_upstream_split_to_rms()`.

# fwatlasbc 0.0.1.9012

- Preserved point characteristics when adjusting with `fwa_swap_branches()`.

# fwatlasbc 0.0.1.9011

- Added `fwa_mapview()`.
- `fwa_convert_stream_network_to_rms()` now excludes channels with a stream order of 1 from being parents.
- Added `...` argument to `fwa_snap_rm_to_point()` to allow assignment by additional columns.

# fwatlasbc 0.0.1.9010

- Switched to new `fwapgr` endpoint.

# fwatlasbc 0.0.1.9009

- Fixed internal `group_split()` so that active geometry is preserved.

# fwatlasbc 0.0.1.9008

- Added `swap_branches_rms()`, `fwa_parent_stream_name_rms()`, `fwa_parent_blk_rms()`, `fwa_mapview_rms()`, and `fwa_swap_branches_rms()`.
- Added `stream_name = fwatlasbc:fwa_stream_name` argument to `fwa_add_blks_to_stream_name()`.

# fwatlasbc 0.0.1.9007

- Added `fwa_add_intersection_to_geometry()` and `fwa_add_gm_elevation_to_point()`.
- Added `chunk_size` and `digits` arguments to `fwa_add_gm_elevation_to_point()`.

# fwatlasbc 0.0.1.9006

- Renamed `fwa_add_rm_to_lon_lat()` to `fwa_add_blk_to_lon_lat()`.

# fwatlasbc 0.0.1.9005

- Soft-deprecated `fwa_add_collection_to_watershed()` for `fwa_add_collection_to_polygon()`.
- Changed behaviour of `fwa_add_collection_to_watershed()` so that it copies blue_line_key to blk if present.
- Added `fwa_add_split_to_rms()`.

# fwatlasbc 0.0.1.9004

- Added `fwa_convert_stream_network_to_rms()`.
- Renamed `fwa_add_section_to_rm()` to `fwa_add_section_to_rms()`

# fwatlasbc 0.0.1.9002

- Added `fwa_add_rm_to_lon_lat()` and `fwa_add_stream_name_to_blk()`.
- Exported `fwa_stream_name` data.
- Moved `rm` argument in `fwa_add_watershed_to_blk()` to column in argument x.

# fwatlasbc 0.0.1.9001

- Switched column names to snake case.
- In `fwa_add_rms_to_blk()`, removed z-coordinate from returned geometry column and added as a numeric elevation column.

# fwatlasbc 0.0.1

- Added a `NEWS.md` file to track changes to the package.
