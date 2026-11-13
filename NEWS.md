# ggplot3d 0.0.0.9000

* Draw points, paths, segments, text, polygons, surfaces, contours, meshes, bands, columns and extrusions in three dimensions.
* Train continuous, discrete and date/time z scales, with fixed or free z ranges across facets.
* Control camera angles, projection, lighting and framing with `coord_3d()`.
* Panel frames: `coord_cylindrical_3d()` and `coord_spherical_3d()` bend paths and faces along angle, radius and height, with rear wall or shell panes and curved axes; `coord_ternary_3d()` and `coord_simplex_3d()` place three- or four-part compositions (aesthetics `a`, `b`, `c`, `d`) in a triangular prism or a regular tetrahedron.
* `coord_3d(transform = )` applies coordinate transformations after statistics, with expansion in transformed space.
* `stat_density_3d()` and `stat_hull_3d()` gain `space = "ilr"` for compositions.
* `camera_3d()` returns named views (isometric, dimetric, trimetric, top, front, right, and the oblique cabinet, cavalier and military views); `coord_3d()` gains `projection = "oblique"`, `oblique` and `up`.
* Resolve visibility across layers within each panel for vector and raster output.
* Large scenes compile faster: a 200,000-face surface takes about 2 s instead of 7 s. The occlusion graph uses several threads, set with `options(ggplot3d.threads = )`; the result is the same for every thread count. Redrawing a plot with the same panel size and view reuses the compiled scene (`options(ggplot3d.cache_bytes = )`).
* Compute function surfaces, fitted surfaces, density surfaces, ridgelines, convex hulls and scalar-field level surfaces.
* Animate data that change over time with `animate_3d(time = )`: one frame per value of a column, with scales trained on all frames.
* `geom_blank()` and `expand_limits()` train scales inside `coord_3d()`.
* The raster backend renders 64-pixel tiles on several threads (option `ggplot3d.threads`) with one fragment list per sample, keeps the exact compositing order and can return depth, id and normal buffers.
* The raster backend composites translucent paint and averages antialiasing samples in linear light; `backend_3d(blend = "srgb")` matches vector devices.
* `geom_point_3d(render = "splat")` draws large point clouds in the raster backend as depth-tested spheres with eye-dome lighting (`edl`); the vector backend stops above `getOption("ggplot3d.vector_markers", 50000)` markers per panel.
* `backend_3d("hybrid")` rasterises the layers drawn with `raster = TRUE` and keeps the other layers, axes, panes and text as vector graphics, clipped where the image's opaque coverage lies in front of them.
* Translucent surfaces in vector output cover the shared edges inside each group of non-overlapping faces once.
* `geom_label()`, `geom_rect()`, `geom_tile()`, `geom_hline()`, `geom_vline()`, `annotation_custom()` and `annotation_raster()` work inside `coord_3d()`.
* Position adjustments in three dimensions: `position_jitter_3d()` jitters x, y and z; `position_dodge_3d()` places groups side by side along x or y within each x/y cell; `position_stack_3d()` and the new `position_fill_3d()` stack along z, x or y, for columns, walls and bands, points and text. `register_geom_3d()` gains `positions` and `extents`, so each geometry declares the adjustments it accepts.
* `annotate_3d()` adds fixed annotations with z positions.
* `stat_density_2d_3d(plane = )` draws the density flat on the floor or at a given height, filled by density.
* The camera arguments of `coord_3d()` take one value per panel, so facets can show different views; `animate_3d()` keeps each panel's offset.
* `labs(x = NULL)` and friends remove a 3D axis title.
* Discrete axes keep every label unless `check.overlap = TRUE`; tick labels are thinned by their measured rectangles, `n.dodge` rows stack directly outside one another, and axis titles sit next to the tick labels.
