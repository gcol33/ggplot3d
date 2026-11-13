# ggplot3d 0.0.0.9000

* Draw points, paths, segments, text, polygons, surfaces, contours, meshes, bands, columns and extrusions in three dimensions.
* Train continuous, discrete and date/time z scales, with fixed or free z ranges across facets.
* Control camera angles, projection, lighting and framing with `coord_3d()`.
* Panel frames: `coord_cylindrical_3d()` and `coord_spherical_3d()` bend paths and faces along angle, radius and height, with rear wall or shell panes and curved axes; `coord_ternary_3d()` and `coord_simplex_3d()` place three- or four-part compositions (aesthetics `a`, `b`, `c`, `d`) in a triangular prism or a regular tetrahedron.
* `coord_3d(transform = )` applies coordinate transformations after statistics, with expansion in transformed space.
* `stat_density_3d()` and `stat_hull_3d()` gain `space = "ilr"` for compositions.
* `camera_3d()` returns named views (isometric, dimetric, trimetric, top, front, right, and the oblique cabinet, cavalier and military views); `coord_3d()` gains `projection = "oblique"`, `oblique` and `up`.
* Resolve visibility across layers within each panel for vector and raster output.
* Compute function surfaces, fitted surfaces, density surfaces, ridgelines, convex hulls and scalar-field level surfaces.
* Animate data that change over time with `animate_3d(time = )`: one frame per value of a column, with scales trained on all frames.
* `geom_blank()` and `expand_limits()` train scales inside `coord_3d()`.
