# Changelog

## ggplot3d 0.1.0

- Initial CRAN release.
- [`coord_3d()`](https://gillescolling.com/ggplot3d/reference/coord_3d.md)
  adds a third position scale
  ([`scale_z_continuous()`](https://gillescolling.com/ggplot3d/reference/scale_z_continuous.md)
  and relatives) and a camera to ggplot2. Plain ggplot2 layers such as
  [`geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.html)
  and
  [`geom_path()`](https://ggplot2.tidyverse.org/reference/geom_path.html)
  work inside it.
- Every panel is one scene: points, paths, surfaces, solids, text and
  the panel box are sorted together with exact visibility, so layers
  intersect and occlude each other correctly. Output is vector (grid) or
  raster through
  [`backend_3d()`](https://gillescolling.com/ggplot3d/reference/backend_3d.md).
- Layers for points, paths, surfaces, meshes, solids, volumes, voxels,
  rasters, simple features, intervals, labels and annotations, with
  statistics for density, smoothing, hulls, isosurfaces, binning,
  streamlines and parametric and implicit surfaces.
- Lighting and materials
  ([`light_3d()`](https://gillescolling.com/ggplot3d/reference/light_3d.md),
  [`material_3d()`](https://gillescolling.com/ggplot3d/reference/material_3d.md)),
  shadows, depth cues, clipping planes, edges and outlines.
- Faceting by slices and views, curvilinear frames (cylindrical,
  spherical, globe, simplex, hyperbolic), animation
  ([`animate_3d()`](https://gillescolling.com/ggplot3d/reference/animate_3d.md),
  camera paths), an interactive viewer
  ([`view_3d()`](https://gillescolling.com/ggplot3d/reference/view_3d.md))
  and export to mesh and vector formats
  ([`export_3d()`](https://gillescolling.com/ggplot3d/reference/export_3d.md)).
- Mesh tools: reading, normals, smoothing, subdivision, decimation,
  remeshing, curvature and geodesics.
