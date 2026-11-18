# ggplot3d 0.1.0

* Initial CRAN release.
* `coord_3d()` adds a third position scale (`scale_z_continuous()` and
  relatives) and a camera to ggplot2. Plain ggplot2 layers such as
  `geom_point()` and `geom_path()` work inside it.
* Every panel is one scene: points, paths, surfaces, solids, text and the
  panel box are sorted together with exact visibility, so layers intersect
  and occlude each other correctly. Output is vector (grid) or raster
  through `backend_3d()`.
* Layers for points, paths, surfaces, meshes, solids, volumes, voxels,
  rasters, simple features, intervals, labels and annotations, with
  statistics for density, smoothing, hulls, isosurfaces, binning,
  streamlines and parametric and implicit surfaces.
* Lighting and materials (`light_3d()`, `material_3d()`), shadows, depth
  cues, clipping planes, edges and outlines.
* Faceting by slices and views, curvilinear frames (cylindrical, spherical,
  globe, simplex, hyperbolic), animation (`animate_3d()`, camera paths),
  an interactive viewer (`view_3d()`) and export to mesh and vector formats
  (`export_3d()`).
* Mesh tools: reading, normals, smoothing, subdivision, decimation,
  remeshing, curvature and geodesics.
