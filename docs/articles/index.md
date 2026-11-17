# Articles

### Building plots

- [Preparing data for 3D
  layers](https://gillescolling.com/ggplot3d/articles/data-preparation.md):

  Read coordinate tables, join attributes by identifier, order grouped
  paths and convert a height matrix to a checked surface grid.

- [Choosing layers for observations and
  grids](https://gillescolling.com/ggplot3d/articles/layers.md):

  Worked examples of points, paths, gridded surfaces, columns,
  distributions, intervals and reference geometry.

- [Scaling heights and facet
  ranges](https://gillescolling.com/ggplot3d/articles/z-scale.md):

  Transform heights, label categories and dates, and control limits and
  z ranges across facets.

- [Building surfaces and
  solids](https://gillescolling.com/ggplot3d/articles/surfaces-solids.md):

  Worked examples of indexed meshes, function and fitted surfaces,
  density surfaces, level surfaces, hulls, solid glyphs and sweeps along
  curves.

- [Positioning groups and
  observations](https://gillescolling.com/ggplot3d/articles/positions.md):

  Separate overlapping observations, align grouped columns and labels,
  and display stacked totals or proportions.

- [Displaying intervals and
  summaries](https://gillescolling.com/ggplot3d/articles/intervals.md):

  Plot supplied bounds on three axes and compute means, standard errors
  and quantile ranges for each cell of a grid.

- [Combining 2D and 3D
  graphics](https://gillescolling.com/ggplot3d/articles/combining-2d-3d.md):

  Place ggplot2 panels on walls, stack slices in depth, draw terraced
  contours and arrange 2D and 3D plots.

### Data types

- [Plotting angles and
  compositions](https://gillescolling.com/ggplot3d/articles/frames.md):

  Place data in cylindrical, spherical, globe, ternary prism and
  quaternary simplex coordinates instead of a box.

- [Plotting maps and spatial
  data](https://gillescolling.com/ggplot3d/articles/spatial.md):

  Draw raster terrain, simple features and XYZ geometry, prism maps,
  profiles, flow arcs and space-time paths.

- [Reading and measuring
  meshes](https://gillescolling.com/ggplot3d/articles/meshes.md):

  Read mesh files, smooth, subdivide and simplify triangle meshes,
  measure curvature, and draw geodesic distances, paths and log maps.

- [Reading 3D scans and point
  clouds](https://gillescolling.com/ggplot3d/articles/scans.md):

  Read textured meshes and point clouds from phone LiDAR apps,
  photogrammetry and airborne laser scanning, draw them, and rebuild a
  surface from points.

- [Drawing arrows and
  streamlines](https://gillescolling.com/ggplot3d/articles/vector-fields.md):

  Draw vector arrows, magnitude-scaled cones, integrated streamlines and
  solid streamtubes from a gridded field.

- [Slicing fields and point
  densities](https://gillescolling.com/ggplot3d/articles/field-slices.md):

  Inspect gridded values on axis-aligned and oblique planes, add
  contours, and compare bandwidths for slices of a point-cloud density.

- [Drawing networks and
  trees](https://gillescolling.com/ggplot3d/articles/networks.md):

  Draw igraph networks in three dimensions, multilayer networks on
  stacked planes, and dendrograms standing beside a heat map.

### Appearance

- [Choosing camera
  views](https://gillescolling.com/ggplot3d/articles/camera.md):

  Set camera angles, projection, framing and stereo output for static
  scenes.

- [Styling axes, legends and
  panes](https://gillescolling.com/ggplot3d/articles/guides-themes.md):

  Choose axis labels, align a colourbar with height, and style the panes
  and grids of a three-dimensional plot.

- [Lighting and styling
  surfaces](https://gillescolling.com/ggplot3d/articles/lighting.md):

  Control surface shading, use lighting presets and materials, and show
  the shading range in legends.

### Output and sharing

- [Choosing rendering and image
  formats](https://gillescolling.com/ggplot3d/articles/backends.md):

  Choose vector, raster, hybrid or traced rendering and save figures at
  the intended resolution.

- [Rendering volumes and traced
  surfaces](https://gillescolling.com/ggplot3d/articles/ray-marching.md):

  Trace signed distance surfaces, integrate scalar volumes and
  accumulate seeded path-traced images.

- [Rendering materials with path
  tracing](https://gillescolling.com/ggplot3d/articles/path-tracing.md):

  Render rough metals, glass, image lighting and participating media
  with the native path tracer.

- [Embedding plots in
  reports](https://gillescolling.com/ggplot3d/articles/documents.md):

  Use knitr and Quarto to choose HTML viewers or static figures, supply
  alt text and compose panels.

- [Viewing plots
  interactively](https://gillescolling.com/ggplot3d/articles/viewer.md):

  Explore WebGL 2 scenes with tooltips, picking, selection, linked
  cameras and crosstalk filters.

- [Animating cameras and
  data](https://gillescolling.com/ggplot3d/articles/motion.md):

  Render camera moves and camera paths, tween points and surfaces
  between data states, and hand the scene to the interactive viewer.

- [Exporting plots and
  scenes](https://gillescolling.com/ggplot3d/articles/export.md):

  Write model files, animated glTF, semantic SVG, TikZ and Asymptote, or
  pass scenes to rgl and rayrender.

### Scientific examples

- [Drawing common charts in
  3D](https://gillescolling.com/ggplot3d/articles/gallery.md):

  Worked plots of solid curves, distributions, covariance ellipsoids,
  streamtubes, cutaways and contour terraces alongside familiar charts.

- [Drawing ordination
  biplots](https://gillescolling.com/ggplot3d/articles/ordination.md):

  Draw site scores, variable arrows and grouped regions from
  principal-component and ecological ordinations.

- [Drawing tensor
  glyphs](https://gillescolling.com/ggplot3d/articles/tensors.md):

  Map symmetric positive semidefinite matrices to oriented ellipsoid or
  superquadric glyphs.

- [Drawing molecular
  structures](https://gillescolling.com/ggplot3d/articles/molecules.md):

  Read molecular coordinates, attach bonds by atom identifier and draw
  atoms and bonds in three dimensions.

- [Plotting colour gamuts in Lab
  coordinates](https://gillescolling.com/ggplot3d/articles/colour-gamuts.md):

  Sample sRGB colours in Lab coordinates and display their convex hull
  with interpolated vertex colours.

### Mathematical examples

- [Constructing and reconstructing
  surfaces](https://gillescolling.com/ggplot3d/articles/implicit-surfaces.md):

  Draw level sets from functions, reconstruct surfaces from point clouds
  and triangulate scattered height measurements.

- [Animating differential-equation
  solutions](https://gillescolling.com/ggplot3d/articles/pde.md):

  Exact solutions of eight partial differential equations and a
  numerical magnetohydrodynamic solution drawn as surfaces that change
  over time with animate_3d().

- [Projecting four dimensions and drawing
  tilings](https://gillescolling.com/ggplot3d/articles/higher-dimensions.md):

  Project and rotate four-dimensional data, cut 4D meshes with a
  hyperplane, run a grand tour, and draw regular tilings and honeycombs.

- [Plotting curved spaces and
  spacetime](https://gillescolling.com/ggplot3d/articles/curved-spaces.md):

  Draw data in spherical and hyperbolic geometry, the Hopf fibration,
  hyperbolic tree layouts, light cones, Lorentz boosts and Schwarzschild
  geodesics.

- [Drawing complex functions, knots and
  surfaces](https://gillescolling.com/ggplot3d/articles/complex-knots.md):

  Domain colouring on the Riemann sphere, Riemann surfaces of inverse
  functions, Möbius maps, torus knots and immersed surfaces with their
  self-intersections.

- [Viewing Thurston geometries from
  inside](https://gillescolling.com/ggplot3d/articles/thurston-geometries.md):

  Render an observer view in each of the eight Thurston geometries using
  intrinsic balls and geodesics.

### Internals and extensions

- [How overlapping geometry is
  drawn](https://gillescolling.com/ggplot3d/articles/visibility.md):

  Explain how overlapping faces, lines and marks are ordered and clipped
  within a three-dimensional panel.

- [Writing a geometry
  extension](https://gillescolling.com/ggplot3d/articles/extending.md):

  Write a new geom for coord_3d(): the scene packet it emits, how
  positions are normalised, and how register_geom_3d() connects it to
  the scene.
