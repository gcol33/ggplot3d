# Draw an external triangle or polygon mesh

Draw an external triangle or polygon mesh

## Usage

``` r
geom_mesh_3d(
  mapping = NULL,
  data = NULL,
  stat = StatMesh3D,
  position = "identity",
  ...,
  vertices = NULL,
  faces = NULL,
  shade = TRUE,
  wireframe = FALSE,
  hidden = "hide",
  subdivide = 0L,
  remesh = NULL,
  decimate = NULL,
  smooth = 0L,
  curvature = FALSE,
  crease = NULL,
  outline = NULL,
  alpha_mode = "face",
  shading = "flat",
  material = NULL,
  hatch = NULL,
  texture = NULL,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
```

## Arguments

- mapping, data, stat, position, ..., na.rm, show.legend, inherit.aes:

  Standard layer arguments.

- vertices, faces:

  Optional vertex data frame and face index matrix, passed to mesh_3d().
  With faces supplied, data may hold the vertices.

- shade:

  Whether to apply lighting.

- wireframe:

  Draw original mesh edges with shared edges deduplicated.

- hidden:

  Hidden stroke policy.

- subdivide, smooth:

  Rounds of
  [`mesh_subdivide()`](https://gillescolling.com/ggplot3d/reference/mesh_processing.md)
  and iterations of
  [`mesh_smooth()`](https://gillescolling.com/ggplot3d/reference/mesh_processing.md).

- remesh:

  Target edge length for
  [`mesh_remesh()`](https://gillescolling.com/ggplot3d/reference/mesh_processing.md),
  or TRUE for the mean edge length.

- decimate:

  Target face count or fraction for
  [`mesh_decimate()`](https://gillescolling.com/ggplot3d/reference/mesh_processing.md).

- curvature:

  Whether to add the computed variables of
  [`mesh_curvature()`](https://gillescolling.com/ggplot3d/reference/mesh_processing.md),
  for use with
  [`after_stat()`](https://ggplot2.tidyverse.org/reference/aes_eval.html).

- crease:

  Crease angle for
  [`mesh_normals()`](https://gillescolling.com/ggplot3d/reference/mesh_processing.md);
  NULL keeps the input's normals.

- outline:

  Feature lines from
  [`outline_3d()`](https://gillescolling.com/ggplot3d/reference/outline_3d.md),
  or NULL for none.

- alpha_mode:

  How a translucent surface composites. `"face"` blends every face over
  what lies behind it, so a surface seen through itself darkens where it
  folds over. `"surface"` paints each sheet (a connected set of faces
  sharing vertices, such as one isosurface level) as one translucent
  film: other sheets show through it, its own pieces behind its nearer
  pieces do not. A sheet of several fill colours needs a device with
  Porter-Duff groups and alpha masks (ragg, cairo_pdf, svg); other
  devices, pdf() among them, paint it as `"face"`. The raster and hybrid
  backends keep, per sample, the nearest piece of each sheet and
  composite the sheets in depth order, with any fill colours; so does
  the
  [`view_3d()`](https://gillescolling.com/ggplot3d/reference/view_3d.md)
  viewer. The layers that draw surfaces
  ([`stat_isosurface_3d()`](https://gillescolling.com/ggplot3d/reference/stat_isosurface_3d.md),
  [`stat_parametric_3d()`](https://gillescolling.com/ggplot3d/reference/stat_parametric_3d.md),
  `geom_mesh_3d()` and others) accept it too.

- shading:

  `"flat"` lights and fills each face uniformly; `"smooth"` interpolates
  vertex normals and vertex fills across faces. The raster backend and
  the
  [`view_3d()`](https://gillescolling.com/ggplot3d/reference/view_3d.md)
  viewer interpolate per sample, perspective correctly. The vector
  backend draws a triangle whose colour varies along one direction as
  one linear gradient, with stops that follow the sRGB curve within the
  option `ggplot3d.shading_tolerance` (default 2 / 255 in encoded sRGB),
  and a triangle whose colour stays within that tolerance as one flat
  polygon. A run of faces that holds a triangle whose colour varies in
  two directions by more than the tolerance, which no gradient of R
  graphics can paint, is drawn as one image clipped to the faces'
  outline, in its place in the drawing order, at the device resolution
  and at least `options(ggplot3d.smooth_dpi)` (default 150) dots per
  inch. `options(ggplot3d.smooth_vector = "subdivide")` splits such
  triangles into flat overlapping pieces instead, at most four times
  (256 pieces per triangle) and not into pieces narrower than a device
  pixel, so large faces with distant colours show steps. The layers that
  draw surfaces
  ([`stat_isosurface_3d()`](https://gillescolling.com/ggplot3d/reference/stat_isosurface_3d.md),
  `geom_mesh_3d()` and others) accept it too.

- material:

  Surface material from
  [`material_3d()`](https://gillescolling.com/ggplot3d/reference/material_3d.md):
  ambient, diffuse and specular strengths, shininess, an optional
  back-side colour and an illustrative style (toon, Gooch, normal
  colours). NULL reflects ambient and diffuse light fully, with no
  highlight.

- hatch:

  Hatching from
  [`hatch_3d()`](https://gillescolling.com/ggplot3d/reference/hatch_3d.md),
  or NULL for none.

- texture:

  An image draped over the faces: a
  [`texture_3d()`](https://gillescolling.com/ggplot3d/reference/texture_3d.md)
  object or anything it accepts. Texture coordinates come from the `u`
  and `v` aesthetics when mapped, and otherwise from x and y, spread
  over the texture's extent (default the range of the layer).
  [`stat_parametric_3d()`](https://gillescolling.com/ggplot3d/reference/stat_parametric_3d.md)
  surfaces use their parameters and rgl meshes their `texcoords`, whose
  `texture` file is read when this is `NULL`. Each face shows the image
  over its fill; `fill = NA` shows the image alone. The layers that draw
  surfaces accept it too.

## Value

A ggplot2 layer.

## Details

Data accepts mesh_3d() output, rgl mesh3d objects and projected sf
POLYGON Z or TIN Z objects. With `shading = "flat"` a numeric vertex
fill is averaged per face before colour scaling, and every material must
be constant within each face. With `shading = "smooth"` the fill stays
per vertex, numeric or given as colours such as `fill = I(hex)`, and
each face interpolates its corner colours in linear light;
`shade = FALSE` shows the vertex colours exactly. A discrete fill splits
the rows into groups, so `group = 1` keeps one mesh.

The mesh parameters run the [mesh
processing](https://gillescolling.com/ggplot3d/reference/mesh_processing.md)
verbs on each mesh of a panel, in transformed position units, in the
order subdivide, remesh, decimate, smooth, curvature, crease.

## Examples

``` r
v <- data.frame(x = c(0, 1, 1, 0), y = c(0, 0, 1, 1), z = c(0, 0, 1, 1))
ggplot2::ggplot() + geom_mesh_3d(vertices = v, faces = matrix(1:4, 1)) + coord_3d()
cube <- expand.grid(x = 0:1, y = 0:1, z = 0:1)
quads <- rbind(c(1, 3, 4, 2), c(5, 6, 8, 7), c(1, 2, 6, 5), c(3, 7, 8, 4),
  c(1, 5, 7, 3), c(2, 4, 8, 6))
ggplot2::ggplot() +
  geom_mesh_3d(ggplot2::aes(fill = ggplot2::after_stat(mean_curvature)),
    vertices = cube, faces = quads, subdivide = 3, curvature = TRUE) +
  coord_3d()
```
