# Triangulated surfaces in three dimensions

Grid surfaces estimate vertex normals from height differences along each
axis. With `shading = "flat"` lighting uses the normalised mean of each
face's vertex normals and a numeric fill is averaged over each face.
With `shading = "smooth"` the vertex normals are interpolated across
each face (Phong 1975) and a fill that varies between vertices, numeric
or given as colours such as `fill = I(hex)`, is interpolated in linear
light (Gouraud 1971), as MATLAB's `lighting gouraud` with
`FaceColor = "interp"` and rgl's `smooth` material. A discrete fill
splits the rows into groups, so `group = 1` keeps one surface. Binned
fill and colour scales split faces at the scalar iso-lines, using linear
interpolation of the mapped variable between vertices. Scale
transformations place the breaks in the variable's original units. Each
piece has one band colour, with normals interpolated across its edges.

## Usage

``` r
geom_surface_3d(
  mapping = NULL,
  data = NULL,
  stat = StatSurface3D,
  position = "identity",
  ...,
  method = "grid",
  max_edge = Inf,
  shade = TRUE,
  wireframe = FALSE,
  hidden = "hide",
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

stat_surface_3d(
  mapping = NULL,
  data = NULL,
  geom = GeomSurface3D,
  position = "identity",
  ...,
  method = "grid",
  max_edge = Inf,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
```

## Arguments

- mapping, data, stat, position, ..., na.rm, show.legend, inherit.aes:

  Standard ggplot2 layer arguments.

- method:

  Grid connectivity or Delaunay triangulation.

- max_edge:

  Maximum triangle edge length in transformed x/y units.

- shade:

  Whether to apply lighting.

- wireframe:

  Whether to draw triangle edges.

- hidden:

  Hidden stroke policy.

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
  [`geom_mesh_3d()`](https://gillescolling.com/ggplot3d/reference/geom_mesh_3d.md)
  and others) accept it too.

- shading:

  `"flat"` lights and fills each face uniformly; `"smooth"` interpolates
  vertex normals and vertex fills across faces. The raster backend and
  the
  [`view_3d()`](https://gillescolling.com/ggplot3d/reference/view_3d.md)
  viewer interpolate per sample, perspective correctly. The vector
  backend draws an isolated triangle whose colour varies along one
  direction as one linear gradient, with stops that follow the sRGB
  curve within the option `ggplot3d.shading_tolerance` (default 2 / 255
  in encoded sRGB), and a triangle whose colour stays within that
  tolerance as one flat polygon. Adjacent smooth triangles and triangles
  whose colour varies in two directions are drawn as one image per
  drawing-order run, with per-sample lighting, clipped to the faces'
  union. The image uses sixteen samples per pixel, at the device
  resolution and at least `options(ggplot3d.smooth_dpi)` (default 150)
  dots per inch. `options(ggplot3d.smooth_vector = "subdivide")` splits
  such triangles into flat overlapping pieces instead, at most four
  times (256 pieces per triangle) and not into pieces narrower than a
  device pixel, so large faces with distant colours show steps. The
  layers that draw surfaces
  ([`stat_isosurface_3d()`](https://gillescolling.com/ggplot3d/reference/stat_isosurface_3d.md),
  [`geom_mesh_3d()`](https://gillescolling.com/ggplot3d/reference/geom_mesh_3d.md)
  and others) accept it too.

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

- geom:

  Geom prototype used to draw the computed triangles.

## Value

A ggplot2 layer.

## Examples

``` r
d <- expand.grid(x = seq(-1, 1, length.out = 6),
                 y = seq(-1, 1, length.out = 6))
d$z <- with(d, x^2 + y^2)
p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z))
p + geom_surface_3d(fill = "steelblue") + coord_3d()
p + stat_surface_3d() + coord_3d()
p + geom_surface_3d(ggplot2::aes(fill = z), shading = "smooth") + coord_3d()
stripes <- matrix(rep(c("navy", "gold"), each = 4), 1)
p + geom_surface_3d(texture = texture_3d(stripes, interpolate = FALSE)) + coord_3d()
```
