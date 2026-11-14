# Convert a 3D panel to an rgl mesh or rgl scene

`as.mesh3d()` returns the scene of one panel as one rgl `mesh3d` object
with vertex colours, ready for
[`rgl::shade3d()`](https://dmurdoch.github.io/rgl/dev/reference/shade3d.html),
[`rgl::writeOBJ()`](https://dmurdoch.github.io/rgl/dev/reference/writeOBJ.html)
or other mesh tools. `plot3d()` draws the panel in an rgl window with
rgl's own triangles, segments, points and text, viewed from the plot's
camera. Coordinates are model coordinates with the vertical axis of the
plot as +z; see
[`export_3d()`](https://gillescolling.com/ggplot3d/reference/export_3d.md).

## Usage

``` r
# S3 method for class 'ggplot3d'
as.mesh3d(
  x,
  panel = 1L,
  furniture = FALSE,
  lines = c("tube", "line"),
  points = c("mesh", "point"),
  size = 1,
  width = 640L,
  height = 640L,
  dpi = 96,
  ...
)

# S3 method for class 'ggplot3d'
plot3d(
  x,
  panel = 1L,
  furniture = FALSE,
  size = 1,
  width = 640L,
  height = 640L,
  dpi = 96,
  add = FALSE,
  ...
)
```

## Arguments

- x:

  A ggplot with
  [`coord_3d()`](https://gillescolling.com/ggplot3d/reference/coord_3d.md).

- panel, furniture, size, width, height, dpi:

  See
  [`export_3d()`](https://gillescolling.com/ggplot3d/reference/export_3d.md).

- lines, points:

  See
  [`export_3d()`](https://gillescolling.com/ggplot3d/reference/export_3d.md).
  `"line"` and `"point"` fill the mesh's segment (`is`) and point (`ip`)
  indices.

- ...:

  Unused.

- add:

  Whether to draw into the current rgl scene instead of a new one,
  default FALSE.

## Value

`as.mesh3d()`: a `mesh3d` object. `plot3d()`: the rgl object ids,
invisibly.

## Details

Faces drawn with lighting in the plot are drawn lit by rgl; all other
geometry is unlit. Line widths and point sizes are converted to pixels
at `dpi`. rgl centres the view on the bounding box of the drawn
geometry, so the view matches the plot's camera angles and projection
but can differ in framing.

## Examples

``` r
if (requireNamespace("rgl", quietly = TRUE)) {
  d <- expand.grid(x = seq(-1, 1, length.out = 10), y = seq(-1, 1, length.out = 10))
  d$z <- d$x * exp(-d$x^2 - d$y^2)
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
    geom_surface_3d(ggplot2::aes(fill = z)) + coord_3d()
  old <- options(rgl.useNULL = TRUE)
  rgl::plot3d(p)
  rgl::close3d()
  options(old)
  mesh <- rgl::as.mesh3d(p)
  dim(mesh$it)
}
```
