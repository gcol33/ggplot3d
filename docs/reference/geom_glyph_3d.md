# One solid glyph per observation

Places a copy of a small mesh at each x, y, z, as Makie's `meshscatter`
and PyVista's `glyph()` do. Built-in glyphs are `"sphere"`, `"cube"`,
`"cone"`, `"cylinder"` and `"octahedron"`, all centred on the origin and
spanning -1 to 1 along each axis; any
[`mesh_3d()`](https://gillescolling.com/ggplot3d/reference/mesh_3d.md)
or rgl `mesh3d` object can be used instead, with its origin at the
observation. The glyphs are solids in the panel scene: they intersect
other geometry exactly and take light.

## Usage

``` r
geom_glyph_3d(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  glyph = "sphere",
  size.unit = "mm",
  detail = 2L,
  shade = TRUE,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
```

## Arguments

- mapping, data, stat, position, ..., na.rm, show.legend, inherit.aes:

  Standard layer arguments.

- glyph:

  A built-in glyph name or a mesh from
  [`mesh_3d()`](https://gillescolling.com/ggplot3d/reference/mesh_3d.md)
  or
  [`read_mesh_3d()`](https://gillescolling.com/ggplot3d/reference/read_mesh_3d.md).

- size.unit:

  Unit of the `size` aesthetic: `"mm"` or `"data"`.

- detail:

  Tessellation level of the built-in round glyphs, from 0 to 6.

- shade:

  Whether to apply lighting.

## Value

A ggplot2 layer.

## Details

The `size` aesthetic is the width of a built-in glyph: a mesh is scaled
by `size / 2`. With `size.unit = "mm"` (the default) it is measured on
screen at the centre of the view, so a mapped `size` follows the usual
size scale; with `size.unit = "data"` it is measured in the
scale-transformed units of each position axis (use
[`ggplot2::scale_size_identity()`](https://ggplot2.tidyverse.org/reference/scale_identity.html)
to keep mapped values as they are) and the glyphs expand the position
scales.

The optional `u`, `v` and `w` aesthetics give a direction in data units;
the glyph's z axis, the apex of the cone, turns to follow it. Cones and
cylinders have `8 * 2^detail` sides; spheres are icospheres of
`20 * 4^detail` triangles.

## See also

[`geom_sphere_3d()`](https://gillescolling.com/ggplot3d/reference/geom_sphere_3d.md),
[`geom_ellipsoid_3d()`](https://gillescolling.com/ggplot3d/reference/geom_ellipsoid_3d.md).

## Examples

``` r
d <- expand.grid(x = 1:3, y = 1:3, z = 1)
d$u <- d$y - 2; d$v <- 2 - d$x; d$w <- 1
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, u = u, v = v, w = w)) +
  geom_glyph_3d(glyph = "cone", size = 6, fill = "tomato") + coord_3d()
ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, z = qsec, size = hp, fill = factor(cyl))) +
  geom_glyph_3d(glyph = "cube") + coord_3d()
```
