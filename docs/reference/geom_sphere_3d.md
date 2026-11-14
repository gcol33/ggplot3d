# Spheres

Draws one solid sphere per row, centred on x, y, z. Spheres are analytic
quadrics in the panel scene (Sigg et al. 2006): the vector backend draws
each one's exact outline, an ellipse under any projection, and the
raster backend intersects every sample's ray with it for exact depth and
normal. They intersect surfaces and other solids, take light from
[`light_3d()`](https://gillescolling.com/ggplot3d/reference/light_3d.md),
and shrink with distance under a perspective camera, unlike the flat
markers of
[`geom_point_3d()`](https://gillescolling.com/ggplot3d/reference/geom_point_3d.md).

## Usage

``` r
geom_sphere_3d(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  radius.unit = "data",
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

- radius.unit:

  Unit of the `radius` aesthetic: `"data"` or `"mm"`.

- detail:

  Subdivision level, from 0 to 6, of the icosphere of spheres that show
  their edges.

- shade:

  Whether to apply lighting.

## Value

A ggplot2 layer.

## Details

With `radius.unit = "data"` the `radius` aesthetic is measured in the
scale-transformed units of each position axis, as in Makie's
`meshscatter` and rgl's `spheres3d()`. A sphere is then round on screen
only when the box sides are proportional to the data ranges,
`coord_3d(aspect = "data")`; otherwise it is the ellipsoid those units
describe. Data-unit spheres expand the position scales to contain them.
With `radius.unit = "mm"` the radius is the on-screen radius of a sphere
at the centre of the view, so spheres stay round whatever the box
proportions. Rows without a radius get 2.5% of the longest box side.

Spheres with an edge `colour`, or drawn as wireframes, are icospheres
that show their edges: an icosahedron subdivided `detail` times, 20 \*
4^detail triangles. In curvilinear frames each sphere is placed at its
mapped centre and keeps its shape. The interactive viewer intersects
each pixel's ray with every analytic sphere as well; the exporters
receive an inscribed mesh of it (`options(ggplot3d.quadric_segments)`
points around the equator, 32 by default).

## References

Sigg, Weyrich, Botsch and Gross (2006). GPU-based ray-casting of
quadratic surfaces. Symposium on Point-Based Graphics.
[doi:10.2312/SPBG/SPBG06/059-065](https://doi.org/10.2312/SPBG/SPBG06/059-065)

## See also

[`geom_ellipsoid_3d()`](https://gillescolling.com/ggplot3d/reference/geom_ellipsoid_3d.md),
[`geom_glyph_3d()`](https://gillescolling.com/ggplot3d/reference/geom_glyph_3d.md).

## Examples

``` r
d <- data.frame(x = c(1, 2, 3, 2), y = c(1, 3, 1, 2), z = c(1, 1, 1, 2.5),
                r = c(0.4, 0.6, 0.5, 0.8), g = c("a", "b", "a", "b"))
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, radius = r, fill = g)) +
  geom_sphere_3d() + coord_3d(aspect = "data")
ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, z = qsec, fill = hp)) +
  geom_sphere_3d(radius = 2, radius.unit = "mm", detail = 1) + coord_3d()
```
